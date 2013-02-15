(*========================================================================*)
(*                                                                        *)
(*             cppmem model exploration tool                              *)
(*                                                                        *)
(*                    Mark Batty                                          *)
(*                    Scott Owens                                         *)
(*                    Jean Pichon                                         *)
(*                    Susmit Sarkar                                       *)
(*                    Peter Sewell                                        *)
(*                                                                        *)
(*  This file is copyright 2011, 2012 by the above authors.               *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*  3. The names of the authors may not be used to endorse or promote     *)
(*  products derived from this software without specific prior written    *)
(*  permission.                                                           *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS    *)
(*  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED     *)
(*  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE    *)
(*  ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY       *)
(*  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    *)
(*  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE     *)
(*  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS         *)
(*  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHE   *)
(*  IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR       *)
(*  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN   *)
(*  IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                         *)
(*========================================================================*)

open Auxl
open Printf
open Ids
open Types
open Constraints
open Pp
open Error

(* Munge around the instructions *)
let rec find_r r instrs = (* Find the first, if any exists *)
  match instrs with
  | [] -> None
  | DisplayEdge (r',_) as i :: instrs' when r = r' -> Some i
  | SuppressEdge (r',_) as i :: instrs' when r = r' -> Some i
  | _ :: instrs' -> find_r r instrs'

let rec is_mem a instrs = (* Find the first, if any exists *)
  match instrs with
  | [] -> false
  | DisplayNode (a') :: instrs' -> a = a'
  | SuppressNode (a') :: instrs' -> a = a'
  | _ :: instrs' -> is_mem a instrs'

let rec rem_shadowed instrs =
  match instrs with
  | [] -> []
  | (DisplayEdge (r,_)) as i :: instrs' ->
      begin
        match find_r r instrs' with
        | None -> i :: (rem_shadowed instrs')
        | Some _ -> rem_shadowed instrs'
      end
  | (SuppressEdge (r,_)) as i :: instrs' ->
      begin
        match find_r r instrs' with
        | None -> i :: (rem_shadowed instrs')
        | Some _ -> rem_shadowed instrs'
      end
  | (DisplayNode (a)) as i :: instrs' ->
      begin
        if is_mem a instrs' 
	then rem_shadowed instrs'
        else i :: (rem_shadowed instrs')
      end
  | (SuppressNode (a)) as i :: instrs' ->
      begin
        if is_mem a instrs' 
	then rem_shadowed instrs'
        else i :: rem_shadowed instrs'
      end


exception Instruction_read_error of string

exception Exc_read_error of string

let rec get_reln_instr r instrs = 
  match instrs with
  | [] -> None
  | Display_e (r',ri) as i :: instrs' ->
      if r = r' then Some i
      else get_reln_instr r instrs'
  | Suppress_e (r',ri) as i :: instrs' ->
      if r = r' then Some i
      else get_reln_instr r instrs'

let remove_and_get_last_reln_instr r instrs =
  let rec do_it acc instrs =
    match instrs with
    | [] -> acc, []
    | Display_e (r',Any) as i :: instrs' ->
        if r = r' then do_it (Some i) instrs'
        else let f,after = do_it acc instrs' in f,i::after
    | Display_e (r',Only rl) as i :: instrs' ->
        if r = r' 
        then 
          begin 
            match acc with
            | None 
            | Some (Suppress_e _) 
            | Some (Display_e (_,Any)) -> do_it (Some i) instrs'
            | Some (Display_e (_,Only rl')) -> do_it (Some (Display_e (r,Only (rl @ rl')))) instrs'
          end
        else let f,after = do_it acc instrs' in f,i::after
    | Suppress_e (r',Any) as i :: instrs' ->
        if r = r' then do_it (Some i) instrs'
        else let f,after = do_it acc instrs' in f,i::after
    | Suppress_e (r',Only rl) as i :: instrs' ->
        if r = r' 
        then 
          begin 
            match acc with
            | None 
            | Some (Display_e _) 
            | Some (Suppress_e (_,Any)) -> do_it (Some i) instrs'
            | Some (Suppress_e (_,Only rl')) -> do_it (Some (Suppress_e (r,Only (rl @ rl')))) instrs'
          end
        else let f,after = do_it acc instrs' in f,i::after
  in
  do_it None instrs


let read_instructions_buf lexbuf exwit prev =
  let raw = 
    (try
      Execparser.instructions Execlexer.mylexer lexbuf
    with
(*    | End_of_file -> {empty_raw_instructions with 
   raw_commands = [Quit] } *)
    | My_parse_error s ->
        raise (Instruction_read_error s))
  in
  let an_of_ts (start,trans) = start :: List.map snd trans in

  let all_actions = 
    remove_duplicates 
      ((option_map (function | Action a -> Some a | Action_name _ -> None) 
          (raw.raw_add_actions 
           @ (List.flatten (List.map an_of_ts raw.raw_add_rels))))
       @ prev.add_actions
       @ exwit.exod.actions)
  in
  let ids = remove_duplicates (List.map Cmm.aid_of all_actions) in
  let check_all_equal xs = match xs with 
  | [] -> true 
  | x::xs -> List.for_all 
        (function x' -> 
          if x=x' then true 
          else raise (Instruction_read_error (sprintf "two actions with the same id but different body: %a and %a" (pp_action ([],[])) x (pp_action ([],[])) x'))) xs in
  let _ = 
    List.for_all 
      (function aid -> check_all_equal (List.filter (function a -> Cmm.aid_of a = aid) all_actions)) ids in
  let lookup_data = List.map (function a -> (Cmm.aid_of a,a)) all_actions in 
  let lookup = function
    | Action a -> a
    | Action_name aid -> 
        try List.assoc aid lookup_data with Not_found -> 
          raise (Instruction_read_error (sprintf "action name used but not defined: %a" pp_action_id aid)) in
  let id_of = function
    | Action a -> Cmm.aid_of a
    | Action_name aid -> aid
  in
  let rec edges_of_ts (start,trans) = match trans with
  | [] -> []
  | (edge,an)::trans' -> (edge,[(lookup start,lookup an)])::edges_of_ts (an,trans') in
  let rec id_edges_of_ts (start,trans) = match trans with
  | [] -> []
  | (edge,an)::trans' -> (edge,[(id_of start,id_of an)])::id_edges_of_ts (an,trans') in
  let new_add_rels = remove_duplicates (List.flatten (List.map edges_of_ts raw.raw_add_rels)) in
  let new_remove_rels = remove_duplicates (List.flatten (List.map edges_of_ts raw.raw_remove_rels)) in
  let new_add_actions =
    List.map lookup raw.raw_add_actions in
  let new_remove_actions =
    List.map lookup raw.raw_remove_actions in
  let new_node_instructions =
    List.flatten 
      (List.map 
         (function
           | Raw_display_n ans -> List.map (fun an -> Display_n (id_of an)) ans
           | Raw_suppress_n ans -> List.map (fun an -> Suppress_n (id_of an)) ans)
         raw.raw_node_instructions) in
  let new_edge_instructions =
    List.flatten
      (List.map
         (function
           | Raw_display_e es ->
               List.map 
                 (function
                   | (e,None) -> Display_e (e,Any)
                   | (e,Some ts) -> Display_e (e,Only (List.flatten (List.map (fun (en,er) -> er) (id_edges_of_ts ts)))))
                 es
           | Raw_suppress_e es ->
               List.map 
                 (function
                   | (e,None) -> Suppress_e (e,Any)
                   | (e,Some ts) -> Suppress_e (e,Only (List.flatten (List.map (fun (en,er) -> er) (id_edges_of_ts ts)))))
                 es)
         raw.raw_edge_instructions) in
  let rec take_last_n = 
    function 
      | [] -> []
      | Display_n a :: ninstrs' -> 
          if (List.mem (Display_n a) ninstrs') || (List.mem (Suppress_n a) ninstrs')
          then take_last_n ninstrs'
          else (Display_n a) :: (take_last_n ninstrs') 
      | Suppress_n a :: ninstrs' -> 
          if (List.mem (Display_n a) ninstrs') || (List.mem (Suppress_n a) ninstrs')
          then take_last_n ninstrs'
          else (Suppress_n a) :: (take_last_n ninstrs') 
  in
  let rec take_last_e = 
    function 
      | [] -> []
      | Display_e (r,ri) as i :: ninstrs' -> 
          begin
            let last,removed = remove_and_get_last_reln_instr r (take_last_e ninstrs') in
            match last with
            | None -> i :: removed
            | Some ((Suppress_e _) as lasti) -> lasti :: removed
            | Some ((Display_e (r,Any)) as lasti) -> lasti :: removed
            | Some ((Display_e (r,Only rl)) as lasti) ->
                begin
                  match ri with
                  | Any -> lasti :: removed
                  | Only rl' -> (Display_e (r,Only (remove_duplicates (rl @ rl')))) :: removed
                end
          end
      | Suppress_e (r,ri) as i :: ninstrs' -> 
          begin
            let last,removed = remove_and_get_last_reln_instr r (take_last_e ninstrs') in
            match last with
            | None -> i :: removed
            | Some ((Display_e _) as lasti) -> lasti :: removed
            | Some ((Suppress_e (r,Any)) as lasti) -> lasti :: removed
            | Some ((Suppress_e (r,Only rl)) as lasti) ->
                begin
                  match ri with
                  | Any -> lasti :: removed
                  | Only rl' -> (Suppress_e (r,Only (remove_duplicates (rl @ rl')))) :: removed
                end
          end
  in
  let rec add_ignore_checks prev =
    function
      | [] -> prev
      | IgnoreCheck c :: raw_checks ->
          if List.mem c prev then add_ignore_checks prev raw_checks
          else add_ignore_checks (c :: prev) raw_checks
      | ConsiderCheck c :: raw_checks ->
          if List.mem c prev 
          then add_ignore_checks (List.filter (fun c' -> c <> c') prev) raw_checks
          else add_ignore_checks prev raw_checks
  in
  let rec add_constrain_rels prev =
    function
      | [] -> prev
      | IgnoreRel (r,pairs) :: raw_constrain_rels ->
          let npairs = List.map (fun (a1,a2) -> id_of a1,id_of a2) pairs in
          let prev_r = try List.assoc r prev with Not_found -> [] in
          let new_constrain_rels = 
            (r,List.filter (fun pair -> not (List.mem pair npairs)) prev_r) :: (List.remove_assoc r prev) in
          add_constrain_rels new_constrain_rels raw_constrain_rels 
      | ConsiderRel (r,pairs) :: raw_constrain_rels ->
          let npairs = List.map (fun (a1,a2) -> id_of a1,id_of a2) pairs in
          let prev_r = try List.assoc r prev with Not_found -> [] in
          let new_constrain_rels = 
            (r,remove_duplicates (npairs @ prev_r)) :: (List.remove_assoc r prev) in
          add_constrain_rels new_constrain_rels raw_constrain_rels 
  in
  {edge_instructions = take_last_e (prev.edge_instructions @ new_edge_instructions); 
   node_instructions = take_last_n (prev.node_instructions @ new_node_instructions); 
   ignorechecks = add_ignore_checks prev.ignorechecks raw.raw_checks;
   mode = apply_raw_ppmode_items prev.mode raw.raw_mode;
   add_actions = prev.add_actions @ new_add_actions;
   remove_actions = prev.remove_actions @ new_remove_actions;
   add_rels = prev.add_rels @ new_add_rels;
   remove_rels = prev.remove_rels @ new_remove_rels;
   constrain_rels = add_constrain_rels prev.constrain_rels raw.raw_constrain_rels; 
   commands = raw.raw_commands; (* Forget previous commands *)
   show = (match raw.raw_show with Some i -> Some i | None -> prev.show);
 }


let read_instructions_chan c name exwit prev =
  let lexbuf = Lexing.from_channel c in
  lexbuf.Lexing.lex_curr_p <- 
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = name};
  read_instructions_buf lexbuf exwit prev

let read_instructions_string s exwit prev =
  let lexbuf = Lexing.from_string s in
  read_instructions_buf lexbuf exwit prev

let read_instructions_file dsp_filename exwit prev = 
  let c = open_in dsp_filename in
  let ret = read_instructions_chan c dsp_filename exwit prev in
  let _ = close_in c in
  ret


(* Read it all in *)
let read_exec_buf (model, model_name) permissive lexbuf actionsopt = 
  let items = 
    (try
      Execparser.execfile Execlexer.mylexer lexbuf
    with 
      My_parse_error s ->
        raise (Exc_read_error s)) in
  
  let rec pp_trans = function
    | [] -> ""
    | (edge,an)::trans' -> sprintf " --%s--> %a" edge (pp_action_or_name ([],[])) an ^ pp_trans trans' in

  let pp_transition_sequence () (start,trans) = 
    pp_action_or_name ([],[]) () start  ^ pp_trans trans in

(*  print_string (String.concat "" (List.map (function ts -> pp_transition_sequence () ts^"\n") items)); 
   ris
 *)
  let an_of_ts (start,trans) = start :: List.map snd trans in
  let models,transs,lks,oinsts,ignorechecks = 
    List.fold_right 
      (fun item (ms,ts,lks,oinsts,ics)->
	match item with
        | Model m -> m :: ms,ts,lks,oinsts,ics
	| Trans t -> ms,t :: ts,lks,oinsts,ics
        | Lk lk -> ms,ts,lk@lks,oinsts,ics
	| DisplayEdgeInstr o -> ms,ts,lks,item :: oinsts,ics
        | SuppressEdgeInstr o -> ms,ts,lks,item :: oinsts,ics
	| DisplayNodeInstr os -> ms,ts,lks,item :: oinsts,ics
        | SuppressNodeInstr os -> ms,ts,lks,item :: oinsts,ics
	| Ignore cs -> ms,ts,lks,oinsts,cs @ ics
        | Show nopt -> 
            let () = Globals.show := nopt (* Imperative nastiness, more principled to pass it around, but bah.. *)
            in ms,ts,lks,oinsts,ics)
      items ([],[],[],[],[]) in
  (match models with
    | [] ->
      if model_name = Atomic.default_model_name then ()
      else raise (Failure ("(implicitly) " ^ Atomic.default_model_name ^ " model execution used with " ^ model_name ^ " model"))
    | [m] ->
      if m = model_name then ()
      else
      (if List.mem_assoc m Atomic.the_models then raise (Failure ("execution for model " ^ m ^ " used with model " ^ model_name))
       else raise (Failure ("unknown model ``" ^ m ^ "''")))
    | _::_ -> raise (Failure "several models"));
  let an_of_input = List.flatten (List.map an_of_ts transs) in
  let actions = remove_duplicates ((option_map (function | Action a -> Some a | Action_name aid -> None) an_of_input) @ (match actionsopt with | None -> [] | Some acts -> acts)) in
  let ids = remove_duplicates (List.map Cmm.aid_of actions) in
  let check_all_equal xs = match xs with 
  | [] -> true 
  | x::xs -> List.for_all 
        (function x' -> 
          if x=x' then true 
          else raise (Failure (sprintf "two actions with the same id but different body: %a and %a" (pp_action ([],[])) x (pp_action ([],[])) x'))) xs in
  let _ = List.for_all (function aid -> check_all_equal (List.filter (function a -> Cmm.aid_of a = aid) actions)) ids in
  let lookup_data = List.map (function a -> (Cmm.aid_of a,a)) actions in 
  let lookup = function
    | Action a -> Some a
    | Action_name aid -> 
        try Some (List.assoc aid lookup_data) with Not_found -> 
          if permissive then None 
          else raise (Failure (sprintf "action name used but not defined: %a" pp_action_id aid)) in
  let lk = List.map (function (l,lk) -> (l,lk)) lks in
  let rec edges_of_ts (start,trans) = match trans with
  | [] -> []
  | (edge,an)::trans' -> (lookup start,edge,lookup an)::edges_of_ts (an,trans') in
  let edges = remove_duplicates (List.flatten (List.map (function ts -> edges_of_ts ts) transs)) in
  let threads = remove_duplicates (List.map Cmm.tid_of actions) in
(*   let edges_of_thread tid et = option_map (function (a1,edge,a2) -> if thread_id_of a1=tid && edge=et && thread_id_of a2 = tid then Some (a1,a2) else None) edges in *)
  let edges_of et = 
    option_map 
      (function 
          (Some a1,edge,Some a2) -> if edge=et then Some (a1,a2) else None
        | _ -> None) edges in
  let lookups set = option_map lookup set in
  let transform_nodespec r = 
    match r with
    | All -> None
    | Actions l -> Some (lookups l) in
  let oinstrs =
    List.map 
      (function 
        | DisplayEdgeInstr insts -> 
            List.map 
              (function
                | (edge,RawCrossProduct(froms,tos)) ->
                    DisplayEdge (edge,CrossProduct(transform_nodespec froms,transform_nodespec tos))
                | (edge,RawExact(rel)) ->
                    DisplayEdge 
                      (edge,
                       Exact(option_map 
                               (fun (a1,a2) -> 
                                 match (lookup a1,lookup a2) with
                                 | Some a1,Some a2 -> Some (a1,a2)
                                 | _ -> None) 
                               rel)))
              insts
        | SuppressEdgeInstr insts -> 
            List.map 
              (function
                |  (edge,RawCrossProduct(froms,tos)) ->
                    SuppressEdge (edge,CrossProduct(transform_nodespec froms,transform_nodespec tos))
                | (edge,RawExact(rel)) ->
                    SuppressEdge 
                      (edge,
                       Exact(option_map 
                               (fun (a1,a2) -> 
                                 match (lookup a1,lookup a2) with
                                 | Some a1,Some a2 -> Some (a1,a2)
                                 | _ -> None) 
                               rel)))
              insts
	| DisplayNodeInstr insts -> 
            option_map 
              (fun (aid) ->
                match lookup aid with
                | Some a -> Some (DisplayNode a)
                | None -> None)
              insts
	| SuppressNodeInstr insts ->
	    option_map 
              (fun (aid) -> 
                match lookup aid with
                | Some a -> Some (SuppressNode a)
                | None -> None)
              insts
        | Trans _ -> internal_error "transition parsed as instruction"
        | Show _ -> internal_error "show num parsed as instruction"
	| Ignore _ -> internal_error "ignore checks parsed as instruction")
      oinsts
  in
  let oinstrs = rem_shadowed (List.flatten oinstrs) in
    let exod = {
      actions =  actions;
      threads = threads;
      lk = lk ;
      sb = transitive_closure (edges_of "sb");
      asw =                   (edges_of "asw");
      dd = transitive_closure (edges_of "dd");
      cd = transitive_closure (edges_of "cd");
      vconstraint = ctrue } in
    let exed = {
      rf = edges_of "rf" ;
      sc = transitive_closure (edges_of "sc") ;
      mo = transitive_closure (edges_of "mo");
      (* TODO: jp: should these relations be transitive-expanded? (and transitive-reduced when written, see pp.ml) *)
      lo = edges_of "lo";
      ao = edges_of "ao";
      tot = edges_of "tot";
    } in
    let exedo = 
      if exed = empty_execution_existential_data model then None else Some exed in
    let exdd = {
      locations =  [] (* TODO *);
      derived_relations = List.map (fun nm -> (nm, edges_of nm)) (Iso.names_of_derived_relations model);
      undefined_behaviour = List.map (fun nm -> (nm, Two (edges_of nm))) (Iso.names_of_undefined_behaviour_relations model);
    } in
    let exddo = 
      if exdd = empty_execution_derived_data then None else Some exdd in
    ((exod,exedo,exddo),oinstrs,ignorechecks)


let read_exec_lexbuf model permissive lexbuf name actionsopt =
  lexbuf.Lexing.lex_curr_p <- 
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = name};
  read_exec_buf model permissive lexbuf actionsopt

let read_exec_chan model permissive c name actionsopt =
  let lexbuf = Lexing.from_channel c in
  read_exec_lexbuf model permissive lexbuf name actionsopt

let read_exec_string model permissive s name actionsopt =
  let lexbuf = Lexing.from_string s in
  read_exec_lexbuf model permissive lexbuf name actionsopt

let read_exec_file model permissive exc_filename actionsopt = 
  let c = open_in exc_filename in
  let ret = read_exec_chan model permissive c exc_filename actionsopt in
  let _ = close_in c in
  ret





(* Process the instructions *)

let rec is_not_suppressed_under_relabelling rl instrs a =
  match instrs with
  | [] -> true
  | SuppressNode a' :: instrs' -> 
      if (pp_action rl () a) = (pp_action rl () a')
      then false 
      else is_not_suppressed_under_relabelling rl instrs' a
  | _ :: instrs' -> is_not_suppressed_under_relabelling rl instrs' a

let filter_reln rl instrs relname before =
  let filtered_before =
    List.filter
      (fun (a1,a2) ->
	(is_not_suppressed_under_relabelling rl instrs a1) &&
	(is_not_suppressed_under_relabelling rl instrs a2))
      before 
  in
  match find_r relname instrs with
  | None -> filtered_before (* Default to keep relations unmentioned *)
  | Some (DisplayEdge (relname,CrossProduct(froms,tos))) -> 
      let from_action a =
        match froms with
        | None -> true (* all actions *)
        | Some flist ->
            List.exists 
              (fun a_rl -> 
                (pp_action rl () a) = (pp_action rl () a_rl))
              flist
      in
      let to_action a =
        match tos with
        | None -> true (* all actions *)
        | Some flist ->
            List.exists
              (fun a_rl -> 
                (pp_action rl () a) = (pp_action rl () a_rl))
              flist
      in
      List.filter (fun (a1,a2) -> from_action a1 && to_action a2) filtered_before
  | Some (DisplayEdge (relname,Exact (rels))) ->
      List.filter 
        (fun (a1,a2) -> 
          List.exists
            (fun (a1',a2') ->
              (pp_action rl () a1') = (pp_action rl () a1) &&
              (pp_action rl () a2') = (pp_action rl () a2))
            rels)
        filtered_before
  | Some (SuppressEdge (relname,CrossProduct(froms,tos))) -> 
      let from_action a =
        match froms with
        | None -> false (* all actions *)
        | Some flist ->
            not (List.exists 
                   (fun a_rl -> 
                     (pp_action rl () a) = (pp_action rl () a_rl))
                   flist)
      in
      let to_action a =
        match tos with
        | None -> false (* all actions *)
        | Some flist ->
            not (List.exists
                   (fun a_rl -> 
                     (pp_action rl () a) = (pp_action rl () a_rl))
                   flist)
      in
      List.filter (fun (a1,a2) -> from_action a1 && to_action a2) filtered_before
  | Some (SuppressEdge (relname,Exact (rels))) ->
      List.filter 
        (fun (a1,a2) -> 
          List.for_all
            (fun (a1',a2') ->
              (pp_action rl () a1') <> (pp_action rl () a1) ||
              (pp_action rl () a2') <> (pp_action rl () a2))
            rels)
        filtered_before
  | Some _ -> internal_error "node returned as a relation output instruction"

let filter_exod rl exod output_instrs = 
  {exod with
   actions = List.filter (fun a -> is_not_suppressed_under_relabelling rl output_instrs a) exod.actions;
   sb = filter_reln rl output_instrs "sb" exod.sb;
   asw = filter_reln rl output_instrs "asw" exod.asw;
   dd = filter_reln rl output_instrs "dd" exod.dd;
   cd = filter_reln rl output_instrs "cd" exod.cd;
 }
    

let filter_exed rl exed output_instrs = 
  {rf = filter_reln rl output_instrs "rf" exed.rf;
   sc = filter_reln rl output_instrs "sc" exed.sc;
   mo = filter_reln rl output_instrs "mo" exed.mo;
   lo = filter_reln rl output_instrs "lo" exed.lo;
   ao = filter_reln rl output_instrs "ao" exed.ao;
   tot = filter_reln rl output_instrs "tot" exed.tot;
 }
    
let filter_exdd rl exdd output_instrs = 
  { exdd with
    derived_relations =
      List.map
        (fun (nm, rel) ->
          (nm, filter_reln rl output_instrs nm rel))
        exdd.derived_relations;
    undefined_behaviour =
      List.map
        (fun (nm, fault) ->
          match fault with
            | One acts -> (nm, One acts) (* Ignore instrs for now *)
            | Two rel -> (nm, Two (filter_reln rl output_instrs nm rel)))
        exdd.undefined_behaviour;
  }



(* Process the instructions *)

let rec suppressed instrs a =
  match instrs with
  | [] -> false
  | Suppress_n a' :: instrs' -> 
      if Cmm.aid_of a = a'
      then true 
      else suppressed instrs' a
  | _ :: instrs' -> suppressed instrs' a

let filter_out_reln instrs relname before =
  let filtered_before =
    List.filter
      (fun (a1,a2) ->
	not 
          ((suppressed instrs.node_instructions a1) ||
	  (suppressed instrs.node_instructions a2)))
      before 
  in
  match get_reln_instr relname instrs.edge_instructions with
  | None -> filtered_before (* Default to keep relations unmentioned *)
  | Some (Display_e (relname,Any)) -> filtered_before
  | Some (Suppress_e (relname,Any)) -> []
  | Some (Display_e (relname,Only (rels))) ->
      List.filter 
        (fun (a1,a2) -> 
          List.exists
            (fun (a1',a2') ->
              a1' = Cmm.aid_of a1 &&
              a2' = Cmm.aid_of a2)
            rels)
        filtered_before
  | Some (Suppress_e (relname,Only (rels))) ->
      List.filter 
        (fun (a1,a2) -> 
          List.for_all
            (fun (a1',a2') ->
              a1' <> Cmm.aid_of a1 ||
              a2' <> Cmm.aid_of a2)
            rels)
        filtered_before

let filter_out_exod exod instrs = 
  {exod with
   actions = List.filter (fun a -> not (suppressed instrs.node_instructions a)) exod.actions;
   sb = filter_out_reln instrs "sb" exod.sb;
   asw = filter_out_reln instrs "asw" exod.asw;
   dd = filter_out_reln instrs "dd" exod.dd;
   cd = filter_out_reln instrs "cd" exod.cd;
 }
    

let filter_out_exed exed instrs = 
  {rf = filter_out_reln instrs "rf" exed.rf;
   sc = filter_out_reln instrs "sc" exed.sc;
   mo = filter_out_reln instrs "mo" exed.mo;
   lo = filter_out_reln instrs "lo" exed.lo;
   ao = filter_out_reln instrs "ao" exed.ao;
   tot = filter_out_reln instrs "tot" exed.tot;
 }
    
let filter_out_exdd exdd instrs = 
  {exdd with
   derived_relations = List.map (fun (nm, rel) ->
     (nm, filter_out_reln instrs nm rel)) exdd.derived_relations;
   undefined_behaviour = List.map (fun (nm, fault) -> match fault with
     | One acts -> (nm, One acts) (* Ignore instrs for now *)
     | Two rel -> (nm, Two (filter_out_reln instrs nm rel))) exdd.undefined_behaviour;
 }

let filter_out_exwit exwit instrs = {
  exod = filter_out_exod exwit.exod instrs;
  exed = filter_out_exed exwit.exed instrs;
  exdd = filter_out_exdd exwit.exdd instrs;
}

let filter_out_exwit_wrap exwit instrs =
  let filtered_exwit = filter_out_exwit exwit instrs in
  (filtered_exwit.exod,Some filtered_exwit.exed,Some filtered_exwit.exdd)

let matches_exwit exwit constrain_rels =
  let rel_matches rname rel =
    let rconstr = try List.assoc rname constrain_rels with Not_found -> [] in
    List.for_all 
      (fun (aid1,aid2) -> 
	List.exists 
	  (fun (a1,a2) ->
	    Cmm.aid_of a1 = aid1 && 
	    Cmm.aid_of a2 = aid2)
	  rel) 
      rconstr in
  let exod_matches =
    rel_matches "sb" exwit.exod.sb &&
    rel_matches "asw" exwit.exod.asw &&
    rel_matches "dd" exwit.exod.dd &&
    rel_matches "cd" exwit.exod.cd
  in
  let exed_matches =
    rel_matches "rf" exwit.exed.rf &&
    rel_matches "sc" exwit.exed.sc &&
    rel_matches "mo" exwit.exed.mo 
  in
  let exdd_matches =
    List.for_all (fun (nm, rel) -> rel_matches nm rel) exwit.exdd.derived_relations &&
    List.for_all (fun (nm, fault) -> match fault with
      | One _ -> true
      | Two rel -> rel_matches (List.assoc nm Atomic.short_names) rel) exwit.exdd.undefined_behaviour
  in
  exod_matches && exed_matches && exdd_matches
    
