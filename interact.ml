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
open Value
open Types
open Iso
open Execfile
open Executions
open Globals

exception UserQuit

let interact_user_w_raw_candidate sofar cand m =
  let heremsg () = Printf.printf "Found raw candidate no: %d\n" sofar.raw_candidates_count in
  let () = if !quietmode then () else heremsg () in
  let rec loop () = 
    let () = 
      if !quietmode then heremsg () else ();
      Printf.printf "\nQuit [q]/Continue [c]/Show next real execution [e]/Show next candidate [C]/Never stop again [n]/Always stop from now on [a]\n";
      Printf.printf "Or, Show raw candidate [r]/Generate dot file[d]: "; 
      flush (stdout) in
    let proc_c c = 
      match c with 
      | 'q' -> raise UserQuit
      | 'c' -> ()
      | 'e' -> Globals.stop_at := OnSolutions; ()
      | 'C' -> Globals.stop_at := OnCandidates; ()
      | 'n' -> Globals.stop_at := Never; ()
      | 'a' -> Globals.stop_at := Always; ()
      | 'd' -> 
          let () = 
            Printf.printf "Please give dot filename <out.dot>: ";
            flush(stdout) in
          let dotfile = 
            let s = read_line () in if s="" then "out.dot" else s in
          let exod = conv_execution_result cand in
          let str = Printf.sprintf "%a" (Pp.pp_dot) (m,Some dotfile,(exod,None,None)) in
          let fd = open_out dotfile in
          let () = output_string fd str in
          let () = close_out fd in
          ()
      | 'r' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result cand) in
          loop () 
      | _ -> loop () in
    Scanf.scanf "%1c@\n" proc_c in
  match !Globals.stop_at with
  | OnSolutions
  | OnCandidates
  | Never -> ()
  | Always -> 
      loop ()

let interact_user_w_preliminary_soln sofar raw_cand sub sub_cand count_rf_poss m =
  let heremsg () =
    Printf.printf "Found preliminary solution: %s\n" (pp_substitution () sub);
    Printf.printf "Number of RFmap possibilities = %d\n" count_rf_poss in
  let () = if !quietmode then () else heremsg () in
  let rec loop () = 
    let () = 
      if !quietmode then heremsg () else ();
      Printf.printf "\nQuit [q]/Continue [c]/Show next real execution [e]/Show next candidate [C]/Never stop again [n]/Always stop from now on [a]\n";
      Printf.printf "Or, Show raw candidate [r]/Show solved candidate [s]/Generate dot file[d]: "; 
      flush (stdout) in
    let proc_c c = 
      match c with 
      | 'q' -> raise UserQuit
      | 'c' -> ()
      | 'e' -> Globals.stop_at := OnSolutions; ()
      | 'C' -> Globals.stop_at := OnCandidates; ()
      | 'n' -> Globals.stop_at := Never; ()
      | 'a' -> Globals.stop_at := Always; ()
      | 'r' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result raw_cand) in
          loop () 
      | 's' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result sub_cand) in
          loop () 
      | 'd' -> 
          let () = 
            Printf.printf "Please give dot filename <out.dot>: ";
            flush(stdout) in
          let dotfile = 
            let s = read_line () in if s="" then "out.dot" else s in
          let exod = conv_execution_result sub_cand in
          let str = Printf.sprintf "%a" (Pp.pp_dot) (m,Some dotfile,(exod,None,None)) in
          let fd = open_out dotfile in
          let () = output_string fd str in
          let () = close_out fd in
          ()
      | _ -> loop () in
    Scanf.scanf "%1c@\n" proc_c in
  match !Globals.stop_at with
  | OnSolutions
  | OnCandidates
  | Never -> ()
  | Always -> 
      loop ()

let interact_user_w_rfmap model sofar raw_cand sub_cand rfmap eqs m =
  let heremsg () = 
    Printf.printf "RF-map %d of %d\n" sofar.current_rf_of_candidate sofar.total_rf_of_candidate;
    Printf.printf "RF-map = %s\n" (pp_action_reln "rf" () rfmap) in
  let () = if !quietmode then () else heremsg () in
  let rec loop () = 
    let () = 
      if !quietmode then heremsg () else ();
      Printf.printf "\nQuit [q]/Continue [c]/Show next real execution [e]/Show next candidate [C]/Never stop again [n]/Always stop from now on [a]\n";
      Printf.printf "Or, Show raw candidate [r]/Show solved candidate [s]/Show rf-map and generated equations[m]/Generate dot file[d]: "; 
      flush (stdout) in
    let proc_c c = 
      match c with 
      | 'q' -> raise UserQuit
      | 'c' -> ()
      | 'e' -> Globals.stop_at := OnSolutions; ()
      | 'C' -> Globals.stop_at := OnCandidates; ()
      | 'n' -> Globals.stop_at := Never; ()
      | 'a' -> Globals.stop_at := Always; ()
      | 'r' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result raw_cand) in
          loop () 
      | 's' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result sub_cand) in
          loop () 
      | 'm' ->
          let () = Printf.printf "RF-map = %s\n" (pp_action_reln "rf" () rfmap) in
          let () = Printf.printf "Equations = %s\n" (String.concat " /\\ " (List.map (fun (v1,v2) -> Printf.sprintf "%a == %a" pp_value v1  pp_value v2) eqs)) in
          loop ()
      | 'd' -> 
          let () = 
            Printf.printf "Please give dot filename <out.dot>: ";
            flush(stdout) in
          let dotfile = 
            let s = read_line () in if s="" then "out.dot" else s in
          let exod = conv_execution_result sub_cand in
          let exed = {(empty_execution_existential_data model) with rf = rfmap} in 
          let str = Printf.sprintf "%a" (Pp.pp_dot) (m,Some dotfile,(exod,Some exed,None)) in
          let fd = open_out dotfile in
          let () = output_string fd str in
          let () = close_out fd in
          ()
      | _ -> loop () in
    Scanf.scanf "%1c@\n" proc_c in
  match !Globals.stop_at with
  | OnSolutions
  | OnCandidates
  | Never -> ()
  | Always -> 
      loop ()

let interact_user_w_rfmap_soln model sofar raw_cand sub_rfm sub_cand rfmap count_mo_poss m =
  let heremsg () = 
    Printf.printf "Found a solution to RF-map %d of %d: %s\n" sofar.current_rf_of_candidate sofar.total_rf_of_candidate (pp_substitution () sub_rfm);
    Printf.printf "Number of modification-order possibilities = %d\n" count_mo_poss in
  let () = if !quietmode then () else heremsg () in
  let rec loop () = 
    let () = 
      if !quietmode then heremsg () else ();
      Printf.printf "\nQuit [q]/Continue [c]/Show next real execution [e]/Show next candidate [C]/Never stop again [n]/Always stop from now on [a]\n";
      Printf.printf "Or, Show raw candidate [r]/Show solved candidate [s]/Generate dot file[d]: "; 
      flush (stdout) in
    let proc_c c = 
      match c with 
      | 'q' -> raise UserQuit
      | 'c' -> ()
      | 'e' -> Globals.stop_at := OnSolutions; ()
      | 'C' -> Globals.stop_at := OnCandidates; ()
      | 'n' -> Globals.stop_at := Never; ()
      | 'a' -> Globals.stop_at := Always; ()
      | 'r' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result raw_cand) in
          loop () 
      | 's' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result sub_cand) in
          let () = Printf.printf "RF-map = %s\n" (pp_action_reln "rf" () rfmap) in
          loop () 
      | 'd' -> 
          let () = 
            Printf.printf "Please give dot filename <out.dot>: ";
            flush(stdout) in
          let dotfile = 
            let s = read_line () in if s="" then "out.dot" else s in
          let exod = conv_execution_result sub_cand in
          let exed = {(empty_execution_existential_data model) with rf = rfmap} in 
          let str = Printf.sprintf "%a" (Pp.pp_dot) (m,Some dotfile,(exod,Some exed,None)) in
          let fd = open_out dotfile in
          let () = output_string fd str in
          let () = close_out fd in
          ()
      | _ -> loop () in
    Scanf.scanf "%1c@\n" proc_c in
  match !Globals.stop_at with
  | OnSolutions
  | OnCandidates
  | Never -> ()
  | Always -> 
      loop ()

let interact_user_w_mo sofar raw_cand sub_cand rfmap mo_rel count_sc_poss =
  let heremsg () = 
    Printf.printf "Found memory order %d of %d: %s\n" sofar.current_mo_of_candidate sofar.total_mo_of_candidate (pp_action_reln "mo" () mo_rel);
    Printf.printf "Number of SC-order possibilities = %d\n" count_sc_poss in  
  let () = if !quietmode then () else heremsg () in
  let rec loop () = 
    let () = 
      if !quietmode then heremsg () else ();
      Printf.printf "\nQuit [q]/Continue [c]/Show next real execution [e]/Show next candidate [C]/Never stop again [n]/Always stop from now on [a]\n";
      Printf.printf "Or, Show raw candidate [r]/Show solved candidate [s]: "; 
      flush (stdout) in
    let proc_c c = 
      match c with 
      | 'q' -> raise UserQuit
      | 'c' -> ()
      | 'e' -> Globals.stop_at := OnSolutions; ()
      | 'C' -> Globals.stop_at := OnCandidates; ()
      | 'n' -> Globals.stop_at := Never; ()
      | 'a' -> Globals.stop_at := Always; ()
      | 'r' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result raw_cand) in
          loop () 
      | 's' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result sub_cand) in
          let () = Printf.printf "RF-map = %s\n" (pp_action_reln "rf" () rfmap) in
          let () = Printf.printf "Modification-order = %s\n" (pp_action_reln "mo" () mo_rel) in
          loop () 
      | _ -> loop () in
    Scanf.scanf "%1c@\n" proc_c in
  match !Globals.stop_at with
  | OnSolutions
  | OnCandidates
  | Never -> ()
  | Always -> 
      loop ()

let interact_user_w_sc sofar raw_cand sub_cand rfmap mo_rel sc_rel =
  let heremsg () = Printf.printf "Found SC-order %d of %d: %s\n" sofar.current_sc_of_candidate sofar.total_sc_of_candidate (pp_action_reln "sc" () sc_rel) in
  let () = if !quietmode then () else heremsg () in
  let rec loop () = 
    let () = 
      if !quietmode then heremsg () else ();
      Printf.printf "\nQuit [q]/Continue [c]/Show next real execution [e]/Show next candidate [C]/Never stop again [n]/Always stop from now on [a]\n";
      Printf.printf "Or, Show raw candidate [r]/Show solved candidate [s]: "; 
      flush (stdout) in
    let proc_c c = 
      match c with 
      | 'q' -> raise UserQuit
      | 'c' -> ()
      | 'e' -> Globals.stop_at := OnSolutions; ()
      | 'C' -> Globals.stop_at := OnCandidates; ()
      | 'n' -> Globals.stop_at := Never; ()
      | 'a' -> Globals.stop_at := Always; ()
      | 'r' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result raw_cand) in
          loop () 
      | 's' -> 
          let () = 
            print_string (Printf.sprintf "**********************************************************\n%a*********************************************************\n" pp_execution_result sub_cand) in
          let () = Printf.printf "RF-map = %s\n" (pp_action_reln "rf" () rfmap) in
          let () = Printf.printf "Modification-order = %s\n" (pp_action_reln "mo" () mo_rel) in
          let () = Printf.printf "SC-order = %s\n" (pp_action_reln "sc" () sc_rel) in
          loop () 
      | _ -> loop () in
    Scanf.scanf "%1c@\n" proc_c in
  match !Globals.stop_at with
  | OnSolutions
  | OnCandidates
  | Never -> ()
  | Always -> 
      loop ()



(* Subset under the pp transform... yuck... *)

let pp_thread_ids_subset rl t_set1 t_set2 =
  let pp_t_set1 = List.map (Pp.pp_thread_id' rl ()) t_set1 in
  let pp_t_set2 = List.map (Pp.pp_thread_id' rl ()) t_set2 in
  list_subset pp_t_set1 pp_t_set2

let pp_actions_subset rl a_set1 a_set2 =
  let pp_a_set1 = List.map (Pp.pp_action rl ()) a_set1 in
  let pp_a_set2 = List.map (Pp.pp_action rl ()) a_set2 in
  list_subset pp_a_set1 pp_a_set2

let pp_action_rel_subset rl ar1 ar2 =
  let pp_ar1 = List.map (fun (a1,a2) -> (Pp.pp_action rl () a1,Pp.pp_action rl () a2)) ar1 in
  let pp_ar2 = List.map (fun (a1,a2) -> (Pp.pp_action rl () a1,Pp.pp_action rl () a2)) ar2 in
  list_subset pp_ar1 pp_ar2

let pp_exod_subset rl exod1 exod2 =
  pp_actions_subset rl exod1.actions exod2.actions &&
  pp_thread_ids_subset rl exod1.threads exod2.threads &&
(* Ignore lk's: TODO *)
(*  pp_action_rel_subset rl exod1.lk exod2.lk && *)
  pp_action_rel_subset rl exod1.sb exod2.sb &&
  pp_action_rel_subset rl exod1.asw exod2.asw &&
  pp_action_rel_subset rl exod1.dd exod2.dd &&
  pp_action_rel_subset rl exod1.cd exod2.cd

let pp_exed_subset rl exed1 exed2 =
  pp_action_rel_subset rl exed1.rf exed2.rf &&
  pp_action_rel_subset rl exed1.sc exed2.sc &&
  pp_action_rel_subset rl exed1.mo exed2.mo

let pp_exdd_subset rl exdd1 exdd2 =
  (* TODO: jp: old code said: *)
  (* Ignore locations: TODO *)
  (*  pp_action_rel_subset rl exdd1.locations exdd2.locations && *)
  (* Ignore vsses: TODO *)
  (*  pp_action_rel_subset rl exdd1.vsses exdd2.vsses && *)
  (* Ignore ir: TODO *)
  (*  pp_action_rel_subset rl exdd1.ir exdd2.ir *)
  (* TODO: jp: what does it mean? *)
  let derived_rels1 = my_sort exdd1.derived_relations in
  let derived_rels2 = my_sort exdd2.derived_relations in
  let undef_beh1 = my_sort exdd1.undefined_behaviour in
  let undef_beh2 = my_sort exdd2.undefined_behaviour in
  List.length derived_rels1 = List.length derived_rels2 &&
  List.length undef_beh1 = List.length undef_beh2 &&
  List.for_all2 (fun (nm1, rel1) (nm2, rel2) ->
    nm1 = nm2 &&
    pp_action_rel_subset rl rel1 rel2)
    derived_rels1 derived_rels2 &&
  List.for_all2 (fun (nm1, fault1) (nm2, fault2) ->
    nm1 = nm2 &&
    (match fault1, fault2 with
      | Two rel1, Two rel2 -> pp_action_rel_subset rl rel1 rel2
      | _, _ -> (* TODO: jp: why did the old code behave like this?*) true))
    undef_beh1 undef_beh2



exception Loop

let helptext model m () =
  let bb = "\x1B[1m" in
  let norm = "\x1B[0m" in
  begin
    Printf.printf "-------------------------------------------------------------------------------\n";
    Printf.printf "Graphs are produced in the file %s.pdf\n\n" !Globals.tmpfilename;
    Printf.printf "Allowed commands (end each with a period '.' if giving more than one):\n";
    Printf.printf "%shelp%s : this list\n" bb norm;
    Printf.printf "%squit%s : immediate quit\n" bb norm;
    Printf.printf "%snext%s : go to next (consistent execution, or any candidate, as below)\n" bb norm;
    Printf.printf "%snon_stop%s : never stop to interact again\n" bb norm;
    Printf.printf "%snext_candidate%s : set %snext%s to stop at the next candidate (valid or not)\n" bb norm bb norm;
    Printf.printf "%snext_consistent%s : set %snext%s to stop at the next consistent candidate\n" bb norm bb norm;
    Printf.printf "%sshow%s (<number> | %sall%s): show particular/all execution (takes effect on next)\n" bb norm bb norm;
    Printf.printf "%sgenerate%s <filetype> <filename> : generate filetype {%sdot%s, %sisa%s, %sexc%s, %sdsp%s, %stex%s}\n"
      bb norm bb norm bb norm bb norm bb norm bb norm;
    Printf.printf "%signore%s <check> : ignore memory model check, with <check> as below\n" bb norm;
    Printf.printf "%sconsider%s <check> : consider (previously ignored) memory model check\n" bb norm;
    let rec f = function
      | Cmm.Leaf _ -> ()
      | Cmm.Node bs -> List.iter (fun (nm, t) -> Printf.printf "\t%s%s%s\n" bb nm norm; f t) bs in
    f model.Cmm.consistent;
    List.iter
      (function
        | Cmm.One (nm, _)
        | Cmm.Two (nm, _) ->
          Printf.printf "\t%s%s%s\n" bb nm norm)
      model.Cmm.undefined;
    Printf.printf "%sset%s <ppmode> = <value>, with <ppmode> one of the following\n" bb norm;
    Printf.printf        "\t%sfontsize%s : font size (int, currently %n) \n" bb norm m.fontsize;
    Printf.printf        "\t%snode_height%s : node height (float, currently %f) \n" bb norm m.node_height;
    Printf.printf        "\t%snode_width%s : node width (float, currently %f) \n" bb norm m.node_width;
    Printf.printf        "\t%sfilled%s : fill node (bool, currently %s%B%s) \n" bb norm bb m.filled norm;
    Printf.printf        "\t%sxscale%s : xscale for neato graphs (float, currently %f) \n" bb norm m.xscale;
    Printf.printf        "\t%syscale%s : yscale for neato graphs (float, currently %f) \n" bb norm m.yscale;
    Printf.printf        "\t%snodesep%s : for dot graphs (float, currently %f) \n" bb norm m.nodesep;
    Printf.printf        "\t%sranksep%s : for dot graphs (float, currently %f) \n" bb norm m.ranksep;
    Printf.printf        "\t%spenwidth%s : line width (float, currently %f) \n" bb norm m.penwidth;
    Printf.printf        "\t%slegend%s : graph legend (\"filename\" or other string, currently %s)\n" bb norm (Pp.pp_legend () m.legend);
    Printf.printf        "\t%slayout%s : (<%sdot%s/%sneato_downwards%s/%sneato_par%s/%sneato_par_init%s>, currently %s%s%s)\n" bb norm bb norm bb norm bb norm bb norm bb (Pp.pp_layout () m.layout) norm;
    Printf.printf        "\t%stex%s    : LaTeX node labels (bool, currently %s%B%s) \n" bb norm bb m.texmode norm;
    Printf.printf        "\t%sthread_ids%s: thread ids in node labels (bool, currently %s%B%s) \n" bb norm bb m.thread_ids norm;
    Printf.printf "%ssuppress_action%s <actionnames> : to not display an action\n" bb norm;
    Printf.printf "%sdisplay_action%s <actionnames>  : to cancel a previous suppress_action\n" bb norm;
    Printf.printf "%ssuppress_edge%s <relnames>    : to not display all edges of relname\n" bb norm;
    Printf.printf "%sdisplay_edge%s <relnames>     : to cancel a previous suppress_edge\n" bb norm;
    Printf.printf "\n<relname> can be (%ssb%s | %sasw%s | %sdd%s | %scd%s | %srf%s | %ssc%s | %smo%s | %slk%s | %ssw%s | %sdob%s\n   | %svse%s | %svsses%s | %sithb%s | %shb%s | %sdr%s | %sur%s | %sir%s | %scad%s | %sdummy%s | %srs%s | %shrs%s)\n" bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm bb norm;
(*    Printf.printf "Even finer control can be achieved for relations relname above.\n"; *)
    Printf.printf "One can also suppress or display particular edges, eg %ssuppress_edge%s %srf%s:a --> e, \nor %ssuppress_edge%s a --%srf%s--> e. " bb norm bb norm bb norm bb norm;
    Printf.printf "Note that in this case, only the edges asked to be \ndisplayed (or suppressed) will be displayed (or suppressed).\n";
(*     Printf.printf "If multiple %sdisplay%s or %ssuppress%s are given for a particular relation, we follow the algorithm below to decide\n" bb norm bb norm; *)
(*     Printf.printf          "\tFirst, whether display or suppress is in effect depends on the last command seen for that relation\n"; *)
(*     Printf.printf          "\tIf the last command is unconditional (e.g. %sdisplay%s %srf%s), then the effect is unconditional\n" bb norm bb norm; *)
(*     Printf.printf          "\tOtherwise (the last command mentions particular edges), all edges mentioned in the last command of that type are taken into account\n"; *)
    (* TODO: describe add and remove actions and edges *)
    Printf.printf "-------------------------------------------------------------------------------\n";
  end

let interact_user_checker (model, model_name) ecro sofar exwit instrs (Some filename) continuation =
  let (exod,exed,exdd) = (exwit.exod,exwit.exed,exwit.exdd) in
  let rl = (* EMPTY *) ([],[]) in
  let heremsg () = 
    (match ecro with
      | None -> Printf.printf "Before checking wrt model, execution candidate no: %d\n" sofar.candidates_with_orders_count
      | Some mecr ->
        let is_consistent = mecr.Types.masked_ecr_consistent_masked in
        Printf.printf "Checked to be %B. Executions found : %d\n" is_consistent sofar.checked_candidates_count);
    (match ecro with
      | None -> () 
      | Some mecr -> 
        Printf.printf "\n%s" (Pp.pp_masked_execution_check_result VT220 mecr))
  in
  let () = if !quietmode then () else heremsg () in
  let outdot exwit my_instrs =
    let tmpfile = !Globals.tmpfilename in
    let () =
      let ex = Execfile.filter_out_exwit_wrap exwit my_instrs in
      let str = Printf.sprintf "%a" (Pp.pp_dot) (my_instrs.mode,Some filename,ex) in
      let fd = open_out (tmpfile ^ ".dot") in
      let () = output_string fd str in
      let () = close_out fd in
      let _ = Sys.command (Printf.sprintf "./render_script.sh %s %s.dot %s.pdf" (match my_instrs.mode.layout with LO_dot -> "dot" | _ -> "neato") tmpfile tmpfile) in
      () in
    exwit
  in
  let rec loop exwit instrs = 
    let exwit = outdot exwit instrs in
    let () = 
      if !quietmode then heremsg () else ();
      Printf.printf "\nInput any instructions (or type \"help\"):\n"
    in
    let line = try read_line () with End_of_file -> raise UserQuit in 
    let new_instrs = 
      try 
        read_instructions_buf (Lexing.from_string line) exwit instrs 
      with 
        Instruction_read_error s -> Printf.printf "Error: %s\nPlease try again\n" s; instrs
    in
    try
      let new_rel rname old = 
         List.filter
           (fun (a1,a2) -> List.for_all (fun (e,apairs) -> e <> rname || not (List.mem (a1,a2) apairs)) new_instrs.remove_rels)
           (remove_duplicates 
              (List.fold_left (fun k (e,apairs) -> if e = rname then apairs @ k else k) old new_instrs.add_rels));
      in
      let new_exod =
        {exwit.exod with
         actions = 
         List.filter 
           (fun a -> not (List.mem a new_instrs.remove_actions))
           (remove_duplicates (exwit.exod.actions @ new_instrs.add_actions));
         sb = new_rel "sb" exwit.exod.sb;
         asw = new_rel "asw" exwit.exod.asw;
         dd = new_rel "dd" exwit.exod.dd;
         cd = new_rel "cd" exwit.exod.cd;
       } in
      let new_exed =
        {rf = new_rel "rf" exwit.exed.rf;
         sc = new_rel "sc" exwit.exed.sc;
         mo = new_rel "mo" exwit.exed.mo;
         lo = new_rel "lo" exwit.exed.lo;
         ao = new_rel "ao" exwit.exed.ao;
         tot = new_rel "tot" exwit.exed.tot;
       } in
      let new_exdd =
        {exwit.exdd with
          derived_relations = List.map (fun (nm, rel) -> (nm, new_rel nm rel)) exwit.exdd.derived_relations;
          undefined_behaviour = List.map (fun (nm, fault) -> match fault with
            | One acts -> (nm, One acts)
            | Two rel -> (nm, Two (new_rel nm rel)))
            exwit.exdd.undefined_behaviour;
        } in
      let new_exwit = {exod = new_exod; exed = new_exed; exdd = new_exdd; } in
      let new_exwit =
        List.fold_left
          (fun k c ->
	    match c with
            | Quit -> raise UserQuit
            | Continue -> raise Loop
            | StopAt s -> (Globals.stop_at := s; k)
            | Help -> (helptext model new_instrs.mode ();k)
	    | Relabel ->
		Pp.rename_exwit k new_instrs.mode
            | Generate (Dot,dotfile) ->
                let str = Printf.sprintf "%a" (Pp.pp_dot) (new_instrs.mode,Some filename,(filter_out_exod new_exwit.exod new_instrs,Some (filter_out_exed new_exwit.exed new_instrs),Some (filter_out_exdd new_exwit.exdd new_instrs))) in
                let fd = open_out dotfile in
                let () = output_string fd str in
                let () = close_out fd in
                k
            | Generate (Isa,isafile) ->
                let str = Printf.sprintf "%a" Pp.pp_isa (new_instrs.mode,filename,(new_exwit.exod,Some new_exwit.exed,Some new_exwit.exdd)) in
                let fd = open_out isafile in
                let () = output_string fd str in
                let () = close_out fd in
                k
            | Generate (Tex,texfile) ->
                let str = Printf.sprintf "%a" Pp.pp_tex (new_instrs.mode,Some filename,(new_exwit.exod,Some new_exwit.exed,Some new_exwit.exdd)) in
                let fd = open_out texfile in
                let () = output_string fd str in
                let () = close_out fd in
                k
            | Generate (Exc,excfile) ->
                let () = Pp.generate_exec_file (model_name,new_instrs.mode,Some filename,(new_exwit.exod,Some new_exwit.exed,Some new_exwit.exdd)) excfile in
                k
            | Generate (Instructions,dspfile) ->
              (* TODO: jp: ppmode_default_tex is hardcoded; bad *)
                let () = Pp.generate_display_file (Pp.Dsp_quiet,ppmode_default_tex,
                                                   new_exwit.exod,
                                                   {new_instrs with 
                                                    add_actions = [];
                                                    remove_actions = []; 
                                                    add_rels = []; 
                                                    remove_rels = []; 
                                                    commands = [];})  dspfile in
                k)
          new_exwit new_instrs.commands in
      loop new_exwit
        {new_instrs with
         add_actions = [];
         remove_actions = [];
         add_rels = [];
         remove_rels = [];
         commands = [];
       }
    with
      Loop ->
        begin
          Printf.printf "Continuing on...\n"; 
          {instrs with
           add_actions = [];
           remove_actions = [];
           add_rels = [];
           remove_rels = [];
           commands = [];
         }
        end
  in
  let i = 
    match !Globals.stop_at,ecro with
      | Always,_ -> loop exwit instrs
      | (OnSolutions,Some mecr) when mecr.masked_ecr_consistent_masked -> 
        begin
          if matches_exwit exwit instrs.constrain_rels then loop exwit instrs
          else instrs
        end
      | (OnCandidates,Some _) ->
        begin
        (*        let ((exp_exod,exp_exedo,exp_exddo),instrs,ignorechecks) = 
                  read_exec_file true filenameexc (Some exod.actions) in
                  let exp_exed = match exp_exedo with None -> empty_execution_existential_data | Some exed -> exed in
                  let exp_exdd = match exp_exddo with None -> empty_execution_derived_data | Some exed -> exed in
                  if pp_exod_subset exp_exod exod && pp_exed_subset exp_exed exed && pp_exdd_subset exp_exdd exdd
        *)
          if matches_exwit exwit instrs.constrain_rels then loop exwit instrs
          else instrs
        end
      | _ -> instrs
  in
  continuation i

let final_action filename summ =
  Printf.printf "%s" (Pp.pp_summary () (!Globals.quietmode,true,filename,summ))

let interaction_handles = {
  RunOpsem.interact_user_w_raw_candidate = interact_user_w_raw_candidate;
  RunOpsem.interact_user_w_preliminary_soln = interact_user_w_preliminary_soln;
  RunOpsem.interact_user_w_rfmap = interact_user_w_rfmap;
  RunOpsem.interact_user_w_rfmap_soln = interact_user_w_rfmap_soln;
  RunOpsem.interact_user_w_sc = interact_user_w_sc;
  RunOpsem.interact_user_w_mo = interact_user_w_mo;
  RunOpsem.interact_user_checker = interact_user_checker;
  RunOpsem.final_action = final_action;
}
