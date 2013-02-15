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

open Ids
open Value
open Constraints
open Types 
open Cabs
open Error
open Auxl
open Nat_num

(**
 * The parameters and resulting data of evaluations of subterms of the language
 *)

(**
 * Wrapping "real" actions from types.ml with two additional ones for threads,
 * which are used only for calculation (of additional_syncwith edges).
 * These new ones should be discarded when all calculations are finished. 
 *)
type my_action =
  | RealAction of Cmm.action                            (** Just an ordinary action *)
  | ThreadCreate of Cmm.aid * Cmm.tid * Cmm.tid (** Create a thread with id, thread creator, thread created *)
  | ThreadJoin of Cmm.aid * Cmm.tid * Cmm.tid   (** Join a thread with id, thread issuing join, thread joined *)


let my_action_id_of ma =
  match ma with
  | RealAction a -> Cmm.aid_of a
  | ThreadCreate (aid,_,_) -> aid
  | ThreadJoin (aid,_,_) -> aid

let real_action_of ma = 
  match ma with
  | RealAction a -> Some a
  | _ -> None

let real_actions_of mas = 
  option_map real_action_of mas


(** 
 * Map program variables to locations they have been allocated to
 *)
type varmap = (string * vloc) list

(**
 * The type of environments for evaluation
 *)
type env = {
  current_file   : file;
  thread_id      : Cmm.tid;
  (* 
   * Start from the top and work way down. This simulates a
   * stack-like discipline, since eg x looks first in the 
   * current "stack-frame", then that of the caller (which 
   * is second in the list), etc. Thus a list of varmaps 
   * instead of a single varmap.
   *) 
  var_loc_map    : varmap list;
  }


(**
 * Record the environment and statement to be executed for newly 
 * created threads
 *)
type new_thread_data = 
    {env  : env;
     stmt : statement;
   }


let cross_reln (froms:'a list) (tos:'a list) : 'a reln = 
  List.flatten 
    (List.map 
       (fun a1 -> 
	 List.map 
	   (fun a2 ->
	     a1,a2)
	   tos)
       froms) 

(**
 * An expression can return either a LVal or an RVal, and a statement returns Nothing
 *)
type result_type =
  | LVal of vloc * Cmm.cvalue option (* Location, and possible value stored there *)
  | RVal of Cmm.cvalue
  | Nothing

(**
 * The result of executions
 *)
type execution_result = {
  my_actions          : my_action list;

  global_vconstraint : vconstraint;

  loc_kind_map       : loc_kind_map;

  (*
   * Note that new_threads is unevaluated (contains a statement, not 
   * the resulting actions), and will be evaluated by the calling context
   *)
  new_threads        : new_thread_data list;
  
  (* 
   * The relations build from the operational semantics. 
   * Note that control_deps and seq_deps are a superset of the "real" 
   * relations, including also our thread-creation and join events. Finally, 
   * after calculations, they will be cut down to the real ones.
   * addtnl_syncwith and data_deps do not go to and from the thread actions, 
   * so those are just the actual relations
   *)
  addtnl_syncwith    : Cmm.action reln;
  data_deps          : Cmm.action reln;
  control_deps       : my_action reln;
  seq_deps           : my_action reln;

  result             : result_type;   
}

let unit_execution_result : execution_result = 
  {my_actions = [];
   global_vconstraint = ctrue;
   loc_kind_map = [];
   new_threads = [];
   addtnl_syncwith = [];
   data_deps = [];
   control_deps = [];
   seq_deps = [];
   result = Nothing;
 }



(**
 * Concatenate two execution results together, with the result being that 
 * of the second one
 *)
let concat (fst:execution_result) (snd:execution_result) : execution_result =
  let new_global_vconstraint =
    match fst.global_vconstraint,snd.global_vconstraint with
    | Conj d1,Conj d2 -> Conj (d1 @ d2)
  in
  {my_actions = fst.my_actions @ snd.my_actions;
   global_vconstraint = new_global_vconstraint;
   loc_kind_map = fst.loc_kind_map @ snd.loc_kind_map;
   new_threads = fst.new_threads @ snd.new_threads;
   addtnl_syncwith = fst.addtnl_syncwith @ snd.addtnl_syncwith;
   data_deps = fst.data_deps @ snd.data_deps;
   control_deps = fst.control_deps @ snd.control_deps;
   seq_deps = fst.seq_deps @ snd.seq_deps;
   result = snd.result; (* Throw away the result of the first *)
 }


(** 
 * Evaluate a list, concatenating the results
 *)
let concat_evaluates 
    (evalfun : 'a -> execution_result) 
    (arg: 'a list) 
    : execution_result = 
  List.fold_right
    (fun a k ->
      concat (evalfun a) k) arg unit_execution_result



let filter_ddep_sources (mas:my_action list) : Cmm.action list = 
  List.filter 
(*TODO: Recheck *)
(* the actions that there might be a ddep edge *from* *)
    (function
      | Cmm.Unlock (_,_,_)       
      | Cmm.RMW (_,_,_,_,_,_) 
      | Cmm.Store (_,_,_,_,_) 
      | Cmm.Load (_,_,_,_,_) -> true
      | Cmm.Lock (_,_,_,_)          
      | Cmm.Fence (_,_,_)
      | Cmm.Blocked_rmw (_,_,_) -> false
    )
    (real_actions_of mas)

let filter_ddep_targets (mas : my_action list) : Cmm.action list =
  (* actions there might be a ddep edge *to* *)
  real_actions_of mas

let filter_s_or_cdep_sources (mas : my_action list) : my_action list = 
(*TODO: Recheck this, everything is a possible source & target now!!??*)
  mas

let filter_s_or_cdep_targets (mas : my_action list) : my_action list = 
(*TODO: Recheck this, everything is a possible source & target now!!??*)
  mas


(**
 * Sequence two execution results together, with the result being that of the 
 * second one
 * This is like concat, except additionally new sequencing deps are added from 
 * the first to the second
 *)
let sequence (fst:execution_result) (snd:execution_result) : execution_result =
  let targets = filter_s_or_cdep_targets snd.my_actions in
  let new_sdeps = 
    cross_reln fst.my_actions targets in
  let new_global_vconstraint =
    match fst.global_vconstraint,snd.global_vconstraint with
    | Conj d1,Conj d2 -> Conj (d1 @ d2)
  in
  {my_actions = fst.my_actions @ snd.my_actions;
   global_vconstraint = new_global_vconstraint;
   loc_kind_map = fst.loc_kind_map @ snd.loc_kind_map;
   new_threads = fst.new_threads @ snd.new_threads;
   addtnl_syncwith = fst.addtnl_syncwith @ snd.addtnl_syncwith;
   data_deps = fst.data_deps @ snd.data_deps;
   control_deps = fst.control_deps @ snd.control_deps;
   seq_deps = fst.seq_deps @ snd.seq_deps @ new_sdeps;
   result = snd.result; (* Throw away the result of the first *)
 }


(**
 * Sequence with control-dependency two execution results together, with 
 * the result being that of the second one
 * This is like sequence, except additionally new control deps are added from 
 * the first to the second
 *)
let sequence_with_cdeps (fst:execution_result) (snd:execution_result) : execution_result =
  let targets = filter_s_or_cdep_targets snd.my_actions in
  let new_scdeps = 
    cross_reln fst.my_actions targets in
  let new_global_vconstraint =
    match fst.global_vconstraint,snd.global_vconstraint with
    | Conj d1,Conj d2 -> Conj (d1 @ d2)
  in
  {my_actions = fst.my_actions @ snd.my_actions;
   global_vconstraint = new_global_vconstraint;
   loc_kind_map = fst.loc_kind_map @ snd.loc_kind_map;
   new_threads = fst.new_threads @ snd.new_threads;
   addtnl_syncwith = fst.addtnl_syncwith @ snd.addtnl_syncwith;
   data_deps = fst.data_deps @ snd.data_deps;
   control_deps = fst.control_deps @ snd.control_deps @ new_scdeps;
   seq_deps = fst.seq_deps @ snd.seq_deps @ new_scdeps;
   result = snd.result; (* Throw away the result of the first *)
 }


let combine 
    (fst: (execution_result -> 'a -> 'a) -> 'a -> 'a) 
    (snd: (execution_result -> 'a -> 'a) -> 'a -> 'a) 
    (comb : execution_result -> execution_result -> execution_result) 
    (sc : execution_result -> 'a -> 'a) 
    (s:'a) : 'a = 
  let proc_fst r1 s1 =
    let proc_snd r2 s2 =
      let r = comb r1 r2 in
      sc r s2 in
    snd proc_snd s1 in
  fst proc_fst s
  


(**
 * Utility functions for the operational semantics
 *)
let rec lookup_vmap vmap vi =
  match vmap with
  | [] -> raise Not_found
  | (vi',vm) :: vmap -> if vi' = vi then vm else lookup_vmap vmap vi

let rec lookup vmaps vi = 
  match vmaps with
  | [] -> raise Not_found
  | vmap :: vmaps -> 
      try 
	lookup_vmap vmap vi
      with
	Not_found -> lookup vmaps vi

let add_new_var (n:string) (l:vloc) (old:env) =
  let vmap = old.var_loc_map in
  let new_vmap = 
    match vmap with
    | vm :: vmap' -> ((n,l) :: vm) :: vmap'
    | [] -> [[(n,l)]] in
  {old with var_loc_map = new_vmap}

let name_of_single_name (_,(s,_,_,_)) = s 

let rec find_tp spec =
  match spec with
  | [] -> internal_error "Cannot discover type"
  | SpecType tp :: _ -> tp
  | _ :: spec' -> find_tp spec'

let type_of_single_name sn = 
    let (spec,(_,t,_,_)) = sn in
    find_tp spec 


let find_fun_by_name (file:file) (name:string) : (single_name * block) =
  let rec find_f gs = 
    match gs with
    | [] -> user_error ("Function " ^ name ^ " not found") 
    | g :: gs -> 
	begin
	  match g with
	  | FUNDEF(f,b,_,_) when (name_of_single_name f) = name -> (f,b)
	  | _ -> find_f gs
	end
  in
  find_f (snd file)


let make_val_from_const (c:constant) : Cmm.cvalue = 
  match c with
  | CONST_INT s -> Cmm.Rigid (Cmm.Concrete (int_of_string s))
  | CONST_STRING s -> Cmm.Rigid (fresh_var_named ("string"^s)) (* Hacky, will fail if program writes to the string *)
  | _ -> cannot_handle "Non-integral/string constant"



let conv_mo (mo:Cabs.memory_order) : Cmm.memory_order =
  match mo with
  | MO_RELAXED -> Cmm.Relaxed
  | MO_CONSUME -> Cmm.Consume
  | MO_ACQUIRE -> Cmm.Acquire
  | MO_RELEASE -> Cmm.Release
  | MO_ACQ_REL -> Cmm.Acq_rel
  | MO_SEQ_CST -> Cmm.Seq_cst


(**
 * Substitution functions
 *)
  

let subst_action s a = 
  match a with
  | Cmm.Unlock (aid,tid,l) -> 
      Cmm.Unlock (aid,tid, subst_val s l)
  | Cmm.Lock (aid,tid,l,oc) -> 
      Cmm.Lock (aid,tid, subst_val s l,oc)
  | Cmm.Load (aid,tid,mo,l,v) -> 
      Cmm.Load (aid,tid,mo, subst_val s l, (subst_val s v))
  | Cmm.Store (aid,tid,mo,l,v) -> 
      Cmm.Store (aid,tid,mo, subst_val s l, (subst_val s v))
  | Cmm.RMW (aid,tid,mo,l,v1,v2) -> 
      Cmm.RMW (aid,tid,mo, subst_val s l,(subst_val s v1),(subst_val s v2))
  | Cmm.Blocked_rmw (aid,tid,l) -> 
      Cmm.Blocked_rmw (aid,tid,subst_val s l)
  | Cmm.Fence (aid,tid,mo) -> 
      Cmm.Fence (aid,tid,mo)

let subst_action_reln s rel = 
  List.map 
    (fun (a1,a2) -> subst_action s a1,subst_action s a2) rel

let subst_my_action s a = 
  match a with
  | RealAction ta -> RealAction (subst_action s ta)
  | ThreadCreate (aid,tid1,tid2) -> ThreadCreate (aid,tid1,tid2)
  | ThreadJoin (aid,tid1,tid2) -> ThreadJoin (aid,tid1,tid2)

let subst_my_action_reln s rel = 
  List.map 
    (fun (a1,a2) -> subst_my_action s a1,subst_my_action s a2) rel

let subst_execution_result s res =
  {my_actions = List.map (subst_my_action s) res.my_actions;
   global_vconstraint = subst_vconstraint s res.global_vconstraint;
   loc_kind_map = List.map (fun (l,k) -> subst_val s l,k) res.loc_kind_map;
   new_threads = res.new_threads; (* Assumed fully evaluated, ignore this *)
   addtnl_syncwith = subst_action_reln s res.addtnl_syncwith;
   data_deps = subst_action_reln s res.data_deps;
   control_deps = subst_my_action_reln s res.control_deps;
   seq_deps = subst_my_action_reln s res.seq_deps;
   result = match res.result with 
   | Nothing -> Nothing
   | LVal (v,copt) -> LVal (subst_val s v,copt)
   | RVal v -> RVal (subst_val s v);
 }   

(**
 * Pretty printing functions
 *)

let pp_loc = pp_value

let pp_thread_id = Pp.pp_thread_id
let pp_action_id = Pp.pp_action_id

let pp_outcome () = function
  | Cmm.Locked -> "Locked"
  | Cmm.Blocked -> "Blocked"

let pp_mo () = function
  | Cmm.NA -> "NA"
  | Cmm.Seq_cst -> "SC"
  | Cmm.Relaxed -> "Rlx"
  | Cmm.Release -> "Rel"
  | Cmm.Acquire -> "Acq"
  | Cmm.Consume -> "Con"
  | Cmm.Acq_rel -> "A/R"

let pp_action () = function
  | Cmm.Lock (aid,tid,l,oc) -> 
      Printf.sprintf "%a(%a):Lock %a -> %a" pp_action_id aid  pp_thread_id tid  pp_loc l  pp_outcome oc
  | Cmm.Unlock (aid,tid,l) ->
      Printf.sprintf "%a(%a):Unlock %a" pp_action_id aid  pp_thread_id tid  pp_loc l
  | Cmm.Load (aid,tid,mo,l,v) ->
      Printf.sprintf "%a(%a):Load(%a) %a -> %a" pp_action_id aid  pp_thread_id tid  pp_mo mo  pp_loc l  pp_value v
  | Cmm.Store (aid,tid,mo,l,v) ->
      Printf.sprintf "%a(%a):Store(%a) %a <- %a" pp_action_id aid  pp_thread_id tid  pp_mo mo  pp_loc l  pp_value v
  | Cmm.RMW (aid,tid,mo,l,v1,v2) ->
      Printf.sprintf "%a(%a):RMW(%a) %a = %a <-> %a" pp_action_id aid  pp_thread_id tid  pp_mo mo  pp_loc l  pp_value v1  pp_value v2
  | Cmm.Blocked_rmw (aid,tid,l) ->
      Printf.sprintf "%a(%a):BRMW %a" pp_action_id aid  pp_thread_id tid  pp_loc l
  | Cmm.Fence (aid,tid,mo) ->
      Printf.sprintf "%a(%a):Fence(%a)" pp_action_id aid  pp_thread_id tid  pp_mo mo

let pp_my_action () = function
  | RealAction a -> pp_action () a
  | ThreadCreate (aid,tid1,tid2) ->
      Printf.sprintf "%a(%a):Create Thread %a" pp_action_id aid pp_thread_id tid1 pp_thread_id tid2
  | ThreadJoin (aid,tid1,tid2) ->
      Printf.sprintf "%a(%a):Join Thread %a" pp_action_id aid pp_thread_id tid1 pp_thread_id tid2



let pp_action_reln name () ar =
  String.concat ", " 
    (List.map 
       (fun (a1,a2) ->
	 Printf.sprintf "%a -%s-> %a"
	 pp_action_id (Cmm.aid_of a1) name pp_action_id (Cmm.aid_of a2))
       ar)

let pp_my_action_reln name () ar =
  String.concat ", " 
    (List.map 
       (fun (a1,a2) ->
	 Printf.sprintf "%a -%s-> %a"
	 pp_action_id (my_action_id_of a1) name pp_action_id (my_action_id_of a2))
       ar)


let pp_loc_kind () = function
  | Cmm.Non_Atomic -> "non-atomic"
  | Cmm.Atomic -> "atomic"
  | Cmm.Mutex -> "mutex"


let pp_execution_result () res =
  let pp_actions = "{\n" ^ String.concat ";\n" (List.map (fun a -> Printf.sprintf "\t%a" pp_my_action a) res.my_actions) ^ "\n}" in
  let pp_vconstraint = Printf.sprintf "%a" (pp_vconstraint) res.global_vconstraint in
  let pp_loc_kinds = String.concat ", " (List.map (fun (l,k) -> Printf.sprintf "%a[%a]" pp_loc l pp_loc_kind k) res.loc_kind_map) in
  let pp_addtnl_syncwith = Printf.sprintf "%a" (pp_action_reln "asw") res.addtnl_syncwith in
  let pp_data_deps = Printf.sprintf "%a" (pp_action_reln "dd") (transitive_reduction res.data_deps) in
  let pp_control_deps = Printf.sprintf "%a" (pp_my_action_reln "cd") (transitive_reduction res.control_deps) in
  let pp_seq_deps = Printf.sprintf "%a" (pp_my_action_reln "sb") (transitive_reduction res.seq_deps) in
  let pp_result = 
    match res.result with
    | LVal (v,copt) -> Printf.sprintf "loc(%a)" pp_value v
    | RVal v -> Printf.sprintf "val(%a)" pp_value v
    | Nothing -> "NONE"
  in
  Printf.sprintf "actions = %s\nconstraints = %s\nlocations_with_kinds = %s\naddtnl_syncwith = %s\ndata_deps = %s\ncontrol_deps = %s\nseq_deps = %s\nresult = %s\n" pp_actions pp_vconstraint pp_loc_kinds pp_addtnl_syncwith pp_data_deps pp_control_deps pp_seq_deps pp_result


(**
 * Conversion functions
 *)
let make_addtnl_syncwiths res = 
  let find_maximals (lst:Cmm.action list) (order:(my_action*my_action) list) : Cmm.action list =
    List.filter
      (fun a1 ->
	not (List.exists
	       (fun a2 ->
		 List.mem (RealAction a1,RealAction a2) order && Cmm.aid_of a1 != Cmm.aid_of a2)
	       lst))
      lst in
  let find_minimals (lst:Cmm.action list) (order:(my_action*my_action) list) : Cmm.action list =
    List.filter
      (fun a1 ->
	not (List.exists
	       (fun a2 ->
		 List.mem (RealAction a2,RealAction a1) order && Cmm.aid_of a1 != Cmm.aid_of a2)
	       lst))
      lst in
  let addtnl_syncwith_creates = 
    List.fold_left
      (fun asws mac ->
	match mac with
	| ThreadCreate (aid,tid_creator,tid_createe) ->
	    let creators_actions_before = 
	      List.filter 
		(fun ma -> 
		  match ma with
		  | RealAction a -> 
		      Cmm.tid_of a = tid_creator && 
		      List.mem (ma,mac) res.seq_deps
		  | _ -> false)
		res.my_actions in
	      let createes_actions = 
		List.filter
		  (fun ma ->
		    match ma with
		    | RealAction a -> 
			Cmm.tid_of a = tid_createe
		    | _ -> false) 
		  res.my_actions in
	      (cross_reln 
		 (find_maximals (real_actions_of creators_actions_before) res.seq_deps) 
		 (find_minimals (real_actions_of createes_actions) res.seq_deps)) @ 
	      asws
	  | _ -> asws)
      [] res.my_actions in
  let addtnl_syncwith_joins = 
    List.fold_left
      (fun asws maj ->
	  match maj with
	  | ThreadJoin (aid,tid_joiner,tid_joinee) ->
	      let joiners_actions_after = 
		List.filter 
		  (fun ma -> 
		    match ma with
		    | RealAction a -> 
			Cmm.tid_of a = tid_joiner && 
			List.mem (maj,ma) res.seq_deps
		    | _ -> false)
		  res.my_actions in
	      let joinees_actions = 
		List.filter
		  (fun ma ->
		    match ma with
		    | RealAction a -> 
			Cmm.tid_of a = tid_joinee
		    | _ -> false) 
		  res.my_actions in
	      (cross_reln 
		 (find_maximals (real_actions_of joinees_actions) res.seq_deps)
		 (find_minimals (real_actions_of joiners_actions_after) res.seq_deps)) @ 
	      asws
	  | _ -> asws)
      [] res.my_actions in
  {res with
   addtnl_syncwith = 
   addtnl_syncwith_creates @ addtnl_syncwith_joins @ res.addtnl_syncwith} 

let conv_execution_result r = 
  let asw = r.addtnl_syncwith in
  let lk = r.loc_kind_map in
  let actions = (real_actions_of r.my_actions) in
  let threads = 
    List.fold_left
      (fun k a ->
	let tid = Cmm.tid_of a in
	if List.mem tid k then k else tid :: k) [] actions in
  let sb =
    List.fold_left
      (fun k ar ->
	match ar with
	| (RealAction a1,RealAction a2) -> (a1,a2) :: k
	| _ -> k)
      [] r.seq_deps in
  let dd =  r.data_deps in
  let cd =
    List.fold_left
      (fun k ar ->
	match ar with
	| (RealAction a1,RealAction a2) -> (a1,a2) :: k
	| _ -> k)
      [] r.control_deps in
  {
   actions = actions;
   threads = threads;
   lk = lk;
   sb = sb;
   asw = asw;
   dd = dd;
   cd = cd;
   vconstraint = r.global_vconstraint }


