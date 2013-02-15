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
open Auxl
open Globals

type order_type = 
   Nonatomic
 | Atomic of Cmm.memory_order
 | Mutex

type memory_orders = 
  | MO_Star 
  | MO_Atomic
  | MO_Set of order_type list

type action_skeleton = 
   S_Lock of Cmm.aid * Cmm.tid 
 | S_Unlock of Cmm.aid * Cmm.tid 
 | S_Atomic_rmw of Cmm.aid * Cmm.tid * memory_orders
 | S_Read of Cmm.aid * Cmm.tid * memory_orders
 | S_Write of Cmm.aid * Cmm.tid * memory_orders
 | S_Fence of Cmm.aid * Cmm.tid * memory_orders


type 'a set = 'a list
type 'a reln = ('a * 'a) list

type action_rel = Cmm.action reln

type loc_kind_map = (Cmm.location * Cmm.location_kind) list


type execution_opsem_data = {
                                 actions : Cmm.action set;
                                 threads : Cmm.tid set;
(* location_kinds              *)     lk : loc_kind_map;
(* sequenced_before            *)     sb : action_rel;
(* additional_synchonized_with *)    asw : action_rel;
(* data_dependency             *)     dd : action_rel;
(* control_dependency          *)     cd : action_rel;

(* constraint on values *)   vconstraint : vconstraint}

let empty_execution_opsem_data = { actions = []; threads = []; lk = []; sb = []; asw = []; dd = []; cd = []; vconstraint = ctrue }

type execution_existential_data = {
(* reads_from              *)         rf : action_rel;
(* sc                      *)         sc : action_rel;
(* modification_order      *)         mo : action_rel;
lo: action_rel;
ao: action_rel;
tot: action_rel;
}

let empty_execution_existential_data model = { rf = []; sc = []; mo = []; lo = []; ao = []; tot = []; }

type execution_skeleton = {      
                                 es_actions : action_skeleton set;
                                 es_sameloc : action_skeleton set set;
                              es_atomiclocs : action_skeleton set;
                                 es_threads : thread_id set;
                                      es_sb : action_skeleton reln;
                                     es_asw : action_skeleton reln;
                                      es_dd : action_skeleton reln;
                                      es_cd : action_skeleton reln;
                                      es_rf : action_skeleton reln;
                                     (*es_sc : action_skeleton reln;*)
                                      es_mo : action_skeleton reln }


(* normal form:
   es_sameloc doesn't contain any pairs in es_rf or es_mo
   es_sb is, for each thread, a strict linear order without transitive edges
   es_dd and es_cd are subsets of es_sb
   es_mo is, for each location equivalence class (from es_sameloc, es_rf and es_mo), a strict linear order without transitive edges over [which kind of?] actions
   es_sc is a strict linear order over [which kind of?] actions 
*)

type fault =
    One of Cmm.action list
  | Two of action_rel

type execution_derived_data = {
  locations: Cmm.location set;
  derived_relations: (string * action_rel) list;
  undefined_behaviour: (string * fault) list;
} 
    
let empty_execution_derived_data = {
  locations = [];
  derived_relations = [];
  undefined_behaviour = [];
}

type predicate_tree =
    Pred_leaf of bool
  | Pred_node of (string * predicate_tree) list

type execution_check_result = {
  ecr_pieces_of_consistent: predicate_tree;
  ecr_undefined_behaviour: (string * bool) list;
}

type masked_predicate_tree =
    Masked_pred_leaf of bool
  | Masked_pred_node of bool * (string * masked_predicate_tree) list
  | Masked_dead_pred_leaf of bool
  | Masked_dead_pred_node of bool * (string * masked_predicate_tree) list

type masked_execution_check_result = {
  masked_ecr_pieces_of_consistent: masked_predicate_tree;
  masked_ecr_undefined_behaviour: (string * bool) list;
  masked_ecr_consistent_real: bool;
  masked_ecr_consistent_masked: bool;
  masked_ecr_consistent_race_free: bool;
}


(*
type thread = {
  thread_id : thread_id;
  actions : action list;
  threadwise_sequenced_before : (action * action) list;
  threadwise_data_dependency : (action * action) list;
  threadwise_control_dependency : (action * action) list }

type execution_opsem_data = {
                     threads : thread list;
               location_kind : (location * location_kind) list;
additional_synchronizes_with : (action * action) list;
       kill_dependency_edges : (action * action) list  
          action_constraints : (action * Value.cnstraints) list       }

type execution_existential_data = {
                          rf : (action * action) list;
                          sc_order : (action * action) list;
                          modification_order : (action * action) list;
                          lock_order : (action * action) list;
                          synchronizes_with : (action * action) list;
                          dependency_order : (action * action) list;
                          ithb : (action * action) list;
                          happens_before : (action * action) list }
*)

type terminalmode =
  | VT220
  | Ascii

type layoutmode = 
| LO_dot
| LO_neato_par
| LO_neato_par_init
| LO_neato_downwards

type ppmode = {
    fontsize    : int  ;
    fontname    : string  ;
    node_height : float;
    node_width  : float;
    filled      : bool;
    xscale      : float;
    yscale      : float;
    ranksep     : float;
    nodesep     : float;
    penwidth    : float;
    legend      : string option;
    layout      : layoutmode;
    texmode     : bool;
    thread_ids  : bool;
  } 

let ppmode_default_tex = {
  fontsize    = 12;
  fontname    = "Helvetica";
  node_height = 0.2;
  node_width  = 0.8;
  filled      = false;
  xscale      = 1.0;
  yscale      = 0.5;
  ranksep     = 0.2; (* for dot - but it seems to clip-below at 0.2, for no reason*)
  nodesep     = 0.1;
  penwidth    = 2.0;
  legend      = None; (*Some "filename";*)
  layout      = LO_neato_downwards;
  texmode     = true;
  thread_ids  = false
} 


let ppmode_default_ps = { ppmode_default_tex with
  node_height = 0.18;
  node_width  = 1.5;
  fontname    = "Helvetica";
  fontsize    = 17;
  xscale      = 1.0;
  yscale      = 0.5;
  penwidth    = 2.7;
  texmode     = false
}

let ppmode_default_web = {
  fontsize    = 10;
  fontname    = "Helvetica";
  node_height = 0.2;
  node_width  = 0.9;
  filled      = false;
  xscale      = 1.5;
  yscale      = 0.7;
  ranksep     = 0.2; (* for dot - but it seems to clip-below at 0.2, for no reason*)
  nodesep     = 0.25;   (* for dot and for self-loops in neato *)
  penwidth    = 1.0;
  legend      = None; (*Some "filename";*)
  layout      = LO_neato_par_init;
  texmode     = false;
  thread_ids  = false
}

type raw_ppmode_item = 
  | Fontsize    of int  
  | Node_height of float
  | Node_width  of float
  | Filled      of bool
  | Xscale      of float
  | Yscale      of float
  | Ranksep     of float
  | Nodesep     of float
  | Penwidth    of float
  | Legend      of string option
  | Layout      of layoutmode
  | Texmode     of bool
  | Thread_ids  of bool

type raw_ppmode = raw_ppmode_item list

let empty_raw_mode = []

let apply_raw_ppmode_item m rppm = 
  match rppm with
  | Fontsize    v -> {m with fontsize = v}
  | Node_height v -> {m with node_height = v}
  | Node_width  v -> {m with node_width = v}
  | Filled      v -> {m with filled = v}
  | Xscale      v -> {m with xscale = v}
  | Yscale      v -> {m with yscale = v}
  | Ranksep     v -> {m with ranksep = v}
  | Nodesep     v -> {m with nodesep = v}
  | Penwidth    v -> {m with penwidth = v}
  | Legend      v -> {m with legend = v}
  | Layout      v -> {m with layout = v}
  | Texmode     v -> {m with texmode = v}
  | Thread_ids  v -> {m with thread_ids = v}

let apply_raw_ppmode_items m rppms =
  List.fold_left apply_raw_ppmode_item m rppms

let raw_ppmode_item_same_kind = function
  | (Fontsize    _,Fontsize    _) -> true
  | (Node_height _,Node_height _) -> true
  | (Node_width  _,Node_width  _) -> true
  | (Filled      _,Filled      _) -> true
  | (Xscale      _,Xscale      _) -> true
  | (Yscale      _,Yscale      _) -> true
  | (Ranksep     _,Ranksep     _) -> true
  | (Nodesep     _,Nodesep     _) -> true
  | (Penwidth    _,Penwidth    _) -> true
  | (Legend      _,Legend      _) -> true
  | (Layout      _,Layout      _) -> true
  | (Texmode     _,Texmode     _) -> true
  | (Thread_ids  _,Thread_ids  _) -> true
  | (_,_) -> false

let rec override rppms = match rppms with
| [] -> []
| rppm :: rppms' -> 
    if List.exists 
        (function rppm' -> raw_ppmode_item_same_kind (rppm,rppm')) 
        rppms' 
    then override rppms' 
    else rppm :: override rppms'

let add_ppmode m1 m2 =
  override (m1 @ m2)

type action_or_name =
| Action of Cmm.action
| Action_name of Cmm.aid

type skeleton_action_or_name =
| S_Action of action_skeleton
| S_Action_name of Cmm.aid

type transition_sequence = action_or_name * ((string * action_or_name) list)

type skeleton_transition_sequence = skeleton_action_or_name * ((string * skeleton_action_or_name) list)

type nodespec = 
  | Actions of action_or_name list
  | All

type raw_edgeinstr =
  | RawCrossProduct of nodespec * nodespec
  | RawExact of (action_or_name * action_or_name) list

type execfileitem =
  | Model of string
  | Trans of transition_sequence
  | Lk of (Cmm.location * Cmm.location_kind) list 
  | Ignore of string list
  | DisplayEdgeInstr of (string * raw_edgeinstr) list
  | SuppressEdgeInstr of (string * raw_edgeinstr) list
  | DisplayNodeInstr of action_or_name list
  | SuppressNodeInstr of action_or_name list
  | Show of int option

type skeletonexecfileitem = 
  | S_Trans of skeleton_transition_sequence
  | S_Sameloc of skeleton_action_or_name list list
  | S_Atomiclocs of skeleton_action_or_name list

type edgeinstr =
  | CrossProduct of (Cmm.action list) option * (Cmm.action list) option
  | Exact of (Cmm.action * Cmm.action) list

type reln_instr =
  | Only of (Cmm.aid * Cmm.aid) list
  | Any

type instr =
  | DisplayEdge of (string * edgeinstr)
  | SuppressEdge of (string * edgeinstr)
  | DisplayNode of Cmm.action
  | SuppressNode of Cmm.action

type raw_check =
  | IgnoreCheck of string
  | ConsiderCheck of string

type edge_instruction = 
  | Display_e  of string * reln_instr
  | Suppress_e of string * reln_instr

type raw_edge_instructions = 
  | Raw_display_e  of (string * (transition_sequence) option) list
  | Raw_suppress_e of (string * (transition_sequence) option) list


type node_instruction =
  | Display_n  of Cmm.aid
  | Suppress_n of Cmm.aid

type raw_node_instructions =
  | Raw_display_n  of action_or_name list
  | Raw_suppress_n of action_or_name list

type raw_constrain_rel =
  | IgnoreRel of (string * action_or_name reln)
  | ConsiderRel of (string * action_or_name reln)

type command = 
  | Quit
  | Continue
  | StopAt of stops
  | Help
  | Relabel
  | Generate of filetype * filename

type raw_instructions =
    { raw_edge_instructions : raw_edge_instructions list;
      raw_node_instructions : raw_node_instructions list;
      raw_checks            : raw_check list;
      raw_mode              : raw_ppmode;
      raw_add_actions       : action_or_name list;
      raw_add_rels          : transition_sequence list;
      raw_remove_actions    : action_or_name list;
      raw_remove_rels       : transition_sequence list;
      raw_constrain_rels    : raw_constrain_rel list;
      raw_commands          : command list;
      raw_show              : int option;
    }      

type instructions =
    { edge_instructions : edge_instruction list;
      node_instructions : node_instruction list;
      ignorechecks      : string list;
      mode              : ppmode;
      add_actions       : Cmm.action list;
      add_rels          : (string * action_rel) list;
      remove_actions    : Cmm.action list;
      remove_rels       : (string * action_rel) list;
      constrain_rels    : (string * Cmm.aid reln) list;
      commands          : command list;
      show              : int option;
    }

let empty_instructions m =
  { edge_instructions = [];
    node_instructions = [];
    ignorechecks = [];
    mode = m;
    add_actions = [];
    add_rels = [];
    remove_actions = [];
    remove_rels = [];
    constrain_rels = [];
    commands = [];
    show = None;
  }

let empty_raw_instructions =
  { raw_edge_instructions = [];
    raw_node_instructions = [];
    raw_checks            = [];
    raw_mode              = empty_raw_mode;
    raw_add_actions       = [];
    raw_add_rels          = [];
    raw_remove_actions    = [];
    raw_remove_rels       = [];
    raw_constrain_rels    = [];
    raw_commands          = [];
    raw_show              = None;
  }
    
let add_raw_instructions ins1 ins2 =
  { raw_edge_instructions = ins1.raw_edge_instructions @ ins2.raw_edge_instructions;
    raw_node_instructions = ins1.raw_node_instructions @ ins2.raw_node_instructions;
    raw_checks = ins1.raw_checks @ ins2.raw_checks;
    raw_mode = add_ppmode ins1.raw_mode ins2.raw_mode; 
    raw_add_actions = ins1.raw_add_actions @ ins2.raw_add_actions;
    raw_add_rels = ins1.raw_add_rels @ ins2.raw_add_rels;
    raw_remove_actions = ins1.raw_remove_actions @ ins2.raw_remove_actions;
    raw_remove_rels = ins1.raw_remove_rels @ ins2.raw_remove_rels;
    raw_constrain_rels = ins1.raw_constrain_rels @ ins2.raw_constrain_rels;
    raw_commands = ins1.raw_commands @ ins2.raw_commands;
    raw_show = match ins2.raw_show with Some i -> Some i | None -> ins1.raw_show; 
  }

    
exception TypeError

let rec skeleton_action_id_of = function
    | S_Lock (aid,tid) -> aid
    | S_Unlock (aid,tid) -> aid
    | S_Atomic_rmw (aid,tid,mos) -> aid
    | S_Read (aid,tid,mos) -> aid
    | S_Write (aid,tid,mos) -> aid
    | S_Fence (aid,tid,mos) -> aid

let rec skeleton_thread_id_of = function
    | S_Lock (aid,tid) -> tid
    | S_Unlock (aid,tid) -> tid
    | S_Atomic_rmw (aid,tid,mos) -> tid
    | S_Read (aid,tid,mos) -> tid
    | S_Write (aid,tid,mos) -> tid
    | S_Fence (aid,tid,mos) -> tid

let skeleton_mos_of = function
    | S_Lock (aid,tid) -> None
    | S_Unlock (aid,tid) -> None
    | S_Atomic_rmw (aid,tid,mos) -> Some mos
    | S_Read (aid,tid,mos) -> Some mos
    | S_Write (aid,tid,mos) -> Some mos
    | S_Fence (aid,tid,mos) -> Some mos

let skeleton_located = function
    | S_Lock (aid,tid) -> true
    | S_Unlock (aid,tid) -> true
    | S_Atomic_rmw (aid,tid,mos) -> true
    | S_Read (aid,tid,mos) -> true
    | S_Write (aid,tid,mos) -> true
    | S_Fence (aid,tid,mos) -> false

let set_action_id aid = function
    | Cmm.Lock (aid', uu, uv, oc) -> Cmm.Lock (aid, uu, uv, oc) 
    | Cmm.Unlock (aid', uw, ux) -> Cmm.Unlock (aid, uw, ux) 
    | Cmm.Load (aid', uy, uz, va, vb) -> Cmm.Load (aid, uy, uz, va, vb) 
    | Cmm.Store (aid', vc, vd, ve, vf) -> Cmm.Store (aid, vc, vd, ve, vf) 
    | Cmm.RMW (aid', vg, vh, vi, vj, vk) -> Cmm.RMW (aid, vg, vh, vi, vj, vk) 
    | Cmm.Fence (aid', vr, vs) -> Cmm.Fence (aid, vr, vs) 
    | Cmm.Blocked_rmw (_, tid, l) -> Cmm.Blocked_rmw (aid, tid, l)

let set_thread_id tid = function
    | Cmm.Lock (aid, uu, uv, oc) -> Cmm.Lock (aid, tid, uv, oc) 
    | Cmm.Unlock (aid, uw, ux) -> Cmm.Unlock (aid, tid, ux) 
    | Cmm.Load (aid, uy, uz, va, vb) -> Cmm.Load (aid, tid, uz, va, vb)
    | Cmm.Store (aid, vc, vd, ve, vf) -> Cmm.Store (aid, tid, vd, ve, vf) 
    | Cmm.RMW (aid, vg, vh, vi, vj, vk) -> Cmm.RMW (aid, tid, vh, vi, vj, vk) 
    | Cmm.Fence (aid, vr, vs) -> Cmm.Fence (aid, tid, vs)
    | Cmm.Blocked_rmw (aid, _, l) -> Cmm.Blocked_rmw (aid, tid, l)


let exod_subset exod1 exod2 =
  list_subset exod1.actions exod2.actions &&
  list_subset exod1.threads exod2.threads &&
  list_subset exod1.lk exod2.lk &&
  list_subset exod1.sb exod2.sb &&
  list_subset exod1.asw exod2.asw &&
  list_subset exod1.dd exod2.dd &&
  list_subset exod1.cd exod2.cd

let exed_subset exed1 exed2 =
  list_subset exed1.rf exed2.rf &&
  list_subset exed1.sc exed2.sc &&
  list_subset exed1.mo exed2.mo

let my_sort (l : (string * 'a) list) =
  List.sort (fun (nm1, _) (nm2, _) -> compare nm1 nm2) l

let exdd_subset exdd1 exdd2 =
  let derived_rels1 = my_sort exdd1.derived_relations in
  let derived_rels2 = my_sort exdd2.derived_relations in
  let undef_beh1 = my_sort exdd1.undefined_behaviour in
  let undef_beh2 = my_sort exdd2.undefined_behaviour in
  List.length derived_rels1 = List.length derived_rels2 &&
  List.length undef_beh1 = List.length undef_beh2 &&
  List.for_all2 (fun (nm1, rel1) (nm2, rel2) ->
    nm1 = nm2 &&
    list_subset rel1 rel2)
    derived_rels1 derived_rels2 &&
  List.for_all2 (fun (nm1, fault1) (nm2, fault2) ->
    nm1 = nm2 &&
    (match fault1, fault2 with
      | One acts1, One acts2 -> list_subset acts1 acts2
      | Two rel1, Two rel2 -> list_subset rel1 rel2
      | One _, Two _ -> false
      | Two _, One _ -> false))
    undef_beh1 undef_beh2

type execution_data = 
{ exod : execution_opsem_data;
  exed : execution_existential_data;
  exdd : execution_derived_data;
}

let empty_execution_data model =
  { exod = empty_execution_opsem_data;
    exed = empty_execution_existential_data model;
    exdd = empty_execution_derived_data;
  }

type result = 
{ actual : execution_data;
  cutdown_for_drawing : execution_data}

type cand_summaries =
    {raw_candidates_count         : int;
     candidates_with_constraints_count : int;
     candidates_with_orders_count : int;
     checked_candidates_count     : int;

     current_rf_of_candidate      : int;
     total_rf_of_candidate        : int;

     current_mo_of_candidate      : int;
     total_mo_of_candidate        : int;

     current_sc_of_candidate      : int;
     total_sc_of_candidate        : int;

     current_lo_of_candidate: int;
     total_lo_of_candidate: int;

     current_tot_of_candidate: int;
     total_tot_of_candidate: int;

     instructions                 : instructions;

     results                      : (result * execution_check_result) list;
   }

let init_summ m = 
  {
   raw_candidates_count = 0;
   candidates_with_constraints_count = 0;
   candidates_with_orders_count = 0;
   checked_candidates_count = 0;

   current_rf_of_candidate = 0;
   total_rf_of_candidate = 0;

   current_mo_of_candidate = 0;
   total_mo_of_candidate = 0;

   current_sc_of_candidate = 0;
   total_sc_of_candidate = 0;

   current_lo_of_candidate = 0;
   total_lo_of_candidate = 0;

   current_tot_of_candidate = 0;
   total_tot_of_candidate = 0;

   instructions = empty_instructions m;

   results = []
 } 
