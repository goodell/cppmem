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

module A = Atomic
module T = Types
open Value
open Auxl
open Ids
open Printf


(* Mark: todo: one way or 'other *)
let collapse_vint v = match v with
  | Cmm.Flexible s -> raise (Failure ("can't convert unification variable to int"))
  | Cmm.Rigid (Cmm.Symbolic s) -> (try Cmm.Rigid (Cmm.Concrete (int_of_string s)) with Failure _ -> Cmm.Rigid (Cmm.Symbolic s))
  | Cmm.Rigid (Cmm.Concrete n) -> Cmm.Rigid (Cmm.Concrete n)


let list_of_set s = Pset.elements s
let set_of_list l = Pset.from_list compare l


(* let et_location_kinds  --- can't easily do this direction with the isa function type, but don't need it *)
let te_location_kinds lk l =
  try List.assoc l lk with Not_found -> Cmm.Non_Atomic
    

let eq_action_pair ep1 ep2 = (ep1 = ep2)
(*  match ep1,ep2 with
  | (e11,e12),(e21,e22) ->
      e11 = e21 && e12 = e22*)

(*let eq_action_paira = ({A.eq = eq_action_pair} : (A.action * A.action) A.eq)*)



let xoxw_of_exodexed exod exed =
  let xo =
    {Cmm.actions = set_of_list exod.T.actions;
     Cmm.threads = set_of_list exod.T.threads;
     Cmm.lk = te_location_kinds exod.T.lk;
     Cmm.sb = set_of_list exod.T.sb;
     Cmm.asw = set_of_list exod.T.asw;
     Cmm.dd = set_of_list exod.T.dd;
(*   Cmm.cd = set_of_list exod.T.cd;*)
    } in
  let xw =
    {Cmm.rf = set_of_list exed.T.rf;
     Cmm.mo = set_of_list exed.T.mo;
     Cmm.sc = set_of_list exed.T.sc;
     Cmm.lo = set_of_list exed.T.lo;
     Cmm.ao = set_of_list exed.T.ao;
     Cmm.tot = set_of_list exed.T.tot;
    } in
  (xo, xw)


let derive_data model exod exed = 
  let (xo, xw) = xoxw_of_exodexed exod exed in
  let exdd = model.Cmm.relation_calculation xo xw in
  let locations = Cmm.locations_of xo.Cmm.actions in
  let complete_execution = (xo, xw, exdd) in
  let derived_relations = List.map (fun (nm, rel) -> (nm, list_of_set rel)) exdd in
  let undefined_behaviour = List.map (function
    | Cmm.One (nm, f) -> (nm, T.One (list_of_set (f complete_execution)))
    | Cmm.Two (nm, f) -> (nm, T.Two (list_of_set (f complete_execution))))
    model.Cmm.undefined in
  let exdd = {
    T.locations = list_of_set locations;
    T.derived_relations = derived_relations;
    T.undefined_behaviour = undefined_behaviour;
  } in
  exdd


(*	let actual_exdd = derive_data exod exed in *)

let use_pred_tree leaf_f node_f = function
  | Cmm.Leaf check -> leaf_f check
  | Cmm.Node bs -> node_f bs

let rec check_pred_tree execution = function
  | Cmm.Leaf check -> T.Pred_leaf (check execution)
  | Cmm.Node bs ->
    T.Pred_node
      (List.map
         (fun (nm, t) ->
           (nm, check_pred_tree execution t))
         bs)

let rec pred_tree_for_all = function
  | T.Pred_leaf b -> b
  | T.Pred_node l -> List.for_all (fun (_, t) -> pred_tree_for_all t) l

let value_of_masked_tree = function
  | T.Masked_pred_leaf b -> b
  | T.Masked_dead_pred_leaf b -> b
  | T.Masked_pred_node (b, _) -> b
  | T.Masked_dead_pred_node (b, _) -> b

let rec mask_pred_tree ignorechecks kill = function
  | T.Pred_leaf b ->
    if kill then T.Masked_dead_pred_leaf b
    else T.Masked_pred_leaf b
  | T.Pred_node l ->
    let x =
      List.map
        (fun (nm, t) ->
          let kill = List.mem nm ignorechecks in
          let t2 = mask_pred_tree ignorechecks kill t in
          let b = value_of_masked_tree t2 in
          ((nm, t2), b || kill))
        l in
    let b = List.for_all (fun (_, b) -> b) x in
    let l2 = List.map fst x in
    if kill then T.Masked_dead_pred_node (b, l2)
    else T.Masked_pred_node (b, l2)

let mask_ecr ignorechecks ecr =
  let ignore_consistent = List.mem "consistent_execution" ignorechecks in
  let masked_ecr_pieces_of_consistent = mask_pred_tree ignorechecks ignore_consistent ecr.T.ecr_pieces_of_consistent in
  let masked_ecr_consistent_real = value_of_masked_tree masked_ecr_pieces_of_consistent in
  let masked_ecr_consistent_masked = ignore_consistent || masked_ecr_consistent_real in
  let masked_ecr_consistent_race_free = masked_ecr_consistent_masked && List.for_all (fun (nm, has) -> List.mem nm ignorechecks || not has) ecr.T.ecr_undefined_behaviour in
  {
    T.masked_ecr_pieces_of_consistent = masked_ecr_pieces_of_consistent;
    T.masked_ecr_undefined_behaviour = ecr.T.ecr_undefined_behaviour;
    T.masked_ecr_consistent_real = masked_ecr_consistent_real;
    T.masked_ecr_consistent_masked = masked_ecr_consistent_masked;
    T.masked_ecr_consistent_race_free = masked_ecr_consistent_race_free;
  }

let check_result model exwit =
  let (xo, xw) = xoxw_of_exodexed exwit.T.exod exwit.T.exed in
  let derived_relations = List.map (fun (nm, rel) ->
    (nm, set_of_list rel)) exwit.T.exdd.T.derived_relations in
  let complete_execution = (xo, xw, derived_relations) in
  let pieces_of_consistent = check_pred_tree complete_execution model.Cmm.consistent in
  let undefined_behaviour =
    List.map
      (fun (nm, fault) ->
        let has =
          match fault with
            | Types.One acts -> List.length acts > 0
            | Types.Two rel -> List.length rel > 0 in
        (nm, has))
      exwit.Types.exdd.Types.undefined_behaviour in
  let consistent_execution = pred_tree_for_all pieces_of_consistent in
  let consistent_race_free_execution = consistent_execution && List.for_all (fun (nm, has) -> not has) undefined_behaviour in
  {
    Types.ecr_pieces_of_consistent = pieces_of_consistent;
    Types.ecr_undefined_behaviour = undefined_behaviour;
  }


let names_of_derived_relations model =
  let exod = T.empty_execution_opsem_data in
  let exed = T.empty_execution_existential_data model in
  let (xo, xw) = xoxw_of_exodexed exod exed in
  List.map fst (model.Cmm.relation_calculation xo xw)

(* relations, not unary *)
let names_of_undefined_behaviour_relations model =
  option_map (function
    | Cmm.One _ -> None
    | Cmm.Two (nm, _) -> Some nm) model.Cmm.undefined
