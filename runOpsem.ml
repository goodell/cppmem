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
open Constraints
open Solver
open Types
open Iso
open Execfile
open Executions
open Eval


type ('a, 'b) interaction_handles = {
  interact_user_w_raw_candidate : cand_summaries -> execution_result -> ppmode -> unit;
  interact_user_w_preliminary_soln : cand_summaries -> execution_result -> substitution -> execution_result -> int -> ppmode -> unit;
  interact_user_w_rfmap : Cmm.memory_model -> cand_summaries -> execution_result -> execution_result -> action_rel -> (Cmm.cvalue * Cmm.cvalue) list -> ppmode -> unit;
  interact_user_w_rfmap_soln : Cmm.memory_model -> cand_summaries -> execution_result -> substitution -> execution_result -> action_rel -> int -> ppmode -> unit;
  interact_user_w_mo : cand_summaries -> execution_result -> execution_result -> action_rel -> action_rel -> int -> unit;
  interact_user_w_sc : cand_summaries -> execution_result -> execution_result -> action_rel -> action_rel -> action_rel -> unit;
  interact_user_checker : (Cmm.memory_model * string) -> masked_execution_check_result option -> cand_summaries -> execution_data -> instructions -> string option -> (instructions -> 'a) -> 'b;
  final_action : string option -> cand_summaries -> unit;
}

type collect =
  | Collect_only_consistent_rf
  | Collect_all

let proc_file (model, model_name) instrs (filename, cilf) ih collect =
  
  let proc_raw_candidate success_cont failure_cont res s_raw = 
    let s_raw_calc = 
      {s_raw with 
       raw_candidates_count = s_raw.raw_candidates_count + 1;
       current_rf_of_candidate = 0;
       total_rf_of_candidate = 0;
       current_mo_of_candidate = 0;
       total_mo_of_candidate = 0;
       current_sc_of_candidate = 0;
       total_sc_of_candidate = 0;
     } in

    let () = 
        ih.interact_user_w_raw_candidate s_raw_calc res (s_raw_calc.instructions.mode)
    in

    let proc_sub_first vcw_fst m_fst sub s_sub =
      let remaining_vconstraint = 
	vconstraint_of vcw_fst in
      let vconstraint_sub = 
	    subst_vconstraint sub remaining_vconstraint in
      let res_s = subst_execution_result sub res in
      let res_s_constraint = {res_s with global_vconstraint = vconstraint_sub} in
      let rfm_poss = get_rfm_poss res_s_constraint in

      let count_rf_poss = 
        List.fold_left 
          (fun k rf_poss -> k * (List.length rf_poss))
          1 rfm_poss in

      let s_sub_calc =
        {s_sub with 
         candidates_with_constraints_count = s_sub.candidates_with_constraints_count + 1;
         current_rf_of_candidate = 0;
         total_rf_of_candidate = count_rf_poss;
       } in

      let () = 
          ih.interact_user_w_preliminary_soln s_sub_calc res sub res_s_constraint count_rf_poss s_sub_calc.instructions.mode
      in

      let () = add_mark () in

      let proc_rfm rfcand s_rfm =
        let () = undo_until_mark () in
        let s_rfm_calc =
          {s_rfm with
           current_rf_of_candidate = 1 + s_rfm.current_rf_of_candidate;
         } in
	let opt_rf,new_eqss = List.split rfcand in
        let rf = option_map (fun (wopt,r) -> match wopt with None -> None | Some w -> Some (w,r)) opt_rf in
	let new_eqs = List.flatten new_eqss in

        let () = 
            ih.interact_user_w_rfmap model s_rfm_calc res res_s_constraint rf new_eqs s_rfm_calc.instructions.mode
        in

	let proc_sub_with_rfm vcw_rfm m_rfm sub_rfm s_sub_rfm =
	  let res_s_rfm = subst_execution_result sub_rfm res_s_constraint in
	  let remaining_vconstraint = 
	    vconstraint_of vcw_rfm in
	  let vconstraint_sub = 
	    subst_vconstraint sub_rfm remaining_vconstraint in
	  let rf_f = subst_action_reln sub_rfm rf in
	  let res_f = {res_s_rfm with global_vconstraint = vconstraint_sub} in
	  let locs,stores_per_loc =
            if model.Cmm.relation_flags.Cmm.mo_flag then List.split (writes_per_loc res_f)
            else [], [] in

          let count_mo_poss = 
            List.fold_left
              (fun k ss -> k * fact (List.length ss))
              1 stores_per_loc in

          let s_sub_rfm_calc =
            {s_sub_rfm with
             current_mo_of_candidate = 0;
             total_mo_of_candidate = count_mo_poss;
           } in

          let () = 
              ih.interact_user_w_rfmap_soln model s_sub_rfm_calc res sub_rfm res_f rf_f count_mo_poss s_sub_rfm.instructions.mode
          in

	  let proc_mo mos_list s_mo =
	    let rec list_to_total_order xs =
	      match xs with
	      | [] -> []
	      | x :: xs -> 
		  (List.map (fun y -> (x,y)) xs) @ list_to_total_order xs
	    in
	    let mo_rels = List.map list_to_total_order mos_list in
	    let mo_rel = List.flatten mo_rels in
	    let sc_actions =
              if model.Cmm.relation_flags.Cmm.sc_flag then
	        List.filter
		  (function 
		    | Cmm.Lock _ 
		    | Cmm.Unlock _ ->
                      (* Note: this is very tied to the models *)
                      (match model.Cmm.relation_flags.Cmm.lo_flag with None -> true | Some _ -> false)
		    | Cmm.Load (_,_,Cmm.Seq_cst,_,_) 
		    | Cmm.Store (_,_,Cmm.Seq_cst,_,_) 
		    | Cmm.RMW (_,_,Cmm.Seq_cst,_,_,_)
		    | Cmm.Fence (_,_,Cmm.Seq_cst) -> true
		    | _ -> false)
		  (real_actions_of res_f.my_actions)
              else [] in
            let count_sc_poss = fact (List.length sc_actions) in
            let s_mo_calc =
              {s_mo with
               current_mo_of_candidate = 1 + s_mo.current_mo_of_candidate;
               current_sc_of_candidate = 0;
               total_sc_of_candidate = count_sc_poss;
             } in

            let () =
                ih.interact_user_w_mo s_mo_calc res res_f rf_f mo_rel count_sc_poss
            in

	    let proc_sc sc_list s_sc =
	      let sc_rel = list_to_total_order sc_list in
              let lo_actions_sets =
                match model.Cmm.relation_flags.Cmm.lo_flag with
                  | Some order ->
                    let lo_actions =
                      List.filter
                        (function
                          | Cmm.Lock _ -> true
                          | Cmm.Unlock _ -> true
                          | _ -> false)
                        (real_actions_of res_f.my_actions) in
                    (match order with
                      | Cmm.Global_order -> [lo_actions]
                      | Cmm.Per_location_order ->
                        let locks =
                          remove_duplicates
                            (List.map (function
                              | Cmm.Lock (_,_,l,_) -> l
                              | Cmm.Unlock (_,_,l) -> l
                              | _ -> assert false) lo_actions) in
                        List.map
                          (fun l ->
                            List.filter
                              (function
                                | Cmm.Lock (_,_,l2,_) -> l = l2
                                | Cmm.Unlock (_,_,l2) -> l = l2
                                | _ -> false)
                              lo_actions)
                          locks)
                  | None -> [[]] in
              let count_lo_poss = List.fold_left (fun acc s -> acc * fact (List.length s)) 1 lo_actions_sets in

              let s_sc_calc = 
                {s_sc with
                 current_sc_of_candidate = 1 + s_sc.current_sc_of_candidate;
                  current_lo_of_candidate = 0;
                  total_lo_of_candidate = count_lo_poss;
                } in
              let () = 
                  ih.interact_user_w_sc s_sc_calc res res_f rf_f mo_rel sc_rel
              in

              let proc_lo lo_list s_sc =
                let lo_rel = List.flatten (List.map list_to_total_order lo_list) in
                let tot_actions = if model.Cmm.relation_flags.Cmm.tot_flag then real_actions_of res_f.my_actions else [] in
                let count_tot_poss = fact (List.length tot_actions) in

                let s_lo_calc = { s_sc with
                  current_lo_of_candidate = 1 + s_sc.current_lo_of_candidate;
                  current_tot_of_candidate = 0;
                  total_tot_of_candidate = count_tot_poss;
                } in

                let proc_tot tot_list s_sc =
                  let tot_rel = list_to_total_order tot_list in
                  let s_tot_calc = { s_sc with
                    current_tot_of_candidate = 1 + s_sc.current_tot_of_candidate;
                  } in

                  (* TODO: jp: ao needs to be enumerated (if it is activated) *)

                  let exed = { rf = rf_f; mo = mo_rel; sc = sc_rel; lo = lo_rel; ao = []; tot = tot_rel; } in
	          success_cont res_f exed s_tot_calc
                in
                
                fold_all_perms tot_actions proc_tot s_lo_calc in
              fold_all_perms_sets lo_actions_sets proc_lo s_sc_calc in
	    fold_all_perms sc_actions proc_sc s_mo_calc in
	  fold_perms_cross stores_per_loc proc_mo s_sub_rfm_calc
	in 
	let rfm_failure_cont msg summ =
	  let () =
	    if (!Globals.quietmode) then () 
	    else Printf.printf "Postulated RF-map creates a contradiction\n" in
	  failure_cont msg summ
	in
	solve_more_eqs vcw_fst m_fst new_eqs proc_sub_with_rfm rfm_failure_cont s_rfm_calc
      in
      fold_cross rfm_poss proc_rfm s_sub_calc
    in
    solve res.global_vconstraint proc_sub_first failure_cont s_raw_calc 
  in

  let success_fn res exed s =
    let res_asw = make_addtnl_syncwiths res in
    let s_calc = 
      {s with 
       candidates_with_orders_count = 1 + s.candidates_with_orders_count;} in
    let exod = conv_execution_result res_asw in
    let exdd = derive_data model exod exed in
    let exwit_conv = Pp.rename_exwit {exod=exod;exed=exed;exdd=exdd} s_calc.instructions.mode in
    let continuation2 instrs = 
      begin
        let ecr = Iso.check_result model exwit_conv in
        let checkresult = (Iso.mask_ecr instrs.ignorechecks ecr).Types.masked_ecr_consistent_race_free in
        let checked = checkresult && matches_exwit exwit_conv s_calc.instructions.constrain_rels in
        let collect_anyway = match collect with Collect_all -> true | Collect_only_consistent_rf -> false in
        let s_final = 
          if checked || collect_anyway
          then 
            let actual = exwit_conv in
            let cutdown = {exod=filter_out_exod exwit_conv.exod instrs;
                           exed=filter_out_exed exwit_conv.exed instrs;
                           exdd=filter_out_exdd exwit_conv.exdd instrs;} in
            {s_calc with checked_candidates_count = (if checked then 1 else 0) + s_calc.checked_candidates_count;
              results = ({actual = actual;cutdown_for_drawing=cutdown}, ecr) :: s_calc.results;
            }
          else s_calc 
        in
        let continuation1 instrs =
          {s_final with instructions = instrs} in
        let mecr = Iso.mask_ecr instrs.ignorechecks ecr in
        ih.interact_user_checker (model, model_name) (Some mecr) s_final exwit_conv instrs filename continuation1
        
      end in 
    ih.interact_user_checker (model, model_name) None s_calc exwit_conv s_calc.instructions filename continuation2
  in 

  let failure_fn m s = s in

  let summ = 
    eval_file cilf 
      (proc_raw_candidate success_fn failure_fn) 
      {(init_summ ppmode_default_tex) with instructions = instrs} in

  ih.final_action filename summ;
  
  summ
