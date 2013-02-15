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

(**
 * A symbolic evaluator for programs in a cut-down pseudo-C++0x, which
 * generates dependencies and other data for checking memory model conditions
 *)

open Ids
open Value
open Constraints
open Iso
open Types
open Cabs
open Error
open Globals
open Auxl
open Execfile
open Solver
open Executions

(*module E = Extracted*)
module T = Types

let is_local_var v = try v.[0] = 'r' with Invalid_argument _ -> raise (Failure "empty variable")

let make_loc_from_exp_result (res_e:execution_result) : vloc =
  match res_e.result with
  | LVal (v,_) -> v
  | Nothing -> internal_error "No return value from exp"
  | RVal v -> v

let load_from_exp (env:env) (res_l:execution_result) : execution_result =
  let vl,copt = 
    match res_l.result with
    | LVal (v,copt) -> v,copt
    | RVal v -> internal_error "Cannot load from non-location" 
    | Nothing -> internal_error "No return value from exp" in
  let v = 
    match copt with
    | None -> fresh_var () 
    | Some c -> c in
  let new_action_load = 
    Cmm.Load (genaction_id (),env.thread_id,Cmm.NA,vl,v) in
  let ddep_sources = filter_ddep_sources res_l.my_actions in
  let new_ddep_load = cross_reln ddep_sources [new_action_load] in
  let sdep_acts = filter_s_or_cdep_targets res_l.my_actions in
  let new_sdep_load = cross_reln sdep_acts
      [RealAction new_action_load] in
  {res_l with
   my_actions = (RealAction new_action_load) :: res_l.my_actions;
   data_deps = new_ddep_load @ res_l.data_deps;
   seq_deps = new_sdep_load @ res_l.seq_deps;
   result = RVal v;
 } 
  
(**
 * Evaluation functions proper
 *)
let rec eval_exp 
    (env:env) (e:expression) (as_rval:bool)
    (success_cont: execution_result -> 'a -> 'a) 
    (summary : 'a) 
    : 'a = 
  match e with
  | NOTHING -> success_cont unit_execution_result summary
  | ATOMIC_CAS_WEAK (e1,e2,e3,mo1,mo2) ->
      let proc_exp1 res_e1 s_e1 = 
        let proc_exp2 res_e2 s_e2 = 
          let proc_exp3 res_e3 s_e3 = 
            let vl1 = make_loc_from_exp_result res_e1 in
	    let vnow = fresh_var () in
            let vexpected = 
		match res_e2.result with
		| RVal v -> v
		| _ -> internal_error "Cannot happen" in 
            let vdesired = 
		match res_e3.result with
		| RVal v -> v
		| _ -> internal_error "Cannot happen" in 
            let new_action_load = 
	      Cmm.Load (genaction_id (),env.thread_id,conv_mo mo2,vl1,vnow) in
            let new_action_cas = 
	      Cmm.RMW (genaction_id (),env.thread_id,conv_mo mo1,vl1,vexpected,vdesired) in
            let res_concat1 = concat res_e1 res_e2 in 
            let res_concat = concat res_concat1 res_e3 in
            let ddep_sources = filter_ddep_sources res_concat.my_actions in
	    let sdep_acts = filter_s_or_cdep_targets res_concat.my_actions in
 	    let new_ddep_load = cross_reln ddep_sources [new_action_load] in
	    let new_sdep_load = cross_reln sdep_acts [RealAction new_action_load] in
            let res_fail = 
	      {res_concat with
	       my_actions = (RealAction new_action_load) :: res_concat.my_actions;
	       data_deps = new_ddep_load @ res_concat.data_deps;
	       seq_deps = new_sdep_load @ res_concat.seq_deps;
	       result = RVal vnow;
	     } in
 	    let new_ddep_cas = cross_reln ddep_sources [new_action_cas] in
	    let new_sdep_cas = cross_reln sdep_acts [RealAction new_action_cas] in
            let res_succ = 
	      {res_concat with
	       my_actions = (RealAction new_action_cas) :: res_concat.my_actions;
	       data_deps = new_ddep_cas @ res_concat.data_deps;
	       seq_deps = new_sdep_cas @ res_concat.seq_deps;
	       result = RVal vdesired;
	     } in
            let s_fail = success_cont res_fail s_e3 in
  	    success_cont res_succ s_fail 
          in
          eval_exp env e3 true proc_exp3 s_e2
        in
        eval_exp env e2 true proc_exp2 s_e1          
      in
      eval_exp env e1 false proc_exp1 summary
  | ATOMIC_CAS_STRONG (e1,e2,e3,mo1,mo2) ->
      let proc_exp1 res_e1 s_e1 = 
        let proc_exp2 res_e2 s_e2 = 
          let proc_exp3 res_e3 s_e3 = 
            let vl1 = make_loc_from_exp_result res_e1 in
	    let vnow = fresh_var () in
            let vexpected = 
		match res_e2.result with
		| RVal v -> v
		| _ -> internal_error "Cannot happen" in 
            let vdesired = 
		match res_e3.result with
		| RVal v -> v
		| _ -> internal_error "Cannot happen" in 
            let new_action_load = 
	      Cmm.Load (genaction_id (),env.thread_id,conv_mo mo2,vl1,vnow) in
            let new_action_cas = 
	      Cmm.RMW (genaction_id (),env.thread_id,conv_mo mo1,vl1,vexpected,vdesired) in
            let new_action_block =
              Cmm.Blocked_rmw (genaction_id (),env.thread_id,vl1) in
            let res_concat1 = concat res_e1 res_e2 in
            let res_concat = concat res_concat1 res_e3 in
            let ddep_sources = filter_ddep_sources res_concat.my_actions in
	    let sdep_acts = filter_s_or_cdep_targets res_concat.my_actions in
 	    let new_ddep_load = cross_reln ddep_sources [new_action_load] in
	    let new_sdep_load = cross_reln sdep_acts [RealAction new_action_load] in
  	    let new_fail_vconstraint = 
	      match res_e3.global_vconstraint with
	      | Conj cs -> Conj (C (Neq (vnow,vexpected)) :: cs) in
            let res_fail = 
	      {res_concat with
               global_vconstraint = new_fail_vconstraint;
	       my_actions = (RealAction new_action_load) :: res_concat.my_actions;
	       data_deps = new_ddep_load @ res_concat.data_deps;
	       seq_deps = new_sdep_load @ res_concat.seq_deps;
	       result = RVal vnow;
	     } in
 	    let new_ddep_cas = cross_reln ddep_sources [new_action_cas] in
	    let new_sdep_cas = cross_reln sdep_acts [RealAction new_action_cas] in
            let res_succ = 
	      {res_concat with
	       my_actions = (RealAction new_action_cas) :: res_concat.my_actions;
	       data_deps = new_ddep_cas @ res_concat.data_deps;
	       seq_deps = new_sdep_cas @ res_concat.seq_deps;
	       result = RVal vdesired;
	     } in

 	    let new_ddep_block = cross_reln ddep_sources [new_action_block] in
	    let new_sdep_block = cross_reln sdep_acts [RealAction new_action_block] in
            let res_block = 
	      {res_concat with
	       my_actions = (RealAction new_action_block) :: res_concat.my_actions;
	       data_deps = new_ddep_block @ res_concat.data_deps;
	       seq_deps = new_sdep_block @ res_concat.seq_deps;
	       result = RVal vnow;
	     } in
            let s_block = success_cont res_block s_e3 in
            let s_fail = success_cont res_fail s_block in
  	    success_cont res_succ s_fail 
          in
          eval_exp env e3 true proc_exp3 s_e2
        in
        eval_exp env e2 true proc_exp2 s_e1          
      in
      eval_exp env e1 false proc_exp1 summary
  | ATOMIC_LOAD (e1,mo,copt) ->
      let proc_exp res_e1 s_e1 = 
	let vl = make_loc_from_exp_result res_e1 in
	let proc_v v res_e2 s_e2 = 
	  let new_action_load = 
	    Cmm.Load (genaction_id (),env.thread_id,conv_mo mo,vl,v) in
	  let ddep_sources = filter_ddep_sources res_e2.my_actions in
	  let new_ddep_load = cross_reln ddep_sources [new_action_load] in
	  let sdep_acts = filter_s_or_cdep_targets res_e2.my_actions in
	  let new_sdep_load = cross_reln sdep_acts
	      [RealAction new_action_load] in
	  success_cont 
	    {res_e2 with
	     my_actions = (RealAction new_action_load) :: res_e2.my_actions;
	     data_deps = new_ddep_load @ res_e2.data_deps;
	     seq_deps = new_sdep_load @ res_e1.seq_deps;
	     result = RVal v;
	   } s_e2 in
	match copt with
	| None -> 
	    let v = fresh_var () in
	    proc_v v res_e1 s_e1
	| Some e ->
	    let proc_eread res_e2 s_e2 =
	      let res_e2n = 
		match res_e2.result with
		| LVal _ -> load_from_exp env res_e2 
		| RVal _ -> res_e2
		| Nothing -> internal_error "No return value from exp" in
              let ve = 
		match res_e2n.result with
		| RVal v -> v
		| _ -> internal_error "Cannot happen" in 
	      let res_en = concat res_e1 res_e2n in
	      proc_v ve res_en s_e2 in
	    eval_exp env e false proc_eread s_e1
      in
      eval_exp env e1 false proc_exp summary
  | UNARY (ADDROF,e1) -> 
      let proc_e1 res_e1 s_e1 = 
	let res_e1n = 
          match res_e1.result with
          | LVal (v,copt) -> {res_e1 with result = RVal v}
          | RVal v -> internal_error "Cannot take address of non-location"
          | Nothing -> internal_error "No return value from exp" in
	success_cont res_e1n s_e1 in
      eval_exp env e1 false proc_e1 summary
  | UNARY (MEMOF,e1) ->
      let proc_e1 res_e1 s_e1 = 
	let res_e1n = 
          match res_e1.result with
          | LVal (v,copt) -> load_from_exp env res_e1
          | RVal v -> res_e1
          | Nothing -> internal_error "No return value from exp" in
        let vl = 
          match res_e1n.result with
          | LVal (v,copt) -> internal_error "Location value malformed" 
          | RVal v -> v
          | Nothing -> internal_error "No return value from exp" in
(*        let v = fresh_var () in 
        let new_action_load = 
          Cmm.Load (genaction_id (),env.thread_id,Cmm.NA,vl,v) in
        let ddep_sources = filter_ddep_sources res_e1n.my_actions in
        let new_ddep_load = cross_reln ddep_sources [new_action_load] in
        let sdep_acts = filter_s_or_cdep_targets res_e1n.my_actions in
        let new_sdep_load = cross_reln sdep_acts
            [RealAction new_action_load] in *)
        let res_e1l = 
          {res_e1n with
(*           my_actions = (RealAction new_action_load) :: res_e1n.my_actions;
           data_deps = new_ddep_load @ res_e1n.data_deps;
           seq_deps = new_sdep_load @ res_e1n.seq_deps; *)
           result = LVal (vl,None);
         } in
	success_cont res_e1l s_e1 in
      eval_exp env e1 false proc_e1 summary
  | UNARY (op,e1) -> 
      let proc_e1 res_e1 s_e1 = 
	let res_e1n = 
          match res_e1.result with
          | LVal _ -> load_from_exp env res_e1 
          | RVal _ -> res_e1
          | Nothing -> internal_error "No return value from exp" in
        let v1 =
          match res_e1n.result with
          | RVal v1 -> v1
          | _ -> internal_error "Cannot happen" in
	let vnew,cnew = newv_un_op op v1 in
	let new_global_vconstraint = 
	  match res_e1n.global_vconstraint with
	  | Conj cs -> Conj (C cnew :: cs) in
	let res_e = 
	  {res_e1n with
	   global_vconstraint = new_global_vconstraint;
	   result = RVal vnew;
	 } in
	success_cont res_e s_e1 in
      eval_exp env e1 false proc_e1 summary
  | LABELADDR _ -> cannot_handle "label address expressions"
  | BINARY (ASSIGN,e1,e2) ->
      (match e1 with
      (* special case assignments rXX = foo to not generate a write event *)
      | VARIABLE (vs,copt) when is_local_var vs ->
          eval_exp env e2 true success_cont summary 
      | _ -> 
          let proc_e1 res_e1 s_e1 = 
	    let proc_e2 res_e2 s_e2 =
	      let res_e2n = 
                match res_e2.result with
                | LVal _ -> load_from_exp env res_e2 
                | RVal _ -> res_e2
                | Nothing -> internal_error "No return value from exp" in
              let ve = 
                match res_e2n.result with
                | RVal v -> v
                | _ -> internal_error "Cannot happen" in 
              let vl = make_loc_from_exp_result res_e1 in
	      let new_action_store = 
	        Cmm.Store (genaction_id (),env.thread_id,Cmm.NA,vl,ve) in
	      let res_concat = concat res_e1 res_e2n in
	      let ddep_sources_store = filter_ddep_sources res_concat.my_actions in
	      let new_ddep_store = cross_reln ddep_sources_store [new_action_store] in
	      let sdep_acts_store = filter_s_or_cdep_targets res_concat.my_actions in
	      let new_sdep_store = cross_reln sdep_acts_store [RealAction new_action_store] in
	      let res_s = 
	        {res_concat with
	         my_actions = (RealAction new_action_store) :: res_concat.my_actions;
	         data_deps = new_ddep_store @ res_concat.data_deps;
	         seq_deps = new_sdep_store @ res_concat.seq_deps;
	         result = RVal ve;
	       } in
	      success_cont res_s s_e2 in
	    eval_exp env e2 false proc_e2 s_e1 in
          eval_exp env e1 false proc_e1 summary)
  | BINARY (op,e1,e2) -> 
      let proc_e1 res_e1 s_e1 = 
	let proc_e2 res_e2 s_e2 =
	  let res_e1n = 
            match res_e1.result with
            | LVal _ -> load_from_exp env res_e1 
            | RVal _ -> res_e1
            | Nothing -> internal_error "No return value from exp" in
	  let res_e2n = 
            match res_e2.result with
            | LVal _ -> load_from_exp env res_e2 
            | RVal _ -> res_e2
            | Nothing -> internal_error "No return value from exp" in
          let v1,v2 =
            match res_e1n.result,res_e2n.result with
            | RVal v1,RVal v2 -> v1,v2
            | _ -> internal_error "Cannot happen" in
	  let vnew,cnew = newv_bin_op op v1 v2 in
	  let res_concat = concat res_e1n res_e2n in
	  let new_global_vconstraint = 
	    match res_concat.global_vconstraint with
	    | Conj cs -> Conj (C cnew :: cs) in
	  let res_e = 
	    {res_concat with
	     global_vconstraint = new_global_vconstraint;
	     result = RVal vnew;
	   } in
	  success_cont res_e s_e2 in
	eval_exp env e2 false proc_e2 s_e1 in
      eval_exp env e1 false proc_e1 summary
  | QUESTION (e1,e2,e3) ->
      let proc_e1 res_e1 s_e1 =
	let res_e1n =
	  match res_e1.result with
	  | LVal _ -> load_from_exp env res_e1 
	  | RVal _ -> res_e1
	  | Nothing -> internal_error "No return value from exp" in
	let v1 = 
	  match res_e1n.result with
	  | RVal v -> v
	  | _ -> internal_error "Cannot happen" in
	let success_efalse res_e3 s_e3 =
	  let res_seq = sequence_with_cdeps res_e1n res_e3 in
	  let new_global_vconstraint =
		  match res_seq.global_vconstraint with
		  | Conj cs -> Conj (C (IsFalseC v1) :: cs) in
	  let res_f = {res_seq with global_vconstraint = new_global_vconstraint} in
	  success_cont res_f s_e3 in
	
	let success_etrue res_e2 s_e2 =
	  let res_seq = sequence_with_cdeps res_e1n res_e2 in
	  let new_global_vconstraint =
	    match res_seq.global_vconstraint with
	    | Conj cs -> Conj (C (IsTrueC v1) :: cs) in
	  let res_t = {res_seq with global_vconstraint = new_global_vconstraint} in
	  success_cont res_t s_e2 in
	
        let s_false = eval_exp env e3 as_rval success_efalse s_e1 in
	eval_exp env e2 as_rval success_etrue s_false
      in
      eval_exp env e1 false proc_e1 summary 
  | CAST (_,ie) -> (* Assume type-correct ! *)
      eval_init_exp env ie success_cont summary
  | CALL (VARIABLE ("printf",_),el) -> (* special case *)
      eval_exps env el true success_cont summary
  | CALL (VARIABLE (f,_),el) ->
      let fname,fbody = find_fun_by_name env.current_file f in
      eval_fun env fname el fbody success_cont summary
  | CALL _ -> cannot_handle "Calling calculated function"
  | COMMA el -> eval_exps env el as_rval success_cont summary
  | CONSTANT c -> 
      success_cont 
	{unit_execution_result with result = RVal (make_val_from_const c)} 
	summary
  | PAREN e1 ->
      eval_exp env e1 as_rval success_cont summary 
  | VARIABLE (vs,copt) -> 
      if  is_local_var vs then cannot_handle "Using local variable in a context other than the lhs of an assignment";
      let vloc = lookup env.var_loc_map vs in
      begin
	match copt with 
	| None ->
	    let res = {unit_execution_result with result = LVal (vloc,None)} in
	    let res_n = 
              if as_rval then
		load_from_exp env res
              else res in
	    success_cont res_n summary
	| Some e1 ->
	    let proc_e1 res_e1 s_e1 =
	      let res_e1n = 
		match res_e1.result with
		| LVal _ -> load_from_exp env res_e1 
		| RVal _ -> res_e1
		| Nothing -> internal_error "No return value from exp" in
              let ve = 
		match res_e1n.result with
		| RVal v -> v
		| _ -> internal_error "Cannot happen" in 
	      let res = {res_e1n with result = LVal (vloc,Some ve)} in
	      let res_n = 
		if as_rval then
		  load_from_exp env res
		else res in
	      success_cont res_n s_e1
	    in
	    eval_exp env e1 true proc_e1 summary
      end
  | EXPR_SIZEOF _ -> cannot_handle "Size Of expressions"
  | TYPE_SIZEOF _ -> cannot_handle "Size Of expressions"
  | EXPR_ALIGNOF _ -> cannot_handle "Align Of expressions"
  | TYPE_ALIGNOF _ -> cannot_handle "Align Of expressions"
  | INDEX _ -> cannot_handle "Index expressions"
  | MEMBEROF _ -> cannot_handle "'Member of' expressions"
  | MEMBEROFPTR _ -> cannot_handle "'Member of ptr' expressions"
  | GNU_BODY _ -> cannot_handle "GNU body expressions"
  | EXPR_PATTERN _ -> cannot_handle "pattern variable expressions"

and eval_exps
    (env:env) (es:expression list) (as_rval : bool)
    (success_cont: execution_result -> 'a -> 'a) 
    (summary : 'a) 
    : 'a = 
  match es with
  | [] -> success_cont unit_execution_result summary (* Fallthrough *)
  | e :: es ->
      combine (eval_exp env e as_rval) (eval_exps env es as_rval) sequence success_cont summary

and eval_stmts 
    (env:env) (stmts:statement list) 
    (success_cont: execution_result -> 'a -> 'a) 
    (summary : 'a) : 'a =
  match stmts with
  | [] -> success_cont unit_execution_result summary (* Fallthrough *)
  | s :: ss ->
      begin
	match s with
	| NOP _ -> eval_stmts env ss success_cont summary
	| COMPUTATION (e,_) -> 
	    combine (eval_exp env e true) (eval_stmts env ss) sequence 
	      success_cont summary
	| BLOCK (b,_) ->
	    combine (eval_block env b) (eval_stmts env ss) sequence
	      success_cont summary
	| SEQUENCE (s1,s2,_) ->
	    eval_stmts env (s1 :: s2 :: ss) success_cont summary
	| IF (e,strue,sfalse,l) -> 
	    let proc_exp res_e s_e =
	      let res_en =
		match res_e.result with
		| LVal _ -> load_from_exp env res_e 
		| RVal _ -> res_e
		| Nothing -> internal_error "No return value from exp" in
	      let v = 
		match res_en.result with
		| RVal v -> v
		| _ -> internal_error "Cannot happen" in
	      let truebranch = strue :: ss in
	      let falsebranch = sfalse :: ss in
	      let success_bfalse res_bf s_bf =
		let res_seq = sequence_with_cdeps res_en res_bf in
		let new_global_vconstraint =
		  match res_seq.global_vconstraint with
		  | Conj cs -> Conj (C (IsFalseC v) :: cs) in
		let res_f = {res_seq with global_vconstraint = new_global_vconstraint} in
		success_cont res_f s_bf in
	      
	      let success_btrue res_bt s_bt =
		let res_seq = sequence_with_cdeps res_en res_bt in
		let new_global_vconstraint =
		  match res_seq.global_vconstraint with
		  | Conj cs -> Conj (C (IsTrueC v) :: cs) in
		let res_t = {res_seq with global_vconstraint = new_global_vconstraint} in
		success_cont res_t s_bt in
	      
              let s_false = eval_stmts env falsebranch success_bfalse s_e in
	      eval_stmts env truebranch success_btrue s_false
	    in
	    eval_exp env e false proc_exp summary 
        | WHILE (e,s,_) ->
            let proc_exp res_e s_e =
	      let res_en =
		match res_e.result with
		| LVal _ -> load_from_exp env res_e 
		| RVal _ -> res_e
		| Nothing -> internal_error "No return value from exp" in
	      let v = 
		match res_en.result with
		| RVal v -> v
		| _ -> internal_error "Cannot happen" in
              let proc_ss res_ss s_ss =
                let res_seq = sequence res_en res_ss in
		let new_global_vconstraint =
		  match res_seq.global_vconstraint with
		  | Conj cs -> Conj (C (IsFalseC v) :: cs) in
                (* Only sequence with the false branch *)
		let res_f = {res_seq with global_vconstraint = new_global_vconstraint} in
                success_cont res_f s_ss in
              eval_stmts env ss proc_ss s_e in
            eval_exp env e false proc_exp summary
        | DOWHILE _ -> cannot_handle "do-while loops"
        | FOR _ -> cannot_handle "for loops"
        | BREAK _ -> cannot_handle "break statements"
        | LOCK (lv,l) ->
	    let vloc = lookup env.var_loc_map lv in
	    let v = fresh_var () in 
	    let new_action_load = Cmm.Load (genaction_id (),env.thread_id,Cmm.NA,vloc,v) in
	    let new_action_lock_success = Cmm.Lock (genaction_id (),env.thread_id,v,Cmm.Locked) in
	    let new_sdep_lock = 
	    cross_reln [RealAction new_action_load] [RealAction new_action_lock_success] in
	    let res_s = 
	      {unit_execution_result with
	       my_actions = [(RealAction new_action_load);
			     (RealAction new_action_lock_success)];
	       seq_deps = new_sdep_lock;
	     } in
	    let new_action_lock_block = Cmm.Lock (genaction_id (),env.thread_id,v,Cmm.Blocked) in
	    let new_sdep_lock_block = 
	    cross_reln [RealAction new_action_load] [RealAction new_action_lock_block] in
	    let res_sb = 
	      {unit_execution_result with
	       my_actions = [(RealAction new_action_load);
			     (RealAction new_action_lock_block)];
	       seq_deps = new_sdep_lock_block;
	     } in
	    let proc_ss res_ss s_ss = 
              (* ignore res_ss after a blocked lock *)
              let s_sblock = success_cont res_sb s_ss in
	      let res = sequence res_s res_ss in
	      success_cont res s_sblock in
	    eval_stmts env ss proc_ss summary
        | UNLOCK (lv,l) ->
	    let vloc = lookup env.var_loc_map lv in
	    let v = fresh_var () in 
	    let new_action_load = Cmm.Load (genaction_id (),env.thread_id,Cmm.NA,vloc,v) in
	    let new_action_unlock = Cmm.Unlock (genaction_id (),env.thread_id,v) in
	    let new_sdep_unlock = 
	      cross_reln [RealAction new_action_load] [RealAction new_action_unlock] in
	    let res_s = 
	      {unit_execution_result with
	       my_actions = [(RealAction new_action_load);
			     (RealAction new_action_unlock)];
	       seq_deps = new_sdep_unlock;
	     } in
	    let proc_ss res_ss s_ss = 
	      let res = sequence res_s res_ss in
	      success_cont res s_ss in
	    eval_stmts env ss proc_ss summary
        | ATOMIC_FENCE (mo,l) ->
	    let new_action = Cmm.Fence (genaction_id (),env.thread_id,conv_mo mo) in
	    let res_f = 
	      {unit_execution_result with
	       my_actions = [RealAction new_action];} in
	    let proc_ss res_ss s_ss = 
	      let res = sequence res_f res_ss in
	      success_cont res s_ss in
	    eval_stmts env ss proc_ss summary
        | ATOMIC_STORE (e1,e2,mo,l) ->
	    let proc_e1 res_e1 s_e1 = 
	      let proc_e2 res_e2 s_e2 = 
	        let vl = make_loc_from_exp_result res_e1 in
	        let res_e2n = 
		  match res_e2.result with
		  | LVal _ -> load_from_exp env res_e2 
		  | RVal _ -> res_e2
		  | Nothing -> internal_error "No result value from exp" in
	        let ve = 
		  match res_e2n.result with
		  | RVal v -> v
		  | _ -> internal_error "Cannot happen" in
	        let new_action_store = 
		  Cmm.Store (genaction_id (),env.thread_id,conv_mo mo,vl,ve) in
	        let res_concat = concat res_e1 res_e2n in
	        let ddep_sources_store = filter_ddep_sources res_concat.my_actions in
	        let new_ddep_store = cross_reln ddep_sources_store [new_action_store] in
	        let sdep_acts_store = filter_s_or_cdep_targets res_concat.my_actions in
	        let new_sdep_store = cross_reln sdep_acts_store [RealAction new_action_store] in
	        let res_s = 
		  {res_concat with
		   my_actions = (RealAction new_action_store) :: res_concat.my_actions;
		   data_deps = new_ddep_store @ res_concat.data_deps;
		   seq_deps = new_sdep_store @ res_concat.seq_deps;
		   result = Nothing;
	         } in
	        let proc_ss res_ss s_ss = 
		  let res = sequence res_s res_ss in
		  success_cont res s_ss in
	        eval_stmts env ss proc_ss s_e2
	      in
	      eval_exp env e2 false proc_e2 s_e1 in
	    eval_exp env e1 false proc_e1 summary
        | MYTHREAD (lthread,lfunc,eargs,loc) ->
	    let new_tid = genthread_id () in
            let new_action_create = ThreadCreate (genaction_id (),env.thread_id,new_tid) in
            let vloc = fresh_var_named lthread in
            let env_n = add_new_var lthread (Cmm.Rigid vloc) env in
            let new_action_store = Cmm.Store (genaction_id (),env.thread_id,Cmm.NA,Cmm.Rigid vloc,new_tid) in
	    let new_sdep = [(new_action_create,RealAction new_action_store)] in
	    let res_s = 
	      {unit_execution_result with 
	       new_threads = 
	       [{env = {env with thread_id = new_tid};
		 stmt = COMPUTATION (CALL (VARIABLE (lfunc,None),eargs),loc)}];
               loc_kind_map = [(Cmm.Rigid vloc,Cmm.Non_Atomic)];
	       my_actions = [new_action_create ; 
			     RealAction new_action_store];
	       seq_deps = new_sdep;
	     } in
	    let proc_ss res_ss s_ss = 
	      let res = sequence res_s res_ss in
	      success_cont res s_ss in
	    eval_stmts env_n ss proc_ss summary
        | JOIN (lthread,l) -> 
	    let vloc = lookup env.var_loc_map lthread in
	    let v = fresh_var () in 
	    let new_action_load = Cmm.Load (genaction_id (),env.thread_id,Cmm.NA,vloc,v) in
            let new_action_join = ThreadJoin (genaction_id (),env.thread_id, v) in
	    let new_sdep_join = 
	      cross_reln [RealAction new_action_load] [new_action_join] in
	    let res_s = 
	      {unit_execution_result with
	       my_actions = [RealAction new_action_load;new_action_join];
	       seq_deps = new_sdep_join;
	     } in
	    let proc_ss res_ss s_ss = 
	      let res = sequence res_s res_ss in
	      success_cont res s_ss in
	    eval_stmts env ss proc_ss summary
        | PAR (stmts,loc) ->
	    let def = unit_execution_result in
	    let res_s = 
	      List.fold_left 
	        (fun res stmt ->
		  let new_tid = genthread_id () in
		  let new_action_create = ThreadCreate (genaction_id (),env.thread_id, new_tid) in
		  let new_action_join = ThreadJoin (genaction_id (),env.thread_id, new_tid) in
		  let new_sdep = (new_action_create,new_action_join) in
		  {res with
		   new_threads = {env = {env with thread_id = new_tid};
				  stmt = stmt} :: res.new_threads;
		   my_actions = 
		   new_action_create :: 
		   new_action_join :: res.my_actions;
		   seq_deps = new_sdep :: res.seq_deps})
	        def stmts in
	    let proc_ss res_ss s_ss = 
	      let res = sequence res_s res_ss in
	      success_cont res s_ss in
	    eval_stmts env ss proc_ss summary
        | CONTINUE _ -> cannot_handle "continue statements"
        | RETURN (e,_) ->
	    eval_exp env e true success_cont summary
        | SWITCH _ -> cannot_handle "switch statements"
        | CASE _ -> cannot_handle "case statements"
        | CASERANGE _ -> cannot_handle "case statements"
        | DEFAULT _ -> cannot_handle "default statements"
        | LABEL _ -> cannot_handle "label statements"
        | GOTO _ -> cannot_handle "goto statements"
        | COMPGOTO _ -> cannot_handle "computed gotos"
        | DEFINITION d -> 
	    let proc_d env_d res_d s_d =
	      let proc_ss res_ss s_ss = 
	        let res = sequence res_d res_ss in
	        success_cont res s_ss in
	      eval_stmts env_d ss proc_ss s_d in
	    eval_definition env d proc_d summary
        | ASM _ -> cannot_handle "inline assembly"
        | TRY_EXCEPT _ -> cannot_handle "TryExcept blocks"
        | TRY_FINALLY _ -> cannot_handle "TryFinally blocks"
      end

and eval_block 
    (env:env) (blk:block) 
    (success_cont: execution_result -> 'a -> 'a) 
    (summary : 'a) 
    : 'a = 
  eval_stmts env blk.bstmts success_cont summary

and eval_definition
    (env:env) (d:definition) 
    (success_cont: env -> execution_result -> 'a -> 'a) 
    (summary : 'a) 
    : 'a = 
  match d with
  | FUNDEF _ -> (* no local functions, hence handled already *)
      success_cont env unit_execution_result summary
  | DECDEF (ig,_) -> 
      eval_init_name_group env ig success_cont summary
  | TYPEDEF _ -> cannot_handle "typedef's"
  | ONLYTYPEDEF _ -> cannot_handle "typedef's"
  | GLOBASM _ -> cannot_handle "global asm"
  | PRAGMA _ -> cannot_handle "pragmas"
  | LINKAGE _ -> cannot_handle "external linkages"
  | TRANSFORMER _ -> cannot_handle "form transformers"
  | EXPRTRANSFORMER _ -> cannot_handle "expression transformers" 

and eval_init_name_group
    (env:env) (d:init_name_group) 
    (success_cont: env -> execution_result -> 'a -> 'a) 
    (summary : 'a) 
    : 'a = 
  let (s,ins) = d in
  eval_init_names env s ins success_cont summary

and eval_init_names 
    (env:env) (spec:specifier) (ins:init_name list) 
    (success_cont: env -> execution_result -> 'a -> 'a) 
    (summary : 'a) 
    : 'a = 
  match ins with
  | [] -> success_cont env unit_execution_result summary
  | (n,e) :: ins ->
      let proc_e res_e s_e =
	let (ns,_,_,_) = n in
	let tp = find_tp spec in
	let vl = Cmm.Rigid (fresh_var_named ns) in
	let loc_and_kind = 
	  match tp with
	  | Tatomic_int 
          | Tatomic_address -> (vl,Cmm.Atomic)
                (* Tmutex handled later, assumed to have no initializations *)
	  | _ -> (vl,Cmm.Non_Atomic)
	in
	let env_n = add_new_var ns vl env in
	let res_eno =
	  match res_e.result with
	  | LVal _ -> Some (load_from_exp env res_e)
	  | RVal _ -> Some (res_e)
	  | Nothing -> None in
        let res_ie =
          match res_eno with
          | None -> (* No explicit initialization: check for implicit initialization of mutex locations *)
              begin
                match tp with
                | Tmutex ->
                    let mutexloc = Cmm.Rigid (fresh_var_named "mutex") in
                    {res_e with
                     loc_kind_map = (mutexloc,Cmm.Mutex) :: (vl,Cmm.Non_Atomic) :: res_e.loc_kind_map;
                     my_actions = RealAction (Cmm.Store (genaction_id(),env.thread_id,Cmm.NA,vl,mutexloc)) :: res_e.my_actions;
                   }
                | _ ->
                    {res_e with
                     loc_kind_map = loc_and_kind :: res_e.loc_kind_map }
              end
          | Some res_en ->
	      let ve =
	        match res_en.result with
	        | RVal v -> v
	        | _ -> internal_error "Cannot happen" in
	      let new_action_store = Cmm.Store (genaction_id (),env.thread_id,Cmm.NA,vl,ve) in
	      let sdep_acts = filter_s_or_cdep_targets res_en.my_actions in
	      let new_sdep_store = cross_reln sdep_acts
	          [RealAction new_action_store] in
	      let res_ie = 
	        {res_en with
	         my_actions = (RealAction new_action_store) :: res_en.my_actions;
	         loc_kind_map = loc_and_kind :: res_en.loc_kind_map;
	         seq_deps = new_sdep_store @ res_en.seq_deps;
	       } in 
              res_ie 
        in
	let proc_i env_i res_i s_i =
	  let res = concat res_ie res_i in
	  success_cont env_i res s_i in
	eval_init_names env_n spec ins proc_i s_e in
      eval_init_exp env e proc_e summary
      
and eval_init_exp
    (env:env) (ie:init_expression) 
    (success_cont: execution_result -> 'a -> 'a) 
    (summary : 'a) 
    : 'a = 
  match ie with
  | NO_INIT -> success_cont unit_execution_result summary
  | SINGLE_INIT e ->
      eval_exp env e false success_cont summary
  | COMPOUND_INIT _ -> cannot_handle "compound type initializations"

and eval_fun 
    (env:env) (f:single_name) (args:expression list) (sbody:block) 
    (success_cont: execution_result -> 'a -> 'a) 
    (summary : 'a) 
    : 'a = 
  let formal_params = 
    match f with 
    | (_,(_,PROTO(_,formals,_),_,_)) -> formals
    | _ -> internal_error ("Cannot find formal arguments") in
  let rec eval_exps_formals env es fs vmap sc s =
    match (fs,es) with
    | [],[] -> sc unit_execution_result vmap s
    | f::_,[] -> internal_error ("Less arguments than required")
    | [],e::_ -> internal_error ("More arguments than required")
    | f :: fs, e:: es ->
	let proc_e res_e s_e =
	  let proc_es res_es vmap_n s_es =
	    let new_loc = Cmm.Rigid (fresh_var_named (name_of_single_name f)) in
	    let loc_and_kind = 
	      match (type_of_single_name f) with
	      | Tatomic_int 
              | Tatomic_address -> (new_loc,Cmm.Atomic)
              | Tmutex -> (new_loc,Cmm.Mutex)
	      | _ -> (new_loc,Cmm.Non_Atomic)
	    in
	    let res_en = 
	      match res_e.result with
	      | LVal _ -> load_from_exp env res_e 
	      | RVal _ -> res_e
	      | Nothing -> internal_error "No return value from exp" in
	    let ve =
	      match res_en.result with
	      | RVal v -> v
	      | _ -> internal_error "Cannot happen" in
	    let res_i = concat res_en res_es in
	    let new_action = Cmm.Store (genaction_id (),env.thread_id,Cmm.NA,new_loc,ve) in
	    let ddep_sources = filter_ddep_sources res_en.my_actions in
	    let new_ddep = cross_reln ddep_sources [new_action] in
	    let sdep_acts = filter_s_or_cdep_targets res_en.my_actions in
	    let new_sdep = cross_reln sdep_acts [RealAction new_action] in
	    let res_a = 
	      concat 
		{res_i with
		 my_actions = (RealAction new_action) :: res_en.my_actions;
		 loc_kind_map = loc_and_kind :: res_en.loc_kind_map;
		 seq_deps = new_sdep @ res_en.seq_deps;
		 data_deps = new_ddep @ res_en.data_deps;
	       }
		res_es in
	    sc res_a ((name_of_single_name f,new_loc) :: vmap_n) s_es in
	  eval_exps_formals env es fs vmap proc_es s_e in
	eval_exp env e false proc_e s
  in
  let proc_args res_args arg_vmap s_args =
    let new_vmap = (* new_local_vmap @ *) arg_vmap in 
    let proc_block res_b s_b =
      let res_concat = sequence (* res_args_and_locals *) res_args res_b in
      let res = res_concat in
      success_cont res s_b in
    eval_block {env with var_loc_map = new_vmap :: env.var_loc_map} 
      sbody proc_block s_args in
  eval_exps_formals env args formal_params [] proc_args summary


let rec eval_threads 
    (env:env) (ts:new_thread_data list) 
    (success_cont: execution_result -> 'a -> 'a) 
    (summary : 'a) 
    : 'a = 
  match ts with
  | [] -> success_cont unit_execution_result summary
  | t :: ts ->
      let proc_t res_t s_t =
	let proc_ts res_ts s_ts =
	  let res = concat res_t res_ts in
	  success_cont res s_ts in
	eval_threads env ts proc_ts s_t in
      eval_stmts t.env [t.stmt] proc_t summary

let eval_file (file:file) 
    (success_cont: execution_result -> 'a -> 'a) 
    (summary : 'a) 
    : 'a = 
  let main_name,main_fun = find_fun_by_name file "main" in
(*  let globalvars = 
    List.filter 
      (fun g -> 
	match g with
	| GVar _ -> true
	| _ -> false) file.globals in
  let new_locs,new_global_vmap,global_inits =
    split3 (* Turns list of triples into triple of lists *)
      (List.map 
	 (function 
	   | (GVar(vi,ii,_)) -> 
	       let new_loc = fresh_var_named vi.vname in
	       let loc_and_kind = 
		 match vi.vtype with
		 | TAtomic_int _ 
                 | TAtomic_address _ -> (new_loc,LK_Atomic)
		 | TMutex _ -> (new_loc,LK_Mutex) (* TODO: needs to be initted *)
		 | _ -> (new_loc,LK_Nonatomic)
	       in
	       loc_and_kind,(vi,new_loc),(vi,ii)
	   | _ -> internal_error "Filtered out non-global variable appeared")
	 globalvars) in *)
  let env = {current_file = file;
	     thread_id = genthread_id ();
	     var_loc_map = [] (* [new_global_vmap]*);
	   } in
  let rec loop sc res s =
    match res.new_threads with
    | [] -> sc res s 
    | t :: _ ->
	let proc_ts res_ts s_ts =
	  let res_new = concat {res with new_threads = []} res_ts in
	  loop sc res_new s_ts in
	eval_threads env res.new_threads proc_ts s in
(*  let rec eval_global_inits env inits sc s =
    match inits with 
    | [] -> sc unit_execution_result s
    | (vi,init) :: inits ->
	begin
	  match init.init with
	  | None -> eval_global_inits env inits sc s
	  | Some (SingleInit e) ->
	      let proc_e res_e s_e =
		let proc_rest res_rest s_rest =
		  let ve = 
		    match res_e.result with
		    | Some v -> v 
		    | None -> internal_error "No return value from exp" in
		  let vl = 
		    try 
		      lookup env.var_loc_map vi
		    with Not_found -> user_error ("Cannot find variable "^vi.vname)
		  in
		  let new_action_store = Store (genaction_id (),env.thread_id,vl,ve) in
		  let ddep_sources = filter_ddep_sources res_e.my_actions in
		  let new_ddep = cross_reln ddep_sources [new_action_store] in
		  let sdep_acts = filter_s_or_cdep_targets res_e.my_actions in
		  let new_sdep = cross_reln sdep_acts [RealAction new_action_store] in
		  let res_concat = concat res_e res_rest in
		  let res = {res_concat with
			     my_actions = (RealAction new_action_store) :: res_concat.my_actions;
			     data_deps = new_ddep @ res_concat.data_deps;
			     seq_deps = new_sdep @ res_concat.seq_deps;} in
		  sc res s_rest in
		eval_global_inits env inits proc_rest s_e in
	      eval_exp env e proc_e s
	  | Some (CompoundInit _) -> cannot_handle "Complex initialization"
	end in
 *)  
  let proc_global_inits res_gi s_gi = 
    let proc_loop_done res_ld s_ld =
      let res = sequence res_gi res_ld in
      success_cont res s_ld 
    in
    eval_fun env main_name [] main_fun (loop proc_loop_done) s_gi 
  in
  proc_global_inits unit_execution_result summary
(*  eval_global_inits env global_inits proc_global_inits summary *)


let get_rfm_poss res = 
  let acts = real_actions_of res.my_actions in
  let load_acts = 
    List.filter
      (fun a -> 
	match a with
	| Cmm.Load _
	| Cmm.RMW _
	(* After discussion with MB, no more locks in rfmaps | Lock _ *) -> true
	| _ -> false) acts in
  (* Prefilter obvious unification failures *)
  (* Mark: this looks like an optimization. If I don't change the type of locations then it might kick out some valid stuff *)
  let prefilter = function
    | Cmm.Rigid (Cmm.Concrete i1),Cmm.Rigid (Cmm.Concrete i2),_,_ when i1 != i2 -> true
    | Cmm.Rigid (Cmm.Symbolic s1),Cmm.Rigid (Cmm.Symbolic s2),_,_ when s1 != s2 -> true
    | Cmm.Rigid (Cmm.Concrete _),Cmm.Rigid (Cmm.Symbolic _),_,_ -> true
    | Cmm.Rigid (Cmm.Symbolic _),Cmm.Rigid (Cmm.Concrete _),_,_ -> true

    | _,_,Cmm.Rigid (Cmm.Concrete i1),Cmm.Rigid (Cmm.Concrete i2) when i1 != i2 -> true (* Mark: and this *)
    | _,_,Cmm.Rigid (Cmm.Symbolic s1),Cmm.Rigid (Cmm.Symbolic s2) when s1 != s2 -> true
    | _,_,Cmm.Rigid (Cmm.Concrete _),Cmm.Rigid (Cmm.Symbolic _) -> true
    | _,_,Cmm.Rigid (Cmm.Symbolic _),Cmm.Rigid (Cmm.Concrete _) -> true

    | _ -> false in
  let poss =
    List.map
      (fun ra -> 
	match ra with
	| Cmm.Load (_,_,_,lr,vr)
	| Cmm.RMW (_,_,_,lr,vr,_) ->
	    let poss_writes =
	      option_map
		(fun wa ->
		  match wa with
		  | Cmm.Store (_,_,_,lw,vw) 
		  | Cmm.RMW (_,_,_,lw,_,vw) ->
                    if prefilter (lr,lw,vr,vw) then None
                    else
                      (* if List.mem (RealAction ra,RealAction wa) res.seq_deps (* Do not read from sb-future *)
			 then None 
			 else *)
                      Some (wa,[(lr,lw);(vr,vw)])
		  | _ -> None) acts in
	    List.map 
	      (fun (wa,cs) -> ((Some wa,ra),cs)) poss_writes
(* 
	| Lock (_,_,lr) ->
	    let poss_writes =
	      option_map
		(fun wa ->
		  match wa with
		  | Unlock (_,_,lw) ->
		      begin
			(* Prefilter obvious unification failures *)
			match lr,lw with
			| Const_int i1,Const_int i2 when i1 != i2 -> None
			| Const_id s1,Const_id s2 when s1 != s2 -> None
			| Const_int _,Const_id _ -> None
			| Const_id _,Const_int _ -> None
			| _ -> 
(*			    if List.mem (RealAction ra,RealAction wa) res.seq_deps (* Do not read from sb-future *)
			    then None 
			    else *) Some (wa,[(lr,lw)])
		      end
		  | _ -> None) acts in
            (* The initial lock ? *)
            ((None,ra),[]) :: 
	    List.map 
	      (fun (wa,cs) -> ((Some wa,ra),cs)) poss_writes	
*)    
	| _ -> internal_error "Non-load in load position")
      load_acts in
  poss

let rec fold_all_perms xs sc s = 
  match xs with
  | [] -> sc [] s
  | [x] -> sc [x] s
  | x :: xs ->
      let proc_pxs pxs s_pxs =
	let rec insert zs ys =
	  match ys with
	  | [] -> sc (zs @ [x]) s_pxs
	  | y :: ys -> 
	      sc (zs @ [x] @ (y :: ys))
		(insert (zs @ [y]) ys)
	in
	insert [] pxs
      in
      fold_all_perms xs proc_pxs s


let rec fold_all_perms_sets xss sc s =
  let rec f acc s = function
    | [] -> sc (List.rev acc) s
    | xs :: xss ->
      fold_all_perms xs (fun x s -> f (x :: acc) s xss) s in
  f [] s xss


(*
let rec fold_perms_matching xs ord sc s = 
  match xs with
  | [] -> sc [] s
  | [x] -> sc [x] s
  | x :: xs ->
      let proc_pxs pxs s_pxs =
	let rec insert zs ys =
	  match ys with
	  | [] -> sc (zs @ [x]) s_pxs
	  | y :: ys -> 
	      sc (zs @ [x] @ (y :: ys))
		(if List.mem (y,x) ord
		    then (* done ?? *) s_pxs
		else
		  (insert (zs @ [y]) ys))
	in
	insert [] pxs
      in
      fold_perms_matching xs ord proc_pxs s
*)

let fold_cross xss sc s =
  let rec fold_rec s ys xss = match xss with
  | [] -> sc ys s
  | xs :: xss ->
      List.fold_left
	(fun s x -> fold_rec s (x :: ys) xss)
	s xs in
  fold_rec s [] (List.rev xss)

let fold_perms_cross xss sc s =
  let rec fold_rec s ys xss = match xss with
  | [] -> sc ys s
  | xs :: xss ->
      let proc_pxs pxs s_pxs =
	fold_rec s_pxs (pxs :: ys) xss
      in
      fold_all_perms xs proc_pxs s in
  fold_rec s [] (List.rev xss)

let writes_per_loc res = 
  let acts = real_actions_of res.my_actions in
  let store_acts = 
    List.filter
      (fun a -> 
	match a with
	| Cmm.Store _
	| Cmm.RMW _ -> true 
	| _ -> false) acts in
  let stores_by_loc =
    List.fold_left 
      (fun sl s ->
	let l = 
	  match Cmm.loc_of s with
	  | Some l -> l
	  | None -> internal_error "No location for picked out store action"
	in
	try  
	  begin
	    match List.assoc l res.loc_kind_map with
	    | Cmm.Atomic ->
		begin 
		  try 
		    let s_at_l = List.assoc l sl 
		    in (l,s::s_at_l) :: (List.remove_assoc l sl)
		  with Not_found -> (l,[s]) :: sl
		end
	    | _ -> 
		sl
	  end
	with Not_found -> sl
	   ) [] store_acts 
  in
  stores_by_loc

let parse_cil filename = 
  let cilf = Frontc.parse_to_cabs filename in 
(*  let _ = Rmtmps.removeUnusedTemps cilf in
  let _ = Cil.lineDirectiveStyle := None in
  let _ = if !quietmode then () else Cil.dumpFile Cil.plainCilPrinter stdout "" cilf in *)
  cilf

  
let rec fact n =
  if n <= 1 then 1 else n * fact (n-1)


