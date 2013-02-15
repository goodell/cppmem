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

open Value
open Error

type atom_constraint =
  | EqBinOp of Cmm.cvalue * Cmm.cvalue * Cabs.binary_operator * Cmm.cvalue
  | EqUnOp of Cmm.cvalue * Cabs.unary_operator * Cmm.cvalue
  | IsLocOf of vloc * Cmm.cvalue
  | Eq of Cmm.cvalue * Cmm.cvalue
  | Neq of Cmm.cvalue * Cmm.cvalue
  | IsTrueC of Cmm.cvalue
  | IsFalseC of Cmm.cvalue

and d_constraint =
  | C of atom_constraint
  | Disj of vconstraint list

and vconstraint =
  | Conj of d_constraint list


let ctrue = Conj []
let cfalse = Conj [(Disj [])]

let pp_un_op () = function
  | Cabs.MINUS -> "-"
  | Cabs.PLUS -> "+"
  | Cabs.NOT -> "!"
  | Cabs.BNOT -> "~"
  | Cabs.MEMOF -> "*"
  | Cabs.ADDROF -> "&"
  | Cabs.PREINCR 
  | Cabs.PREDECR 
  | Cabs.POSINCR
  | Cabs.POSDECR -> cannot_handle "increment/decrement operators"

let pp_bin_op () = function
  | Cabs.ADD -> "+"
  | Cabs.SUB -> "-"
  | Cabs.MUL -> "*"
  | Cabs.DIV -> "/"
  | Cabs.MOD -> "%"
  | Cabs.AND -> "&&"
  | Cabs.OR -> "||"
  | Cabs.BAND -> "&"
  | Cabs.BOR -> "|"
  | Cabs.XOR -> "^"
  | Cabs.SHL -> "<<"
  | Cabs.SHR -> ">>"
  | Cabs.EQ -> "=="
  | Cabs.NE -> "!="
  | Cabs.LT -> "<"
  | Cabs.GT -> ">"
  | Cabs.LE -> "<="
  | Cabs.GE -> ">="

  | Cabs.ASSIGN -> "="
  | Cabs.ADD_ASSIGN -> "+="
  | Cabs.SUB_ASSIGN -> "-="
  | Cabs.MUL_ASSIGN -> "*="
  | Cabs.DIV_ASSIGN -> "/="
  | Cabs.MOD_ASSIGN -> "%="
  | Cabs.BAND_ASSIGN -> "&="
  | Cabs.BOR_ASSIGN -> "|="
  | Cabs.XOR_ASSIGN -> "^="
  | Cabs.SHL_ASSIGN -> "<<="
  | Cabs.SHR_ASSIGN -> ">>="


let rec pp_atom_constraint () = function
  | EqBinOp (v,v1,op,v2) -> Printf.sprintf "%a = %a %a %a" pp_value v  pp_value v1  pp_bin_op op  pp_value v2
  | EqUnOp (v,op,v1) -> Printf.sprintf "%a = %a %a" pp_value v  pp_un_op op  pp_value v1
  | IsLocOf (v1,v2) -> Printf.sprintf "%a == & %a" pp_vloc v1  pp_value v2
  | Eq (v1,v2) -> Printf.sprintf "%a == %a" pp_value v1  pp_value v2
  | Neq (v1,v2) -> Printf.sprintf "%a != %a" pp_value v1  pp_value v2
  | IsTrueC v -> Printf.sprintf "is_true(%a)" pp_value v
  | IsFalseC v -> Printf.sprintf "is_false(%a)" pp_value v

and pp_dconstraint () = function
  | C c -> pp_atom_constraint () c
  | Disj vcs -> String.concat " \\/ " (List.map (fun c -> Printf.sprintf "%a" pp_vconstraint c) vcs)

and pp_vconstraint () = function 
  | Conj [] -> "true"
  | Conj [(Disj [])] -> "false"
  | Conj ds -> String.concat " /\\ " (List.map (fun d -> Printf.sprintf "%a" pp_dconstraint d) ds) 
 

let rec subst_atom_constraint s = function
  | EqBinOp (vr,v1,op,v2) -> EqBinOp (subst_val s vr,subst_val s v1,op,subst_val s v2)
  | EqUnOp (vr,op,v1) -> EqUnOp (subst_val s vr,op,subst_val s v1)
  | IsLocOf (v1,v2) -> IsLocOf (subst_loc s v1,subst_val s v2)
  | Eq (v1,v2) -> Eq (subst_val s v1,subst_val s v2)
  | Neq (v1,v2) -> Neq (subst_val s v1,subst_val s v2)
  | IsTrueC (v) -> IsTrueC (subst_val s v)
  | IsFalseC (v) -> IsFalseC (subst_val s v)

and subst_d_constraint s = function
  | C at -> C (subst_atom_constraint s at)
  | Disj ats -> Disj (List.map (subst_vconstraint s) ats)

and subst_vconstraint s = function
  | Conj ds -> Conj (List.map (subst_d_constraint s) ds)


let newv_un_op op v1 = 
  let v = fresh_var () in
  let c = EqUnOp (v,op,v1) in
  v,c

let newv_bin_op op v1 v2 = 
  let v = fresh_var () in
  let c = EqBinOp (v,v1,op,v2) in
  v,c

let newv_locof v = 
  let vl = fresh_loc () in
  let c = IsLocOf (vl,v) in
  v,c



