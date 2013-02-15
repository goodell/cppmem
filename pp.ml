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
open Types
open Auxl
open Printf
open Globals

(* for id.ml *)
 

(* for types.ml *)
let pp_layout () = function
| LO_dot             -> "dot"            
| LO_neato_par       -> "neato_par"      
| LO_neato_par_init  -> "neato_par_init" 
| LO_neato_downwards -> "neato_downwards"

let pp_legend () = function
| None -> "\"\""
| Some s -> "\""^s^"\""

let pp_outcome () = function
  | Cmm.Locked -> "Locked"
  | Cmm.Blocked -> "Blocked "

let pp_loc = pp_value


let pp_location_kind () = function
  | Cmm.Non_Atomic -> "nonatomic"
  | Cmm.Atomic -> "atomic"
  | Cmm.Mutex -> "mutex"

let pp_isa_location_kind () = function
  | Cmm.Non_Atomic -> "Non_atomic"
  | Cmm.Atomic -> "Atomic"
  | Cmm.Mutex -> "Mutex"

let pp_tex_location_kind () = function
  | Cmm.Non_Atomic -> "\\textsc{Non\\_atomic}"
  | Cmm.Atomic -> "\\textsc{Atomic}"
  | Cmm.Mutex -> "\\textsc{Mutex}"

let pp_isa_memory_order_enum () = function
  | Cmm.NA -> "Mo_NA"
  | Cmm.Seq_cst -> "Mo_seq_cst"
  | Cmm.Relaxed -> "Mo_relaxed"
  | Cmm.Release -> "Mo_release"
  | Cmm.Acquire -> "Mo_acquire"
  | Cmm.Consume -> "Mo_consume"
  | Cmm.Acq_rel -> "Mo_acq_rel"

(*let pp_memory_order_enum2 () = function
  | Cmm.Mo_seq_cst -> "sc"
  | Cmm.Mo_relaxed -> "rlx"
  | Cmm.Mo_release -> "rel"
  | Cmm.Mo_acquire -> "acq"
  | Cmm.Mo_consume -> "con"
  | Cmm.Mo_acq_rel -> "a/r"
*)
let pp_memory_order_enum2 () = function
  | Cmm.NA -> "na"
  | Cmm.Seq_cst -> "sc"
  | Cmm.Relaxed -> "rlx"
  | Cmm.Release -> "rel"
  | Cmm.Acquire -> "acq"
  | Cmm.Consume -> "con"
  | Cmm.Acq_rel -> "a/r"

let pp_memory_order_enum_skel = function
  | Cmm.NA -> "na"
  | Cmm.Seq_cst -> "sc"
  | Cmm.Relaxed -> "rlx"
  | Cmm.Release -> "rel"
  | Cmm.Acquire -> "acq"
  | Cmm.Consume -> "con"
  | Cmm.Acq_rel -> "a/r"

let pp_order_type_skel = function
  | Nonatomic ->  "na"
  | Atomic a -> pp_memory_order_enum_skel a
  | Mutex -> sprintf "mutex"

let pp_order_type () = function
  | Nonatomic -> sprintf "nonatomic"
  | Atomic a -> pp_isa_memory_order_enum () a
  | Mutex -> sprintf "mutex"

let pp_order_type2 () = function
  | Nonatomic -> sprintf "na"
  | Atomic a -> pp_memory_order_enum2 () a
  | Mutex -> sprintf "mtx"

let pp_memory_order_enum3 m () = 
  function mo -> 
    sprintf "%a" pp_memory_order_enum2 mo



let pp_order_type3 m () = 
  function o -> 
    sprintf "%a" pp_order_type2 o

let pp_thread_id () tid =
  pp_value () tid

(* let rec pp_label () = function *)
(*   | Lab_tau -> sprintf "Tau"   *)
(*   | Lab_write (i,tid,o,l,e) -> sprintf "%a:W %a %a %a %a" pp_action_id i pp_thread_id tid pp_order_type o pp_loc l pp_exp e *)
(*   | Lab_read (i,tid,o,l,e)  -> sprintf "%a:R %a %a %a %a" pp_action_id i pp_thread_id tid pp_order_type o pp_loc l pp_exp e *)
(*   | Lab_new_l (i,tid,o,l,e) -> sprintf "%a:new %a %a %a %a" pp_action_id i pp_thread_id tid pp_order_type o pp_loc l pp_exp e *)
(*   | Lab_new_t s -> pp_stmt () s  *)

(* and pp_label2 () = function *)
(*   | Lab_tau -> sprintf "Tau"   *)
(*   | Lab_write (i,tid,o,l,e) -> sprintf "%a:W %a %a %a"   pp_action_id i pp_order_type2 o pp_loc l pp_exp e *)
(*   | Lab_read (i,tid,o,l,e)  -> sprintf "%a:R %a %a %a"   pp_action_id i pp_order_type2 o pp_loc l pp_exp e *)
(*   | Lab_new_l (i,tid,o,l,e) -> sprintf "%a:new %a %a %a" pp_action_id i pp_order_type2 o pp_loc l pp_exp e *)
(*   | Lab_new_t s -> pp_stmt () s  *)

(* and *)
let rec pp_action rl () a = match a with
  | Cmm.Lock (aid,tid,l,oc) -> 
     sprintf "%a,%a:Lock %a %a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_loc l  pp_outcome oc
  | Cmm.Unlock (aid,tid,l) ->
     sprintf "%a,%a:Unlock %a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_loc l
  | Cmm.Load (aid,tid,mo,l,v) ->
     sprintf "%a,%a:Load %a %a %a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_memory_order_enum2 mo  pp_loc l  pp_value v
  | Cmm.Store (aid,tid,mo,l,v) ->
     sprintf "%a,%a:Store %a %a %a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_memory_order_enum2 mo  pp_loc l  pp_value v
  | Cmm.RMW (aid,tid,mo,l,v1,v2) ->
     sprintf "%a,%a:RMW %a %a %a %a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_memory_order_enum2 mo  pp_loc l  pp_value v1  pp_value v2
  | Cmm.Blocked_rmw (aid,tid,l) ->
    sprintf "%a,%a:Blocked_rmw %a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_loc l
  | Cmm.Fence (aid,tid,mo) ->
     sprintf "%a,%a:Fence %a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_memory_order_enum2 mo

and pp_skeleton_action rl () = function a -> match a with
  | S_Lock (aid,tid) ->
     sprintf "%a;%a:L" (pp_skeleton_action_id' rl) aid  (pp_skeleton_thread_id' rl) tid  
  | S_Unlock (aid,tid) ->
     sprintf "%a;%a:U" (pp_skeleton_action_id' rl) aid  (pp_skeleton_thread_id' rl) tid 
  | S_Read (aid,tid,mos) ->
     sprintf "%a;%a:R %a" (pp_skeleton_action_id' rl) aid  (pp_skeleton_thread_id' rl) tid  (pp_skeleton_mos) mos
  | S_Write (aid,tid,mos) ->
     sprintf "%a;%a:W %a" (pp_skeleton_action_id' rl) aid  (pp_skeleton_thread_id' rl) tid   (pp_skeleton_mos) mos
  | S_Atomic_rmw (aid,tid,mos) ->
     sprintf "%a;%a:RMW %a" (pp_skeleton_action_id' rl) aid  (pp_skeleton_thread_id' rl) tid   (pp_skeleton_mos) mos
  | S_Fence (aid,tid,mos) ->
     sprintf "%a;%a:F %a" (pp_skeleton_action_id' rl) aid  (pp_skeleton_thread_id' rl) tid   (pp_skeleton_mos) mos

and pp_skeleton_mos () = function
  | MO_Star -> "*"
  | MO_Atomic -> "atomic"
  | MO_Set moset -> "{"^String.concat "," (List.map pp_order_type_skel moset) ^"}"  

and pp_action_or_name rl () = function 
  | Action a -> pp_action rl () a
  | Action_name s -> pp_action_id' rl () s

and pp_skeleton_action_or_name rl () = function 
  | S_Action a -> pp_skeleton_action rl () a
  | S_Action_name s -> pp_action_id' rl () s

and pp_action_id () aid = aid

and pp_action_id' rl () aid = 
  let (action_relabelling,thread_relabelling) = rl in 
  try List.assoc aid action_relabelling with Not_found -> pp_action_id () aid

and pp_skeleton_action_id' rl () aid = 
  let (action_relabelling,thread_relabelling) = rl in 
  try List.assoc aid action_relabelling with Not_found -> pp_action_id () aid

and pp_thread_id' rl () tid =
  let (action_relabelling,thread_relabelling) = rl in 
  try List.assoc tid thread_relabelling with Not_found -> pp_thread_id () tid

and pp_skeleton_thread_id' rl () (tid : Cmm.tid) =
  let (action_relabelling,thread_relabelling) = rl in 
  try List.assoc tid thread_relabelling with Not_found -> pp_thread_id () tid


(* and pp_action' m rl () = function a -> match a with *)
(*   | Lock(aid,tid,l)         ->  *)
(*      sprintf "%a:Lock %a" (pp_action_id' rl) a   pp_loc l  *)
(*   | Unlock(aid,tid,l)       -> *)
(*      sprintf "%a:Unlock %a" (pp_action_id' rl) a   pp_loc l  *)
(*   | Atomic_load(aid,tid,mo,l,v) -> *)
(*      sprintf "%a:R%a %a=%a" (pp_action_id' rl) a   (pp_memory_order_enum3 m) mo pp_loc l  pp_vint v *)
(*   | Atomic_store(aid,tid,mo,l,v) -> *)
(*      sprintf "%a:W%a %a=%a" (pp_action_id' rl) a   (pp_memory_order_enum3 m) mo pp_loc l  pp_vint v *)
(*   | Atomic_rmw(aid,tid,mo,l,v1,v2) -> *)
(*      sprintf "%a:RMW%a %a=%a/%a" (pp_action_id' rl) a   (pp_memory_order_enum3 m) mo pp_loc l  pp_vint v1  pp_vint v2 *)
(*   | Load(aid,tid,l,v) -> *)
(*      sprintf "%a:R%a %a=%a" (pp_action_id' rl) a  (pp_order_type3 m) Nonatomic pp_loc l  pp_vint v *)
(*   | Store(aid,tid,l,v) -> *)
(*      sprintf "%a:W%a %a=%a" (pp_action_id' rl) a  (pp_order_type3 m) Nonatomic pp_loc l  pp_vint v *)
(*   | Fence(aid,tid,mo) -> *)
(*      sprintf "%a:Fence %a" (pp_action_id' rl) a   (pp_memory_order_enum3 m) mo *)

and pp_action_thread_id' m rl () (aid,tid) = 
  if m.thread_ids then
    sprintf "%a,%a" (pp_action_id' rl) aid (pp_thread_id' rl) tid
  else
    sprintf "%a" (pp_action_id' rl) aid 

and pp_action' m rl () = function a -> match a with
  | Cmm.Lock (aid,tid,l,oc) -> 
      let fmt = 
        if m.texmode then format_of_string "\\\\LK{%a}{%a}{%a}" else format_of_string "%a:LK %a %a" in 
     sprintf fmt (pp_action_thread_id' m rl) (aid,tid)  pp_loc l  pp_outcome oc
  | Cmm.Unlock(aid,tid,l) ->
      let fmt =
        if m.texmode then format_of_string "\\\\UL{%a}{%a}" else format_of_string "%a:UL %a" in
      sprintf fmt (pp_action_thread_id' m rl) (aid,tid)  pp_loc l 
  | Cmm.Load (aid,tid,mo,l,v) ->
      let fmt =
        if m.texmode then format_of_string "\\\\RA{%a}{%a}{%a}{%a}" else format_of_string "%a:R%a %a=%a" in
      sprintf fmt (pp_action_thread_id' m rl) (aid,tid)  (pp_memory_order_enum3 m) mo  pp_loc l  pp_value v
  | Cmm.Store (aid,tid,mo,l,v) ->
      let fmt =
        if m.texmode then format_of_string "\\\\WA{%a}{%a}{%a}{%a}" else format_of_string "%a:W%a %a=%a" in 
     sprintf fmt (pp_action_thread_id' m rl) (aid,tid)  (pp_memory_order_enum3 m) mo  pp_loc l  pp_value v
  | Cmm.RMW (aid,tid,mo,l,v1,v2) ->
      let fmt =
        if m.texmode then format_of_string "\\\\RMW{%a}{%a}{%a}{%a}{%a}" else format_of_string "%a:RMW%a %a=%a/%a" in
     sprintf fmt (pp_action_thread_id' m rl) (aid,tid)  (pp_memory_order_enum3 m) mo  pp_loc l  pp_value v1  pp_value v2
  | Cmm.Blocked_rmw (aid,tid,l) ->
      let fmt =
        if m.texmode then format_of_string "\\\\BRMW{%a}{%a}" else format_of_string "%a:BRMW%a" in
     sprintf fmt (pp_action_thread_id' m rl) (aid,tid)  pp_loc l
  | Cmm.Fence (aid,tid,mo) ->
      let fmt =
        if m.texmode then format_of_string "\\\\FN{%a}{%a}" else format_of_string "%a:F%a" in
      sprintf fmt (pp_action_thread_id' m rl) (aid,tid)  (pp_memory_order_enum3 m) mo

and pp_tex_action' m rl () a = 
  (* remove the extra dot-escaped backslash *)
  let s = (sprintf "%a" (pp_action' m rl) a) in
  String.sub s 1 (String.length s -1)


and pp_isa_string s = "''"^s^"''"
and pp_isa_action_id rl () a = pp_isa_string (sprintf "%a" (pp_action_id' rl) a)
and pp_isa_thread_id rl () tid = pp_isa_string (sprintf "%a" (pp_thread_id' rl) tid)
and pp_isa_loc () l = pp_isa_string (sprintf "%a" pp_loc l)
and pp_isa_vint () e = pp_isa_string (sprintf "%a" pp_value e)
and pp_isa_value () e = pp_isa_string (sprintf "%a" pp_value e)
and pp_isa_outcome () = function
  | Cmm.Locked -> pp_isa_string "success"
  | Cmm.Blocked -> pp_isa_string "blocked"

and pp_isa_action rl () a = match a with
  | Cmm.Lock (aid,tid,l,oc) -> 
     sprintf "Lock %a %a %a %a" (pp_isa_action_id rl) aid  (pp_isa_thread_id rl) tid  pp_isa_loc l  pp_isa_outcome oc
  | Cmm.Unlock (aid,tid,l) ->
     sprintf "Unlock %a %a %a" (pp_isa_action_id rl) aid  (pp_isa_thread_id rl) tid  pp_isa_loc l 
  | Cmm.Load (aid,tid,mo,l,v) ->
     sprintf "Load %a %a %a %a %a" (pp_isa_action_id rl) aid  (pp_isa_thread_id rl) tid  pp_isa_memory_order_enum mo  pp_isa_loc l  pp_isa_value v
  | Cmm.Store (aid,tid,mo,l,v) ->
     sprintf "Store %a %a %a %a %a" (pp_isa_action_id rl) aid  (pp_isa_thread_id rl) tid  pp_isa_memory_order_enum mo  pp_isa_loc l  pp_isa_value v
  | Cmm.RMW (aid,tid,mo,l,v1,v2) ->
     sprintf "RMW %a %a %a %a %a %a" (pp_isa_action_id rl) aid  (pp_isa_thread_id rl) tid  pp_isa_memory_order_enum mo  pp_isa_loc l  pp_isa_value v1  pp_isa_value v2
  | Cmm.Blocked_rmw (aid,tid,l) ->
    sprintf "BRMW %a %a %a" (pp_isa_action_id rl) aid  (pp_isa_thread_id rl) tid  pp_isa_loc l

  | Cmm.Fence(aid,tid,mo) ->
     sprintf "Fence %a %a %a" (pp_isa_action_id rl) aid  (pp_isa_thread_id rl) tid  pp_isa_memory_order_enum mo


(* and pp_exp () = function *)
(*   | (Expr_num n) -> pp_vint () n *)
(*   | (Expr_bool b) -> pp_vbool () b  *)
(*   | (Expr_op (e1,op,e2)) -> sprintf "%a %a %a" pp_exp e1 pp_op op pp_exp e2 *)
(*   | (Expr_loc l) -> pp_loc () l *)
(*   | (Expr_atomic_initialization (o,e1,e2)) -> sprintf "int_%a %a = %a" pp_memory_order_enum2 o pp_exp e1 pp_exp e2 *)
(*   | (Expr_atomic_load (o,e)) -> sprintf "load_%a (%a)" pp_memory_order_enum2 o pp_exp e *)
(*   | (Expr_atomic_store (o,e1,e2)) -> sprintf "store_%a (%a,%a)" pp_memory_order_enum2 o pp_exp e1 pp_exp e2 *)
(*   | (Expr_skip) -> sprintf "skip" *)

(* and pp_stmt rl () = function *)
(*   | Stmt_expr e -> pp_exp () e *)
(*   | Stmt_cond (e,s1,s2) -> sprintf "if (%a) %a else %a" pp_exp e (pp_stmt rl) s1 (pp_stmt rl) s2 *)
(*   | Stmt_seq (s1,s2) ->  sprintf "%a ; %a" (pp_stmt rl) s1 (pp_stmt rl) s2 *)
(*   | Stmt_start_new_thread s -> sprintf "new_thread %a" (pp_stmt rl) s *)
(*   | Stmt_ddep (a,s) -> sprintf "ddep_%a (%a)" (pp_action rl) a (pp_stmt rl) s *)
(*   | Stmt_sdep (a,s) -> sprintf "sdep_%a (%a)" (pp_action rl) a (pp_stmt rl) s *)
(*   | Stmt_cdep (a,s) -> sprintf "cdep_%a (%a)" (pp_action rl) a (pp_stmt rl) s *)

(* let pp_thread rl () = function *)
(*   | Thread (tid,s) -> sprintf "%a : %a" pp_thread_id tid (pp_stmt rl) s *)
 
(* let pp_threads rl () = function threads -> String.concat " " (List.map (function thread -> sprintf "%a" (pp_thread rl) thread) threads) *)

(* let pp_rX rl () = function lls -> "{" ^ String.concat "," (List.map (function (a1,a2)->sprintf "(%a,%a)" (pp_action rl) a1 (pp_action rl) a2) lls)^"}" *)

let pp_ids rl () = function ids -> "{" ^ String.concat "," (List.map (function id->sprintf "%a" (pp_action rl) id) ids)^"}"

let pp_locs rl () = function locs -> "{" ^ String.concat "," (List.map (function l->sprintf "%a" pp_loc l) locs)^"}"

(* (*let pp_deps () = function {data=rD;control=rC;seq=rS} -> sprintf "(-;%a;%a;%a)" pp_rX rD pp_rX rC pp_rX rS *) *)
(* let pp_deps rl () = function {data=rD;control=rC;seq=rS} -> sprintf "{data=%a; control=%a; seq=%a}" (pp_rX rl) rD (pp_rX rl) rC (pp_rX rl) rS *)

(* let pp_global_configuration rl () = function {gcthreads=threads;ids=ids;locs=locs;deps=deps} -> sprintf "{threads=%a; locs=%a; deps=%a}" (pp_threads rl) threads (pp_locs rl) locs (pp_deps rl) deps *)


let pp_ignorechecks ignorechecks =
  String.concat "\n" (List.map (fun s -> "ignore " ^ s ^ ".") ignorechecks)

let pp_instrs rl instrs = 
  String.concat "\n"
    (List.map 
       (function 
	 | DisplayNode a -> 
	     "display_action " ^ (pp_action_id' rl () (Cmm.aid_of a)) ^ "."
	 | SuppressNode a -> 
	     "suppress_action " ^ (pp_action_id' rl () (Cmm.aid_of a)) ^ "."
         | DisplayEdge 
             (rname,CrossProduct(froms,tos)) ->
               let pp_froms =
                 match froms with
                 | None -> ""
                 | Some l -> 
                     " from " 
                     ^ String.concat " " (List.map (fun a -> pp_action_id' rl () (Cmm.aid_of a)) l)
               in
               let pp_tos =
                 match tos with
                 | None -> ""
                 | Some l -> 
                     " to " 
                     ^ String.concat " " (List.map (fun a -> pp_action_id' rl () (Cmm.aid_of a)) l)
               in
               "display_edge " ^ rname ^ pp_froms ^ pp_tos ^ "."
         | DisplayEdge 
             (rname,Exact(rel)) ->
               "display_edge " ^ rname ^ ": "  
               ^ String.concat " " 
                   (List.map (fun (a1,a2) -> (pp_action_id' rl () (Cmm.aid_of a1)) ^ " --> " ^ (pp_action_id' rl () (Cmm.aid_of a2))) rel)
               ^ "."
         | SuppressEdge 
             (rname,CrossProduct(froms,tos)) ->
               let pp_froms =
                 match froms with
                 | None -> ""
                 | Some l -> 
                     " from " 
                     ^ String.concat " " (List.map (fun a -> pp_action_id' rl () (Cmm.aid_of a)) l)
               in
               let pp_tos =
                 match tos with
                 | None -> ""
                 | Some l -> 
                     " to " 
                     ^ String.concat " " (List.map (fun a -> pp_action_id' rl () (Cmm.aid_of a)) l)
               in
               "suppress_edge " ^ rname ^ pp_froms ^ pp_tos ^ "."
         | SuppressEdge 
             (rname,Exact(rel)) ->
               "suppress_edge " ^ rname ^ ": "  
               ^ String.concat " " 
                   (List.map (fun (a1,a2) -> (pp_action_id' rl () (Cmm.aid_of a1)) ^ " --> " ^ (pp_action_id' rl () (Cmm.aid_of a2))) rel)
               ^ "."
       )
       instrs)
  ^ "\n"



(* for dot output *)

type column_head = 
  | CH_tid of Cmm.tid
  | CH_loc of Cmm.location

let pp_column_head rl () = function 
  | CH_tid tid -> sprintf "%a" (pp_thread_id' rl) tid
  | CH_loc loc -> sprintf "%a" pp_loc loc

type layout = {
    columns : ((column_head*(int*int)) * (Cmm.action * (int*int)) list) list;
    column_of : Cmm.action -> int;
    size_x : int;
    size_y : int;
    relabelling : (Cmm.aid*string) list * (Cmm.tid*string) list }


let layout_by_thread m fortext do_relabel exod : layout = 

  let sequenced_before a1 a2 = 
    if List.mem (a1,a2) exod.sb then -1 
    else if List.mem (a2,a1) exod.sb then 1 
    else 0 in

  let sorted_threads = List.sort (fun tid1 tid2 -> compare tid1 tid2) exod.threads in

  let actions_of_thread tid = List.filter (fun a -> tid = Cmm.tid_of a) exod.actions in  

  let actions_by_column = 
    List.map 
      (function tid -> 
        (CH_tid tid,
         List.stable_sort 
           sequenced_before 
           (actions_of_thread tid)))
      sorted_threads in
  
  let size_x = List.length actions_by_column in

  let size_y = List.fold_left max 0 (List.map (function (ch,actions) -> 1+List.length actions) actions_by_column) in

  let rec add_coords_col actions x y = 
    match actions with 
    | [] -> [] 
    | a::actions' -> (a,(x,y)):: add_coords_col actions' x (y+1) in
  
  let rec add_coords x abc = 
    let x_offset,y_offset = (*if (match m.layout with LO_neato_par_init -> true | _->false) then
      (if x=0 then size_y/2 else 0),(if x=0 then 0 else size_y-1)
    else *)
      0,0 in
    match abc with
    | [] -> []
    | (ch,actions)::abc' -> 
        ((ch,(x+x_offset,0+y_offset)),add_coords_col actions (x+x_offset) (1+y_offset)) :: add_coords (x+1) abc' in

  let actions_by_column_with_coords = add_coords 0 actions_by_column in

  let rec action_relabelling_column actions n acc =
    match actions with
    | [] -> (acc,n)
    | (a,(x,y))::actions' ->
        let new_label =
          if n < 25 then String.make 1 (Char.chr (n+Char.code 'a'))
          else sprintf "z%i" n in
        action_relabelling_column actions' (n+1) ((Cmm.aid_of a,new_label)::acc) in

  let rec action_relabelling_columns abc n acc =
    match abc with
    | [] -> acc
    | (ch,actions)::abc' -> 
        let (acc',n') = action_relabelling_column actions n acc in
        action_relabelling_columns abc' n' acc' in



  let rec thread_relabelling ts n acc = 
    match ts with
    | [] -> acc
    | tid::ts' -> 
        let new_label = (if fortext then sprintf "Thread %i" n else sprintf "%i" n) in
        thread_relabelling ts' (n+1) ((tid,new_label)::acc) in

  let action_relabelling : (Cmm.aid * string) list = action_relabelling_columns actions_by_column_with_coords 0 [] in

  let thread_relabelling : (Cmm.tid * string) list = thread_relabelling sorted_threads 0 [] in

  { columns = actions_by_column_with_coords;
    column_of = (function a -> let rec f ts n = match ts with tid::ts' -> if Cmm.tid_of a = tid then n else f ts' (n+1) | [] -> raise (Failure "thread id not found") in f sorted_threads 0);
    size_x = size_x; 
    size_y = (*if (match m.layout with LO_neato_par_init -> true | _->false) then 2*size_y +2 else*) 1*size_y+2 ; (* +2 for possible constraint *) 
    relabelling = if do_relabel then (action_relabelling,thread_relabelling) else ([],[])}


let rename_exwit exwit mode =
  let rl = (layout_by_thread mode false true exwit.exod).relabelling in
  let thread_pp t = Cmm.Rigid (Cmm.Symbolic (pp_thread_id' rl () t)) in
  let action_pp a =
    let a_fixed_aid =                    
      set_action_id (pp_action_id' rl () (Cmm.aid_of a)) a in
    let a_fixed_tid =
      set_thread_id (Cmm.Rigid (Cmm.Symbolic (pp_thread_id' rl () (Cmm.tid_of a)))) a_fixed_aid in
    a_fixed_tid
  in
  let rel_pp r =
    List.map (fun (a1,a2) -> action_pp a1,action_pp a2) r 
  in
  let action_set_rel_pp sr =
    List.map (fun (a,acts) -> action_pp a,(List.map action_pp acts)) sr 
  in
  let fault_pp = function
    | One acts -> One (List.map action_pp acts)
    | Two rel -> Two (rel_pp rel) in
  let exod_pp =
     {actions = List.map action_pp exwit.exod.actions;
      threads = List.map thread_pp exwit.exod.threads;
      lk = exwit.exod.lk;
      sb = rel_pp exwit.exod.sb;
      asw = rel_pp exwit.exod.asw;
      dd = rel_pp exwit.exod.dd;
      cd = rel_pp exwit.exod.cd;
      vconstraint = exwit.exod.vconstraint;
    } in
  let exed_pp =
    {rf = rel_pp exwit.exed.rf;
     sc = rel_pp exwit.exed.sc;
     mo = rel_pp exwit.exed.mo;
     lo = rel_pp exwit.exed.lo;
     ao = rel_pp exwit.exed.ao;
     tot = rel_pp exwit.exed.tot;
   } in
  let exdd_pp =
    {locations = exwit.exdd.locations;
     derived_relations = List.map (fun (nm, rel) ->
       (nm, rel_pp rel))
       exwit.exdd.derived_relations;
     undefined_behaviour = List.map (fun (nm, fault) ->
       (nm, fault_pp fault))
       exwit.exdd.undefined_behaviour;
   } in    
  let exwit_pp =
    {exod = exod_pp;
     exed = exed_pp;
     exdd = exdd_pp;
   } 
  in
  exwit_pp

let pp_ascii () (m,exod) = 

  let lo = layout_by_thread m true false exod in    

  (* build array of pp'd actions *)
  (* hmm - not sure if this digression into arrays is a good idea *)
  let pp = Array.make_matrix lo.size_x lo.size_y ""  in

  let rec do_pp_column axys = 
    match axys with 
    | [] -> () 
    | (a,(x,y))::axys' -> pp.(x).(y) <- (pp_action lo.relabelling () a) ; do_pp_column axys' in

  let rec do_pp_columns columns = 
    match columns with
    | [] -> ()
    | ((ch,(x,y)),axys)::columns' -> 
        pp.(x).(y) <- sprintf "%a" (pp_column_head lo.relabelling) ch;
        do_pp_column axys ; 
        do_pp_columns columns' in

  let _ = do_pp_columns lo.columns in

  (* pad *)
  let _ = 
    for x = 0 to lo.size_x - 1 do
      let max_length = Array.fold_left (fun n s -> max n (String.length s)) 0 pp.(x) in
      for y = 0 to lo.size_y - 1 do
        pp.(x).(y) <- pad max_length pp.(x).(y) ^ "  "
      done
    done in

  (* transpose *)
  let pp_transposed = Array.make_matrix lo.size_y lo.size_x ""  in

  let _ = 
    for x = 0 to lo.size_x - 1 do
      for y = 0 to lo.size_y - 1 do
        pp_transposed.(y).(x) <- pp.(x).(y) 
      done
    done in

  (* convert to string *)
  Array.fold_right 
    (function ys -> function s -> 
      ((Array.fold_right (^) ys  "\n") ^ s) )
    pp_transposed 
    ""


let pp_execution_opsem_data () m = function (exod:execution_opsem_data) -> pp_ascii () (m,exod)

(*
"actions: %a\ngc: %a\nstmt: %a\n" pp_actions actions pp_global_configuration gc  pp_stmt vs) in
  let _ = print_string (sprintf "%a\n\n" pp_all (List.map snd actions,gc.deps)) in
  ()

*)


exception NonLinearSB

let partition_faults faults =
  let (unary,binary) =
    List.fold_left
      (fun (un,bin) (nm,fault) -> match fault with
        | One acts -> ((nm,acts) :: un,bin)
        | Two rel -> (un,(nm,rel) :: bin))
      ([],[]) faults in
  (List.rev unary, List.rev binary)

let pp_dot () (m,testname,(exod,exedo,exddo)) = 

  let lo = layout_by_thread m true false exod in    

  let fontsize_node   = m.fontsize in
  let fontsize_edge   = m.fontsize in
  let fontsize_legend = m.fontsize in

  let fontname_node   = m.fontname in
  let fontname_edge   = m.fontname in
  let fontname_legend = m.fontname in
  
  let pp_attr () (attr,v) = match v with
  | "" -> ""
  | _  -> sprintf ", %s=\"%s\"" attr v in

  let pp_intattr () (attr,v) =
    sprintf ", %s=%i" attr v in

  let pp_floatattr () (attr,v) =
    sprintf ", %s=%s" attr (string_of_float v) in
  
  let pp_fontsize () f = pp_intattr () ("fontsize",f) in

  let pp_color () color = pp_attr () ("color",color) in

  let pp_fontcolor () color = pp_attr () ("fontcolor",color) in

  let pp_fontname () fontname = pp_attr () ("fontname",fontname) in
      
  let pp_extra () attr_value = match attr_value  with
  | "" -> ""
  | _  -> sprintf "%s" attr_value in

  let pl () = sprintf  "%s\n" in
  let pf () fmt = sprintf fmt in

  let escape_tex s =
    let buff = Buffer.create 16 in
    for k=0 to String.length s-1 do
      let c = s.[k] in
      begin match c with
      | '_' -> Buffer.add_char buff '\\'
      | _ -> ()
      end ;
      Buffer.add_char buff c 
    done ;
    Buffer.contents buff in

  let escape_dot s =
    let buff = Buffer.create 16 in
    for k=0 to String.length s-1 do
      let c = s.[k] in
      begin match c with
      | '\\' -> Buffer.add_char buff '\\'
      | _ -> ()
      end ;
      Buffer.add_char buff c 
    done ;
    Buffer.contents buff in
      
  let escape_label s = escape_dot (escape_tex s) in

  let pp_edge_label () (m, lbl) =
    (* escape_label lbl in *)
    if m.texmode then
      "\"" ^ String.concat "," (List.map (fun (l,c) -> "\\\\color{" ^ c ^ "}{" ^ l ^ "}") lbl) ^ "\""
    else "<" ^ String.concat "," (List.map (fun (l,c) -> "<font color=\"" ^ c ^ "\">" ^ l ^ "</font>") lbl) ^ ">" in

  let pp_node_name () a = sprintf "node%a" pp_action_id (Cmm.aid_of a) in

  let pp_column_head_node_name () (x,y) = sprintf "column%i%i" x y in

  let is_ur_or_dr lbls =
    match lbls with
        (* TODO: jp: the information of which relations are faults should be piped to here *)
      | [(lbl, _)] -> List.mem lbl ["ur";"dr"]
      | _ -> false in

  let pp_edge () m a1 a2 lbl colours style arrowsize extra_attr =
    let colour = String.concat ":" colours in
    sprintf "%a -> %a [label=%a%s%a%a%a%a%a%s%a]%a;\n"
      pp_node_name a1 
      pp_node_name a2  
      pp_edge_label (m, lbl)
      "" (* (if filled then ",style=\"filled\",labelfloat=\"true\"" else "") *)
      pp_attr ("color",colour)
      pp_fontname fontname_edge
      pp_fontsize fontsize_edge
      pp_attr ("style",style)
      pp_floatattr ("penwidth",m.penwidth)
      (if is_ur_or_dr lbl then ",constraint=false,arrowhead=\"none\"" else "")
      pp_attr ("arrowsize",arrowsize)
      pp_extra extra_attr  in

  let pp_point () n lbl color pos =
    sprintf "%s [label=\"\", shape=point%a%a];\n" 
      n
      pp_attr  ("color",color)
      pp_extra pos  in

  let max_x = lo.size_x -1 in
  let max_y = lo.size_y -1 in

  let xorigin=1.0 in 
  let yorigin=1.0 in


(* old-fashioned *)
(*   let xscale= match max_x with *)
(*   | _ -> 0.7 in *)
(*   let yscale= match max_y with *)
(*   | _ -> (match m.neato with *)
(*     | true -> 0.4 *)
(*     | false -> 0.7) in *)
(*   let action_position (x,y) =  *)
(*     (m.xscale *. float_of_int x +. xorigin), *)
(*     (m.yscale *. (float_of_int max_y -. (if y=0 then 0.25 else (float_of_int y -. 0.5))) +. yorigin) in *)
(* (\* new-fangled *\)     *)
(*   let xscale= match max_x with *)
(*   | _ -> 1.7 in *)
(*   let yscale= match max_y with *)
(*   | _ -> (match m.neato with *)
(*     | true -> 1.0 *)
(*     | false -> 0.7) in *)
  let action_position (x,y) = 
    (m.xscale *. float_of_int x +. xorigin),
    (m.yscale *. (float_of_int max_y -. (float_of_int y )) +. yorigin) in
    


  let pp_action_position () (x,y) = 
    let (x',y') = action_position (x,y) in
    sprintf "pos=\"%f,%f!\"" x' y' in

  let pp_init_rf_position () (x,y) = 
    let (x',y') = action_position (x,y) in
    sprintf "pos=\"%f,%f!\"" (x' -. 1.25) (y'+. 0.25) in
  

  let (unary_faults,binary_faults) =
    match exddo with
      | None -> ([],[])
      | Some exdd -> partition_faults exdd.undefined_behaviour in

  let faulty_action_ids =
    List.concat (List.map (fun (_,acts) -> List.map Cmm.aid_of acts) unary_faults) in

  let axygeometry = sprintf "[margin=\"0.0,0.0\"][fixedsize=\"true\"][height=\"%f\"][width=\"%f\"]" m.node_height m.node_width in
  let chgeometry = "[margin=\"0.0,0.0\"][fixedsize=\"false\"][height=\"0.15\"][width=\"0.1\"]" in

  let pp_axy () color rank (a,(x,y)) =
    sprintf 
      "%a [shape=plaintext%a%a%s%a] %s [label=\"%s\", %a] %s;\n"
      pp_node_name a
      pp_fontname fontname_node
      pp_fontsize fontsize_node
      (if m.filled then ", style=\"filled\"" else "")
      pp_fontcolor color
      rank
      (((pp_action' m lo.relabelling) () a))
      pp_action_position (x,y) 
      axygeometry 
  in


  let pp_column_head_node () (color,rank,(ch,(x,y))) =
    sprintf 
      "%a [shape=box%a%a] %s [label=\"%s\" %a] %s ;\n"
      pp_column_head_node_name (x,y)
      pp_fontsize fontsize_node
      pp_color color
      rank
      (escape_label ((pp_column_head lo.relabelling) () ch))
      pp_action_position (x,y) 
      chgeometry 
  in

  let pp_constraint_node_name = "constraint" in
  let pp_constraint_name = "Constraint:\\n" in
  let pp_constraint_color = "" in
  let pp_constraint_rank = "" in
  (* The positioning depends on layout always adding an additional +2 at the end *)
  let pp_constraint_x = 0 in
  let pp_constraint_y = max_y - 1in

  let pp_constraint () vc = 
    if vc = Constraints.ctrue then "" 
    else
      pl () "/* Global value constraint */\n"
      ^ sprintf "%s [shape=box%a%a%a] %s [label=\"%s%s\" %a] %s;\n"
	  pp_constraint_node_name
	  pp_fontname fontname_node
	  pp_fontsize fontsize_node
	  pp_color pp_constraint_color
	  pp_constraint_rank
	  pp_constraint_name 
	  (escape_label (Constraints.pp_vconstraint () vc)) 
	  pp_action_position (pp_constraint_x,pp_constraint_y)
	  chgeometry
  in

  let rec pp_axys () axys = 
    match axys with 
    | [] -> "" 
    | (a,(x,y))::axys' ->
      let color = if List.mem (Cmm.aid_of a) faulty_action_ids then "darkorange" else "" in
      pp_axy () color "" (a,(x,y)) ^  pp_axys () axys' in

  let rec pp_columns () columns = 
    match columns with
    | [] -> ""
    | ((ch,(x,y)),axys)::columns' -> 
        pl () "/* column */\n" 
        ^ sprintf "%s%a%a" 
          "" (* (if m.neato then pp_column_head_node () ("","",(ch,(x,y))) else "")*)
          pp_axys axys  
          pp_columns columns' in

(*   let pp_relation () (lbl,color,edges) = *)
(*     "/* relation "^lbl^" */\n" *)
(*     ^ String.concat "" (List.map (function (a1,a2) -> pp_edge () a1 a2 lbl color "" "") edges) in *)
(* graphviz colours: http://www.graphviz.org/doc/info/colors.html  *)

  (* NB: the colours in pp.ml and in notsocommon.sty have to be kept
  consistent manually *)
  let relations = 
    [ ("sb","black", transitive_reduction exod.sb);
      ("dd","magenta", transitive_reduction exod.dd);
      ("cd","magenta", transitive_reduction exod.cd);
      ("asw","deeppink4",exod.asw) ] 
    @
      (match exedo with None -> [] | Some exed ->
        [ ("rf",  "red",   exed.rf);
          ("sc",  "orange", transitive_reduction exed.sc);
          ("mo",  "blue",  transitive_reduction exed.mo);
          ("lo",  "gray54", transitive_reduction exed.lo);
          ("ao",  "black", exed.ao);
          ("tot", "blue", transitive_reduction exed.tot)])
    @
      (match exddo with None -> [] | Some exdd ->
        (* TODO: jp: make this generic *)
        let colour_scheme = [
          ("sw", "deeppink4");
          ("rs", "black");
          ("hrs", "black");
          ("cad", "deeppink4");
          ("dob", "deeppink4");
          ("ithb", "forestgreen");
          ("hb", "forestgreen");
          ("vse", "brown");
          ("vsses", "brown4");
          ("dummy", "white")
        ] in
        let try_to_transitive_reduce rel = if is_transitive rel then try transitive_reduction rel with Transitive -> rel else rel in
        (* Note: doing the reduction on each relation is expensive *)
        let colour_and_prepare (nm, rel) =
          (nm,
           (try List.assoc nm colour_scheme with Not_found -> "black"),
           try_to_transitive_reduce rel) in
        List.map colour_and_prepare exdd.derived_relations
        @
        let relation_faults =
          List.map
            (fun (nm, rel) -> ((try List.assoc nm Atomic.short_names with Not_found -> nm), rel))
            binary_faults in
        List.map (fun (nm, rel) -> (nm, "darkorange", symmetric_reduction rel)) relation_faults) in

  let debug s = () (* print_string s;flush stdout *) in 
  let relayout_downwards columns = 
    try
      let relayout_downwards_reln = 
        transitive_reduction 
          (transitive_closure
             (reflexive_reduction
                (List.flatten 
                   (option_map 
                      (function (e,c,r) -> 
                        if List.mem e ["dr";"ur"] then None
                        else Some r)
                      relations)))) in
      let check_all_linear_downwards =
        let _,_,sbrel = 
          List.find (fun (name,_,r) -> name = "sb") relations in 
        let rec check_related_by_sb rel =
          match rel with
          | [] -> ()
          | (a,_) :: rel' ->
              if List.exists (fun (a',_) -> List.mem (a,a') sbrel) rel' 
              then check_related_by_sb rel'
              else raise NonLinearSB
        in
        List.iter
          (fun (_,actions) -> check_related_by_sb actions)
          columns
      in    
      let r = ref relayout_downwards_reln in
      let print_r () = debug "r = \n"; List.iter (function (a',b') -> debug (sprintf "   <%a, %a>\n" (pp_action lo.relabelling) a' (pp_action lo.relabelling)  b')) !r; debug "" in
      let () = print_r() in
      let n = List.length columns in
      let a_todo = Array.of_list (List.map (function (_,axys) ->axys) columns) in
      let chs = Array.of_list (List.map (function (ch,_) ->ch) columns) in
      let a_done = Array.make n [] in
      let y_next = Array.make n 0 in
      let y_next' = Array.make n 0 in
      let newly_done = ref [] in
      let print_axy (a,(x,y)) = debug (sprintf "(%a,(%n,%n))[%n] " (pp_action lo.relabelling) a x y (lo.column_of a)) in
      let print_axys axys = List.iter print_axy axys; debug "\n" in 
      let () =
        debug "a_todo: ";
        for i = 0 to n-1 do
          debug (Printf.sprintf "@%i = " i); print_axys (a_todo.(i))
        done;
        debug "\n" 
      in
      let print_r () = debug "r = \n"; List.iter (function (a',b') -> debug (sprintf "   <%a, %a>\n" (pp_action lo.relabelling) a' (pp_action lo.relabelling)  b')) !r; debug "" in
      while Array.fold_right (function a_s -> function b -> (a_s <> [] || b)) a_todo false do
        let _ = read_line () in
        debug "\nnew round:\n";
        print_r ();
        debug "a_todo: ";
        for i = 0 to n-1 do
          debug (Printf.sprintf "@%i = " i); print_axys (a_todo.(i))
        done;
        debug "\n" ;
        debug "y_next:  ";for i = 0 to n-1 do debug (sprintf "%n  " y_next.(i)) done; debug "\n";
        newly_done := [];
        for i = 0 to n-1 do
          match a_todo.(i) with
          | [] -> ()
          | (a,(x,y))::axys' -> 
              if List.exists (function (a',b') -> b'=a) (!r) then ()
              else (
                a_todo.(i)<- axys' ;
                let axy' = (a,(x,y_next.(i))) in
                print_axy axy';
                a_done.(i)<- a_done.(i) @ [axy'];
                newly_done := a :: (!newly_done);
               )
        done;
        debug "newly done: ";
        List.iter (function a -> debug (pp_action lo.relabelling () a)) (!newly_done); debug "\n";
        for i= 0 to n-1 do
          y_next'.(i) <- 
            List.fold_left max
              (y_next.(i) + (if List.exists (function a -> lo.column_of a = i) !newly_done then 1 else 0) )
              ( option_map 
                  (function (a',b') -> 
                    if List.mem a' (!newly_done) && lo.column_of b' = i 
                    then Some (1 + y_next.(lo.column_of a')) 
                    else None)
                  !r )
        done;
        for i = 0 to n-1 do 
          y_next.(i) <- y_next'.(i) 
        done;
        r := List.filter (function (a',b') -> not (List.mem a' (!newly_done))) (!r)
      done;
      Array.to_list (Array.mapi (fun i axys' -> ( chs.(i), a_done.(i))) a_done )
    with 
      Transitive -> 
        debug "relayout_downwards invoked on a transitive set of relations\n"; 
        columns 
    |  NonLinearSB -> 
        debug "relayout_downwards invoked on structure with non-linear sequenced-before relations\n"; 
        columns  in

  let relayout_par_init columns = 
    let column0::columns' = columns in
    let y_start = match column0 with (_,axys) -> List.length axys in
    column0 :: List.map (function (ch,axys) -> (ch, List.map (function (a,(x,y))->(a,(x,y+y_start))) axys)) columns' in
  
  let lo = 
    let columns' = match m.layout with
    | LO_neato_downwards -> relayout_downwards lo.columns 
    | LO_neato_par_init -> relayout_par_init lo.columns
    | LO_dot | LO_neato_par -> lo.columns 
    in
    { lo 
    with 
      columns = columns';
      size_y = List.fold_left max 0 
        (List.map 
           (function (ch,axys) -> 
             1+
               List.fold_right 
               (function (a,(x,y)) -> function y' -> max y y')
               axys
               0
           ) 
           columns')
    } in

  let flattened = List.flatten (List.map (function (e,c,r) -> (List.map (function (a1,a2) -> (e,c,a1,a2)) r)) relations) in

  let source_target_pairs = remove_duplicates (List.map (function (e,c,a1,a2)->(a1,a2)) flattened) in
  
  let glommed_edges = 
    List.map 
      (function (a1',a2') -> 
        let parallel_edges = List.filter (function (e,c,a1,a2)->a1=a1'&&a2=a2') flattened in
        let (_,_,a1,a2) = List.hd parallel_edges in 
(* the following would make multiple labels appear vertically, but the overall layout produced by graphviz is often much worse *)
(*        let labels = "\\\\ml{"^String.concat "\\\\\\\\" (List.map (function (e,_,_,_)-> "\\\\"^e) parallel_edges)^"}" in *)
        let labels = List.map (function (e,c,_,_) -> (e,c)) parallel_edges in
(*         let non_hb_colours = remove_duplicates (option_map (function (e,c,_,_) -> match e with "hb"->None |_->Some c) parallel_edges) in *)
(*         let colour = match non_hb_colours with [c]->c | _ -> "black" in *)
        let colours = remove_duplicates (option_map (function (_,c,_,_) -> Some c) parallel_edges) in
        let arrowsize = match List.length colours with 
        | 1 -> "0.8"
        | 2 -> "1.0"
        | _ -> "1.2" in
        (labels,colours,arrowsize,a1,a2))
      source_target_pairs in

  let pp_graph () legend =
    "digraph G {\n" 
(* this gives *different* results to invoking dot/neato on the command-line *)
(*     ^ " layout = "^(match m.layout with Dot -> "dot" | _ -> "neato") ^"\n"  *)
    ^ " splines=true;\n"
    ^ " overlap=false;\n"
    ^ " ranksep = "^string_of_float m.ranksep ^";\n"  
    ^ " nodesep = "^string_of_float m.nodesep ^";\n" 
(*    ^ " fontname = \""^fontname_graph^"\";\n"*)
    ^ "/* legend */\n" 
    ^ pf () "fontsize=%i fontname=\"%s\" label=\"%s\"; \n\n" fontsize_legend fontname_legend legend 
    ^ "/* columns */\n" 
    ^ pf () "%a" pp_columns lo.columns
    ^ pf () "%a" pp_constraint exod.vconstraint
    ^ String.concat "" (List.map (function (labels,c,arrowsize,a1,a2) -> pp_edge () m a1 a2 labels c "" arrowsize "") glommed_edges)
    ^ "}" in

   let legend = match m.legend with 
   | None -> ""
   | Some "filename" -> (match testname with None -> "" | Some testname -> escape_label testname) 
   | Some s -> s in
   
   pp_graph () legend 






let pp_isa () (m,testname,(exod,exedo,exddo)) = 

  let exed = match exedo with Some exed->exed | None -> error "pp_isa called without an exed\n" in

  (* run layout just to get a readable organisation *)
  let lo = layout_by_thread m false false exod in    
  let rl = lo.relabelling in 

  (* build array of pp'd actions *)
  (* hmm - not sure if this digression into arrays is a good idea *)
(*   let pp = Array.make_matrix lo.size_x lo.size_y ""  in *)

(*   let rec pp_column' axys =  *)
(*     match axys with  *)
(*     | [] -> [] *)
(*     | (a,(x,y))::axys' -> (pp_isa_action rl () a) :: pp_column' axys' in *)

  let pp_isa_relation rl rname pairs = 
     "definition \""^rname^" \\<equiv> {" ^ String.concat (",\n    ") (List.map (function (a1,a2) -> sprintf "(%a, %a)" (pp_isa_action rl) a1 (pp_isa_action rl) a2) pairs) ^ "}\"\n" in

(*   let pp_thread ind ((ch,_),axys) =  *)
(*     let tid = match ch with CH_tid tid -> tid | CH_loc _ -> error "internal error: CH_loc in pp_thread" in *)
(*     let thread = List.find (fun t -> t.thread_id=tid) exod.threads in *)
(*       sprintf   "(| thread_id = %a,\n" pp_thread_id tid  *)
(*     ^ sprintf "%s   actions = {%s},\n" ind (String.concat (",\n"^ind^"               ") (pp_column' axys))  *)
(*     ^ sprintf "%s   threadwise_sequenced_before = \n%s,\n" ind (pp_isa_relation "           " rl thread.threadwise_sequenced_before) *)
(*     ^ sprintf "%s   threadwise_data_dependency = \n%s,\n" ind (pp_isa_relation "           " rl thread.threadwise_data_dependency) *)
(*     ^ sprintf "%s   threadwise_control_dependency = \n%s\n" ind (pp_isa_relation "           " rl thread.threadwise_control_dependency) *)
(*     ^ sprintf "%s   |)" ind in *)


  (* TODO: fix this for mixed accesses *)
  let location_kinds = 
    remove_duplicates 
      (option_map 
         (function a -> match Cmm.loc_of a with 
         | None -> None 
         | Some l -> match a with 
           | Cmm.Lock (aid,tid,l,oc) -> Some (l,Cmm.Mutex)
           | Cmm.Unlock (aid,tid,l) -> Some (l,Cmm.Mutex)
           | Cmm.Load (aid,tid,mo,l,v) -> Some (l,Cmm.Atomic)
           | Cmm.Store (aid,tid,mo,l,v) -> Some (l,Cmm.Atomic)
           | Cmm.RMW (aid,tid,mo,l,v1,v2) -> Some (l,Cmm.Atomic)
           | Cmm.Blocked_rmw (aid,tid,l) -> Some (l,Cmm.Atomic)
           | Cmm.Fence (aid,tid,mo) -> raise (Failure "location_kind on Fence"))
         exod.actions) in
  
  let pp_isa_set rl nm pp_elem set =
    sprintf "definition \"%s \\<equiv> {%s}\"\n" nm (String.concat (",\n    ") (List.map (pp_elem rl ()) set)) in
  
  let pp_isa_action_set rl nm acts =
    pp_isa_set rl nm pp_isa_action acts in

  let pp_exod = 
    pp_isa_action_set rl "actions" exod.actions
    ^ pp_isa_set rl "threads" pp_isa_thread_id exod.threads
    ^ sprintf "definition \"lk \\<equiv> (\\<lambda> _.undefined)(%s)\"\n" (String.concat (", ") (List.map (function (l,lk) -> sprintf "%a:= %a" pp_isa_loc l pp_isa_location_kind lk) exod.lk))
    ^ pp_isa_relation rl "sb" exod.sb 
    ^ pp_isa_relation rl "dd" exod.dd
    ^ pp_isa_relation rl "cd" exod.cd
    ^ pp_isa_relation rl "asw" exod.asw in
    (* No place to put vconstraint *)

  let pp_exedo = match exedo with None -> "" | Some exed ->
      pp_isa_relation rl "rf" exed.rf
    ^ pp_isa_relation rl "sc" exed.sc
    ^ pp_isa_relation rl "mo" exed.mo
    ^ pp_isa_relation rl "lo" exed.lo
    ^ pp_isa_relation rl "ao" exed.ao
    ^ pp_isa_relation rl "tot" exed.tot
  in

  let pp_exddo = match exddo with None -> "" | Some exdd ->
    String.concat "" (List.map (fun (nm, rel) -> pp_isa_relation rl nm rel) exdd.derived_relations)
    ^
    String.concat "" (List.map (fun (nm, fault) -> let nm = List.assoc nm Atomic.short_names in match fault with
      | One acts -> pp_isa_action_set rl nm acts
      | Two rel -> pp_isa_relation rl nm rel
    ) exdd.undefined_behaviour)
  in

  let rec pp_isa_escape s = 
    String.concat "" 
      (List.map 
       (fun c -> 
         if c='/' then "SS" else 
         if c='.' then "DD" else
         String.make 1 c) 
       (char_list_of_string s))
  in

  let theoryname = pp_isa_escape testname in

  let pp_execution =
      sprintf "(*<*)header {* %s *}\ntheory \"%s\" imports Main Atomic\nbegin(*>*)\n" testname theoryname
    ^ pp_exod
    ^ pp_exedo
    ^ pp_exddo

    ^ sprintf "\nlemma \"consistent_low_level_execution actions threads lk sb asw dd cd rf mo sc\"\n\nsorry\n"
    ^ sprintf "(*<*)end(*>*)\n"
  in

  pp_execution
    
(* stolen from OCaml 4.00.0 *)
let string_map f s =
  let l = String.length s in
  if l = 0 then s else begin
    let r = String.create l in
    for i = 0 to l - 1 do String.unsafe_set r i (f(String.unsafe_get s i)) done;
    r
  end

let pp_tex () (m,testname,(exod,exedo,exddo)) = 

  let exed = match exedo with Some exed->exed | None -> error "pp_tex called without an exed\n" in

  (* run layout just to get a readable organisation *)
  let lo = layout_by_thread m false false exod in    
  let rl = lo.relabelling in 

  (* build array of pp'd actions *)
  (* hmm - not sure if this digression into arrays is a good idea *)
(*   let pp = Array.make_matrix lo.size_x lo.size_y ""  in *)

(*   let rec pp_column' axys =  *)
(*     match axys with  *)
(*     | [] -> [] *)
(*     | (a,(x,y))::axys' -> (pp_tex_action rl () a) :: pp_column' axys' in *)
  let pp_tex_arrow rname = "\\ttran{"^rname^"}" in

  let pp_tex_relation rl rlongname rname pairs = 
   let key (a1,a2) = ((pp_action_id' rl) () (Cmm.aid_of a1), (pp_action_id' rl) () (Cmm.aid_of a2)) in
   let sorted_pairs = List.sort (fun (a1,a2) (b1,b2) -> compare (key (a1,a2)) (key (b1,b2))) pairs in
     "\\noindent\\mbox{\\quad} \\textit{"^rlongname^"} $"^pp_tex_arrow rname^" = \\{$ " ^ String.concat (",\\ ") (List.map (function (a1,a2) -> sprintf "$%a %s %a$" (pp_action_id' rl) (Cmm.aid_of a1) (pp_tex_arrow ""(*rname*)) (pp_action_id' rl) (Cmm.aid_of a2)) sorted_pairs) ^ " $\\}$\\\\\n" in

(*   let pp_thread ind ((ch,_),axys) =  *)
(*     let tid = match ch with CH_tid tid -> tid | CH_loc _ -> error "internal error: CH_loc in pp_thread" in *)
(*     let thread = List.find (fun t -> t.thread_id=tid) exod.threads in *)
(*       sprintf   "(| thread_id = %a,\n" pp_thread_id tid  *)
(*     ^ sprintf "%s   actions = {%s},\n" ind (String.concat (",\n"^ind^"               ") (pp_column' axys))  *)
(*     ^ sprintf "%s   threadwise_sequenced_before = \n%s,\n" ind (pp_tex_relation "           " rl thread.threadwise_sequenced_before) *)
(*     ^ sprintf "%s   threadwise_data_dependency = \n%s,\n" ind (pp_tex_relation "           " rl thread.threadwise_data_dependency) *)
(*     ^ sprintf "%s   threadwise_control_dependency = \n%s\n" ind (pp_tex_relation "           " rl thread.threadwise_control_dependency) *)
(*     ^ sprintf "%s   |)" ind in *)

  let sorted_threads = List.sort (fun tid1 tid2 -> compare tid1 tid2) exod.threads in

  let actions_per_thread = List.map (function tid -> (tid,List.filter (function a -> Cmm.tid_of a = tid) exod.actions)) sorted_threads in



  let pp_exod = 
   "\\noindent Data from the operational semantics ($\\exod$):\\\\\n"
    ^ sprintf "\\noindent\\mbox{\\quad} $\\textit{threads} = \\{ %s \\}$\\\\\n" (String.concat (",\\ ") (List.map (pp_thread_id' rl ()) sorted_threads ))

    ^ "{\\renewcommand{\\raiseaction}[1]{\\mbox{#1}}\n"
    ^ sprintf "\\noindent\\mbox{\\quad} $\\textit{actions} =$\\\\\n" 
    ^ (String.concat "" (List.map (function (tid,actions) -> " \\mbox{\\quad\\quad} Thread $"^pp_thread_id' rl () tid^": \\{" ^ String.concat ",\\ " (List.map (pp_tex_action' {m with texmode=true } rl ()) actions) ^ "\\}$\\\\\n") actions_per_thread))
    ^ "}\n"
    ^ "\\noindent\\mbox{\\quad} $\\textit{location-kinds} = \\{" ^ (String.concat (",\\ ") (List.map (function (l,lk) -> sprintf "%a \\mapsto %a" pp_loc l pp_tex_location_kind lk) exod.lk)) ^ "\\}$\\\\\n"
    ^ pp_tex_relation rl "sequenced-before" "sb" exod.sb 
    ^ pp_tex_relation rl "data-dependency" "dd" exod.dd
    ^ pp_tex_relation rl "control-dependency" "cd" exod.cd
    ^ pp_tex_relation rl "additional-synchronized-with" "asw" exod.asw in
    (* No place to put vconstraint *)

  let pp_exedo = match exedo with None -> "" | Some exed ->
    "\\noindent Existential witness data ($\\exed$):\\\\\n"
    ^ pp_tex_relation rl "reads-from" "rf" exed.rf
    ^ pp_tex_relation rl "SC-order" "sc" exed.sc
    ^ pp_tex_relation rl "modification-order" "mo" exed.mo
    ^ pp_tex_relation rl "lock-order" "lo" exed.lo
    ^ pp_tex_relation rl "a-order" "ao" exed.ao
    ^ pp_tex_relation rl "total-order" "tot" exed.tot in

  let replace_underscore_by_hyphen =
    string_map (fun c -> if c = '_' then '-' else c) in

  let pp_tex_action_set rl long_name short_name acts =
    sprintf "\\noindent\\mbox{\\quad} \\textit{%s} (%s) = \\{%s\\}\\\\\n" (replace_underscore_by_hyphen long_name) short_name (String.concat (",\\ ") (List.map (function a -> pp_action_id' rl () (Cmm.aid_of a)) acts)) in

  let pp_exddo = match exddo with None -> "" | Some exdd ->
    "\\noindent Derived relations:\\\\\n"
    ^
    String.concat "" (List.map (fun (nm, rel) -> pp_tex_relation rl (try List.assoc nm Atomic.long_names with Not_found -> nm) nm rel) exdd.derived_relations)
    ^
    String.concat "" (List.map (fun (nm, fault) ->
      let short_name = (try List.assoc nm Atomic.short_names with Not_found -> nm) in
      match fault with
        | One acts -> pp_tex_action_set rl nm short_name acts
        | Two rel -> pp_tex_relation rl nm short_name rel)
                        exdd.undefined_behaviour)
  in

  let pp_execution =
    (match testname with
      | None -> ""
      | Some testname -> sprintf "\\noindent An execution of test: %s \\\\\n" testname)
    ^ pp_exod
    ^ pp_exedo
    ^ pp_exddo
    ^ sprintf "\n" 
  in

  pp_execution
    




let pp_execfile () (model_name,m,testname,(exod,exedo,exddo)) =
  (* run layout just to get a readable organisation *)
  let lo = layout_by_thread m false false exod in    
  let rl = lo.relabelling in 

  let pp_action () = function a -> match a with
  | Cmm.Lock (aid,tid,l,oc) -> 
     sprintf "%a;%a:L %a %a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_loc l  pp_outcome oc
  | Cmm.Unlock (aid,tid,l) ->
     sprintf "%a;%a:U %a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_loc l 
  | Cmm.Load (aid,tid,mo,l,v) ->
     sprintf "%a;%a:R%a %a=%a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_memory_order_enum2 mo  pp_loc l  pp_value v
  | Cmm.Store (aid,tid,mo,l,v) ->
     sprintf "%a;%a:W%a %a=%a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_memory_order_enum2 mo  pp_loc l  pp_value v
  | Cmm.RMW (aid,tid,mo,l,v1,v2) ->
     sprintf "%a;%a:RMW%a %a=%a/%a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_memory_order_enum2 mo  pp_loc l  pp_value v1  pp_value v2
  | Cmm.Blocked_rmw (aid,tid,l) ->
     sprintf "%a;%a:BRMW%a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_loc l
  | Cmm.Fence (aid,tid,mo) ->
     sprintf "%a;%a:F %a" (pp_action_id' rl) aid  (pp_thread_id' rl) tid  pp_memory_order_enum2 mo
  in

  let pp_actions =
    "% The actions:\n" 
    ^ (String.concat "\n" (List.map (pp_action ()) exod.actions))   
    ^ "\n\n"
  in

  let pp_reln name r =
    if List.length r = 0 then sprintf "%% Relation %s: EMPTY\n" name
    else
      sprintf "%% Relation %s:\n" name
      ^ (String.concat "\n" 
           (List.map 
              (fun (a1,a2) -> 
                sprintf "%s: %a --> %a" name  (pp_action_id' rl) (Cmm.aid_of a1)  (pp_action_id' rl) (Cmm.aid_of a2)) r))
      ^ "\n\n"
  in

  let pp_lk = 
    "location_kinds: {"
    ^ String.concat ", " (List.map (function (l,lk) -> Printf.sprintf "%a:%a" (pp_loc) l  pp_location_kind lk) exod.lk)
    ^ "}\n" in

  let pp_model_name =
    (* TODO: jp: make this more robust *)
    if model_name = Atomic.default_model_name then ""
    else "model:" ^ model_name ^ "\n"
  in

  let pp_exod =
    pp_actions
    (* Not output: threads *)
    ^ pp_lk 
    ^ pp_reln "sb" (transitive_reduction exod.sb)
    ^ pp_reln "asw" exod.asw
    ^ pp_reln "dd" exod.dd
    ^ pp_reln "cd" exod.cd
    (* Not output: vconstraint *)
  in

  let pp_exed =
    match exedo with
    | None -> ""
    | Some exed ->
        pp_reln "rf" exed.rf
        ^ pp_reln "sc" (transitive_reduction exed.sc)
        ^ pp_reln "mo" (transitive_reduction exed.mo)
        (* TODO: jp: which of these relations should be transitive-reduced? (see execfile.ml) *)
        ^ pp_reln "lo" exed.lo
        ^ pp_reln "ao" exed.ao
        ^ pp_reln "tot" exed.tot
  in

  let pp_exdd =
    match exddo with
    | None -> ""
    | Some exdd ->
        "" (* Not output: locations *)
        ^ String.concat "" (List.map (fun (nm, rel) -> pp_reln nm rel) exdd.derived_relations)
        ^ String.concat "" (List.map (fun (nm, fault) ->
          let nm = (try List.assoc nm Atomic.short_names with Not_found -> nm) in
          match fault with
            | One _ -> "" (* Not output: faults on a single action *)
            | Two rel -> pp_reln nm rel
          ) exdd.undefined_behaviour)
  in

  let pp_execution =
    (match testname with
      | None -> ""
      | Some testname -> sprintf "%% Generated from %s\n" testname)
    ^ pp_model_name
    ^ pp_exod
    ^ pp_exed
    ^ pp_exdd

  in
  pp_execution

let pp_skeletonexecfile () (m,testname,ex_skeleton) =
  (* run layout just to get a readable organisation *)
(*   let lo = layout_by_thread m false false exod in     *)
(*   let rl = lo.relabelling in  *)
  let rl = [],[] in
  let pp_actions =
    "% The actions:\n" 
    ^ (String.concat "\n" (List.map (pp_skeleton_action rl ()) ex_skeleton.es_actions))   
    ^ "\n\n"
  in

  let pp_reln name r =
    if List.length r = 0 then sprintf "%% Relation %s: EMPTY\n" name
    else
      sprintf "%% Relation %s:\n" name
      ^ (String.concat "\n" 
           (List.map 
              (fun (a1,a2) -> 
                sprintf "%s: %a --> %a" name  (pp_skeleton_action_id' rl) (skeleton_action_id_of a1)  (pp_skeleton_action_id' rl) (skeleton_action_id_of a2)) r))
      ^ "\n\n"
  in
  let pp_sameloc = 
    "%% Sameloc:\n" ^ 
    "sameloc: "^ 
    "{ " ^ String.concat ", " (List.map (function sl -> "{" ^ (String.concat "," (List.map (fun a -> pp_skeleton_action_id' rl () (skeleton_action_id_of a)) sl))^"}") ex_skeleton.es_sameloc) ^ " }\n" in

  let pp_atomiclocs = 
    "%% Atomiclocs:\n" ^ 
    "atomiclocs: "^ 
     "{" ^ (String.concat "," (List.map (fun a -> pp_skeleton_action_id' rl () (skeleton_action_id_of a)) ex_skeleton.es_atomiclocs)) ^ " }\n" in

  let pp_ex_skel =
    pp_actions
    (* Not output: threads *)
    (* Not output: lk *)
    ^ pp_sameloc 
    ^ pp_atomiclocs
    ^ pp_reln "sb" ((*transitive_reduction*) ex_skeleton.es_sb)
    ^ pp_reln "asw" ex_skeleton.es_asw
    ^ pp_reln "dd" ex_skeleton.es_dd
    ^ pp_reln "cd" ex_skeleton.es_cd
    ^ pp_reln "rf" ex_skeleton.es_rf
   (*    ^ pp_reln "sc" ex_skeleton.es_sc *)
    ^ pp_reln "mo" ex_skeleton.es_mo
    (* TODO: jp: what about lo, ao, and tot? *)
    (* Not output: vconstraint *)
  in

  let pp_skeleton_execution =
    sprintf "%% Generated from %s\n" testname
    ^ pp_ex_skel

  in
  pp_skeleton_execution

type dsp_verbosity = Dsp_verbose | Dsp_quiet

let pp_dsp () (verbosity,default,exod,instrs) =
  let verbose = (match verbosity with Dsp_verbose -> true | Dsp_quiet -> false) in
  let lo = layout_by_thread instrs.mode false false exod in    
  let rl = lo.relabelling in 
  let pp_show = 
    match instrs.show with
    | None -> ""
    | Some i -> sprintf "%%SHOW \nshow %i.\n" i
  in

  let pp_command () = 
    function
      | Quit -> "quit."
      | Continue -> "continue."
      | StopAt Never -> "non_stop."
      | StopAt Always -> "intermediate."
      | StopAt OnCandidates -> "next_candidate."
      | StopAt OnSolutions -> "next_consistent."
      | Help -> "help."
      | Relabel -> "relabel."
      | Generate (Dot,fname) -> sprintf "generate dot %s" fname
      | Generate (Isa,fname) -> sprintf "generate isa %s" fname
      | Generate (Exc,fname) -> sprintf "generate exc %s" fname
      | Generate (Tex,fname) -> sprintf "generate tex %s" fname
      | Generate (Instructions,fname) -> sprintf "generate dsp %s" fname
  in

  let pp_commands =
    if (List.length instrs.commands > 0) then
      "%%COMMANDS \n"
      ^ (String.concat "\n" (List.map (pp_command ()) instrs.commands))
      ^ "\n"
    else ""
  in

  let pp_constrain_rels =
    if (List.length instrs.constrain_rels > 0) then
      "%%CONSTRAIN RELATIONS \n"
      ^ (String.concat "\n" 
           (List.map 
              (fun (e,pairs) -> 
                "consider " 
                ^ String.concat " " 
                    (List.map 
                       (fun (a1,a2) ->                 
                         sprintf "%a --%s--> %a" pp_action_id a1  e  pp_action_id a2)
                       pairs)
                ^ ".")
              instrs.constrain_rels))
      ^ "\n"
    else ""
  in

  let pp_remove_rels =
    if (List.length instrs.remove_rels > 0) then
      "%%REMOVE RELATIONS \n"
      ^ (String.concat "\n"
           (List.map 
              (fun (e,pairs) -> 
                "remove " 
                ^ String.concat " " 
                    (List.map 
                       (fun (a1,a2) ->                 
                         sprintf "%a --%s--> %a" (pp_action_id' rl) (Cmm.aid_of a1)  e  (pp_action_id' rl) (Cmm.aid_of a2))
                       pairs)
                ^ ".")
              instrs.remove_rels))
      ^ "\n"
    else ""
  in

  let pp_add_rels =
    if (List.length instrs.add_rels > 0) then
      "%%ADD RELATIONS \n"
      ^ (String.concat "\n"
           (List.map 
              (fun (e,pairs) -> 
                "add " 
                ^ String.concat " " 
                    (List.map 
                       (fun (a1,a2) ->                 
                         sprintf "%a --%s--> %a" (pp_action_id' rl) (Cmm.aid_of a1)  e  (pp_action_id' rl) (Cmm.aid_of a2))
                       pairs)
                ^ ".")
              instrs.add_rels))
      ^ "\n"
    else ""
  in

  let pp_remove_actions =
    if (List.length instrs.remove_actions > 0) then
      "%%REMOVE ACTIONS \n"
      ^ (String.concat "\n"
           (List.map (fun a -> sprintf "remove %a." (pp_action rl) a) instrs.remove_actions))
      ^ "\n"
    else ""
  in
  
  let pp_add_actions =
    if (List.length instrs.add_actions > 0) then
      "%%ADD ACTIONS \n"
      ^ (String.concat "\n"
           (List.map (fun a -> sprintf "add %a." (pp_action rl) a) instrs.add_actions))
      ^ "\n"
    else ""
   in

   let pp_mode =
   let m = instrs.mode in
    "%%PPMODE \n" 
   ^ (if verbose || m.fontsize   <>default.fontsize    then sprintf "set fontsize=%d\n"    m.fontsize   else "") 
   ^ (if verbose || m.node_height<>default.node_height then sprintf "set node_height=%f\n" m.node_height else "") 
   ^ (if verbose || m.node_width <>default.node_width  then sprintf "set node_width=%f\n"  m.node_width  else "") 
   ^ (if verbose || m.filled     <>default.filled      then sprintf "set filled    =%B\n"  m.filled      else "") 
   ^ (if verbose || m.xscale     <>default.xscale      then sprintf "set xscale    =%f\n"  m.xscale      else "") 
   ^ (if verbose || m.yscale     <>default.yscale      then sprintf "set yscale    =%f\n"  m.yscale      else "") 
   ^ (if verbose || m.ranksep    <>default.ranksep     then sprintf "set ranksep   =%f\n"  m.ranksep     else "") 
   ^ (if verbose || m.nodesep    <>default.nodesep     then sprintf "set nodesep   =%f\n"  m.nodesep     else "") 
   ^ (if verbose || m.penwidth   <>default.penwidth    then sprintf "set penwidth  =%f\n"  m.penwidth    else "") 
   ^ (if verbose || m.legend     <>default.legend      then sprintf "set legend    =%a\n"  pp_legend m.legend else "") 
   ^ (if m.layout     <>default.layout      then sprintf "set layout    =%a\n"  pp_layout m.layout else "") 
   ^ (if m.texmode    <>default.texmode     then sprintf "set tex       =%B\n"  m.texmode     else "") 
   ^ (if verbose || m.thread_ids    <>default.thread_ids     then sprintf "set thread_ids=%B\n"  m.thread_ids     else "") in

(*    ^ ".\n" *)

  let pp_ignorechecks =
    if (List.length instrs.ignorechecks > 0) then
      "%%IGNORE CHECKS \n"
      ^ pp_ignorechecks instrs.ignorechecks
      ^ "\n"
    else ""
  in

  let pp_node_instructions =
    "%%NODE INSTRUCTIONS, e.g. \n"
    ^ "%%suppress_action a.\n"
    ^ if (List.length instrs.node_instructions > 0) then
      (String.concat "\n"
           (List.map 
              (function
                | Display_n a -> sprintf "display_action %a." pp_action_id a
                | Suppress_n a -> sprintf "suppress_action %a." pp_action_id a)
              instrs.node_instructions))
      ^ "\n"
    else ""
  in

  let pp_edge_instructions = 
    if (List.length instrs.edge_instructions > 0) then
      "%%EDGE INSTRUCTIONS, e.g. \n"
      ^ "%%suppress_edge hb.\n"
      ^ (String.concat "\n"
           (List.map 
              (function
                | Display_e (r,Any) -> sprintf "display_edge %s." r
                | Display_e (r,Only rl) ->
                    String.concat " "
                      (List.map 
                         (fun (a1,a2) ->
                           sprintf "display_edge %s:%a --> %a." r  pp_action_id a1  pp_action_id a2)
                         rl)
                | Suppress_e (r,Any) -> sprintf "suppress_edge %s." r
                | Suppress_e (r,Only rl) ->
                    String.concat " "
                      (List.map
                         (fun (a1,a2) ->
                           sprintf "suppress_edge %s:%a --> %a." r pp_action_id a1 pp_action_id a2)
                         rl))
              instrs.edge_instructions))
      ^ "\n"
    else ""
  in

  pp_node_instructions 
  ^ pp_edge_instructions
  ^ pp_ignorechecks 
  ^ pp_mode
  ^ pp_add_actions
  ^ pp_add_rels
  ^ pp_remove_actions
  ^ pp_remove_rels
  ^ pp_constrain_rels
  ^ pp_commands
  ^ pp_show


let pp_execution_opsem_data () m = function (exod:execution_opsem_data) -> pp_ascii () (m,exod)




let generate_dot_file  (m,testname,(exod,exedo,exddo))  (f:string) =
      (* Print dot version of test *)
  let fd = open_out f in
  output_string fd (sprintf "%a" pp_dot (m,testname,(exod,exedo,exddo)));
  close_out fd
      

let generate_thy_file (m,testname,(exod,exedo,exddo))  (f:string) =
      (* Print Isabelle version of test *)
  let fd = open_out f in
  output_string fd (sprintf "%a"  pp_isa (m,testname,(exod,exedo,exddo)));
  close_out fd

let generate_tex_file (m,testname,(exod,exedo,exddo))  (f:string) =
      (* Print tex version of test *)
  let fd = open_out f in
  output_string fd (sprintf "%a"  pp_tex (m,testname,(exod,exedo,exddo)));
  close_out fd

let generate_exec_file (model,m,testname,(exod,exedo,exddo)) (f:string) =
      (* Print .exc version of test *)
  let fd = open_out f in
  output_string fd (sprintf "%a"  pp_execfile (model,m,testname,(exod,exedo,exddo)));
  close_out fd

let generate_display_file (verbosity,default,exod,instrs) (f:string) =
      (* Print .dsp version of test *)
  let fd = open_out f in
  output_string fd (sprintf "%a"  pp_dsp (verbosity,default,exod,instrs));
  close_out fd
      


let pp_summary () (quiet,command_line,filename,summ) =
  let nb_candidates = Printf.sprintf "No. of execution candidates = %d" summ.candidates_with_orders_count in
  (if quiet then ""
   else
      Printf.sprintf "No. of raw execution candidates before constraint solving = %d\n" summ.raw_candidates_count
      ^ Printf.sprintf "No. of raw execution candidates = %d\n" summ.candidates_with_constraints_count
      ^ Printf.sprintf "%s\n" nb_candidates)
  ^ let nb_consistents = Printf.sprintf "No. of consistent execution candidates = %d" summ.checked_candidates_count in
    if command_line then
      Printf.sprintf "%s%s %s\n"
        (match filename with None -> "program " | Some s -> s ^ " ")
        (if summ.checked_candidates_count = 0 then "Forbidden" else "Allowed")
        (Printf.sprintf "(%s)" nb_consistents)
    else
      Printf.sprintf "%s\n%s\n" nb_candidates nb_consistents


let max_len_fst (l : (string * 'a) list) =
  List.fold_left (fun acc (a, _) -> max acc (String.length a)) 0 l

let pp_masked_execution_check_result tm ecr =

  let bb,norm = match tm with
  | VT220 -> "\x1B[1m","\x1B[0m"
  | Ascii -> "","" in

  let ppb () b = Printf.sprintf "%s%B%s" bb b norm in

  let present_absent b = match b with 
  | true -> bb ^ "present" ^ norm
  | false -> bb ^ "absent" ^ norm in

  let pad l n =
    l ^ String.make (n - String.length l) ' ' in

  let indent n nm = String.make (2 * n) ' ' ^ nm in
  let rec pp_masked_pred_tree_list n l =
    List.concat
      (option_map
         (fun (nm, t) -> match t with
           | Masked_pred_leaf b -> Some [indent n nm, b]
           | Masked_pred_node (b, l2) ->
             Some ((indent n nm, b) :: pp_masked_pred_tree_list (n + 1) l2)
           | Masked_dead_pred_leaf b -> Some [indent n nm, b]
           | Masked_dead_pred_node (b, l2) ->
             Some ((indent n nm, b) :: pp_masked_pred_tree_list (n + 1) l2))
         l) in
  let pp_masked_pred_tree n = function
    | Masked_pred_leaf _ -> assert false
    | Masked_dead_pred_leaf _ -> assert false
    | Masked_pred_node (b, l)
    | Masked_dead_pred_node (b, l) ->
      let base = pp_masked_pred_tree_list n l in
      let max_len = max_len_fst base in
      String.concat "" (List.map (fun (l, b) -> pad l (max_len + 1) ^ " = " ^ ppb () b ^ "\n") base)
  in

  let pp_undef undef =
    let s_undef = List.map (fun (nm, has) -> Printf.sprintf "  %s are" nm, present_absent has) undef in
    let max_len = max_len_fst s_undef in
    String.concat "" (List.map (fun (nm, has) ->  Printf.sprintf "%s%s\n" (pad nm (max_len + 1)) has) s_undef) in
    

  ""
  ^ Printf.sprintf "consistent_race_free_execution = %a\n" ppb ecr.Types.masked_ecr_consistent_race_free
  ^ Printf.sprintf "  consistent_execution = %a\n" ppb ecr.Types.masked_ecr_consistent_real
  ^ pp_masked_pred_tree 2 ecr.Types.masked_ecr_pieces_of_consistent
  ^ pp_undef ecr.Types.masked_ecr_undefined_behaviour


(* TODO: jp: fix the '['s and ']'s used to group *)
let pp_masked_execution_check_result_tight tm ecr =

  let bb,norm = match tm with
  | VT220 -> "\x1B[1m","\x1B[0m"
  | Ascii -> "","" in

  let ppb () = function
    | true -> "t"
    | false -> bb^"F"^norm in
  let ppB () = function
    | true -> bb^"T"^norm
    | false -> "f" in

  let rec pp_pred_tree_list l =
    "["
    ^ String.concat " " (List.map (fun (nm, t) -> match t with
      | Masked_pred_leaf b
      | Masked_dead_pred_leaf b -> Printf.sprintf "%s:%a" nm  ppb b
      | Masked_pred_node (b, l2)
      | Masked_dead_pred_node (b, l2) ->
        Printf.sprintf "%s:%a %s" nm  ppb b  (pp_pred_tree_list l2)) l)
    ^ "]" in
  let rec pp_pred_tree = function
    | Masked_pred_leaf _ -> assert false
    | Masked_dead_pred_leaf _ -> assert false
    | Masked_pred_node (b, l)
    | Masked_dead_pred_node (b, l) -> pp_pred_tree_list l in

  ""
  ^ Printf.sprintf "consistent_race_free_execution:%a ["  ppb ecr.masked_ecr_consistent_race_free
  ^ Printf.sprintf "consistent_execution:%a [" ppb ecr.masked_ecr_consistent_real
  ^ pp_pred_tree ecr.masked_ecr_pieces_of_consistent
  ^ " " ^ String.concat " "
    (List.map
       (fun (nm, has) ->
         Printf.sprintf "%s:%a" nm  ppB has)
       ecr.masked_ecr_undefined_behaviour)
  ^ "]\n"
