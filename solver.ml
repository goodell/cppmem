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
open Constraints

exception Contra

type vconstraint_working =
    {eqs : (Cmm.cvalue * Cmm.cvalue) list; (* Conjunction of equalities *)
     neqs : (Cmm.cvalue * Cmm.cvalue) list; (* Conjunction of inequalities *)
     others : atom_constraint list; (* Conjunction of other constraints *)
   }


let vconstraint_of vcw = 
  let all_eqs = List.map (fun (v1,v2) -> C (Eq (v1,v2))) vcw.eqs in
  let all_neqs = List.map (fun (v1,v2) -> C (Neq (v1,v2))) vcw.neqs in
  let all_others = List.map (fun ac -> C ac) vcw.others in
  Conj (all_eqs @ all_neqs @ all_others)

let varsym_of_val v = 
  match v with
  | Cmm.Flexible vsym -> Some vsym
  | Cmm.Rigid _ -> None

let varsyms_of_ac ac = 
  match ac with
  | EqBinOp (v,v1,op,v2) -> 
      Auxl.option_map varsym_of_val [v;v1;v2]
  | IsLocOf (v1,v2) ->
      Auxl.option_map varsym_of_val [v1;v2]
  | Eq (v1,v2) ->
      Auxl.option_map varsym_of_val [v1;v2]
  | Neq (v1,v2) ->
      Auxl.option_map varsym_of_val [v1;v2]
  | IsTrueC v 
  | IsFalseC v -> 
      begin
	match varsym_of_val v with 
	| Some vs -> [vs]
	| None -> []
      end

let all_varsyms_of vcw = 
  let varsyms_of_eqs = 
    Auxl.option_map varsym_of_val
      (List.flatten 
	 (List.map (fun (v1,v2) -> [v1;v2]) vcw.eqs)) in
  let varsyms_of_neqs = 
    Auxl.option_map varsym_of_val
      (List.flatten 
	 (List.map (fun (v1,v2) -> [v1;v2]) vcw.neqs)) in
  let varsyms_of_others = 
    (List.flatten 
       (List.map (fun ac -> varsyms_of_ac ac) vcw.others)) in
  varsyms_of_eqs @ varsyms_of_neqs @ varsyms_of_others


let const_int i = Cmm.Rigid (Cmm.Concrete i)

let rec proc_vconstraint vc sc fc s = 
  match vc with
  | Conj ds -> proc_dconstraints ds sc fc s

and proc_dconstraints cs sc fc s =
  match cs with
  | [] -> sc {eqs = [];neqs = [];others = [];} s 
  | C (IsTrueC vt) :: C (EqBinOp (v,v1,Cabs.EQ,v2)) :: cs when v = vt ->
      let proc_cs r_cs s_cs =
	sc {r_cs with eqs = (v1,v2) :: (vt,const_int 1) :: r_cs.eqs} s_cs in
      proc_dconstraints cs proc_cs fc s
  | C (IsFalseC vt) :: C (EqBinOp (v,v1,Cabs.EQ,v2)) :: cs when v = vt ->
      let proc_cs r_cs s_cs =
	sc {r_cs with 
	    eqs = (vt,const_int 0) :: r_cs.eqs;
	    neqs = (v1,v2) :: r_cs.neqs} s_cs in
      proc_dconstraints cs proc_cs fc s
  | C (IsTrueC vt) :: C (EqBinOp (v,v1,Cabs.NE,v2)) :: cs when v = vt ->
      let proc_cs r_cs s_cs =
	sc {r_cs with 
	    eqs = (vt,const_int 1) :: r_cs.eqs;
	    neqs = (v1,v2) :: r_cs.neqs} s_cs in
      proc_dconstraints cs proc_cs fc s
  | C (IsFalseC vt) :: C (EqBinOp (v,v1,Cabs.NE,v2)) :: cs when v = vt ->
      let proc_cs r_cs s_cs =
	sc {r_cs with eqs = (v1,v2) :: (vt,const_int 0) :: r_cs.eqs} s_cs in
      proc_dconstraints cs proc_cs fc s
  | c :: cs -> 
      let proc_c r_c s_c = 
	let proc_cs r_cs s_cs =
	  sc {eqs = r_c.eqs @ r_cs.eqs;
	     neqs = r_c.neqs @ r_cs.neqs; 
	     others = r_c.others @ r_cs.others;} s_cs in
	proc_dconstraints cs proc_cs fc s_c in
      proc_dconstraint c proc_c fc s

and proc_dconstraint c sc fc s =
  match c with
  | C ac -> proc_atom_constraint ac sc fc s
  | Disj vcs -> proc_vconstraints_disj vcs sc fc s

and proc_vconstraints_disj vcs sc fc s =
  match vcs with
  | [] -> fc "false" s
  | vc :: vcs -> 
      let proc_vcs m s = 
	proc_vconstraints_disj vcs sc fc s in
      proc_vconstraint vc sc proc_vcs s
	

and proc_atom_constraint ac sc fc s = 
  match ac with
    (* Possible optimisation for eq and neq : check constant clashes *)
  | Eq (v1,v2) -> sc {eqs = [v1,v2];neqs = [];others = [];} s
  | Neq (v1,v2) -> sc {eqs = [];neqs = [(v1,v2)];others = [];} s
  | IsTrueC v -> sc {eqs = [];neqs = [v,const_int 0];others = [];} s
  | IsFalseC v -> sc {eqs = [v,const_int 0];neqs = [];others = [];} s
  | IsLocOf (v1,v2) -> sc {eqs = [v1,v2];neqs = [];others = [];} s
  | EqBinOp (v,v1,Cabs.EQ,v2) -> 
      let s1 = proc_vconstraint (Conj [C (Eq (v1,v2));C (Eq (v,const_int 1))]) sc fc s in
      proc_vconstraint (Conj [C (Neq (v1,v2));C (Eq (v,const_int 0))]) sc fc s1
  | EqBinOp (v,v1,Cabs.NE,v2) -> 
      let s1 = proc_vconstraint (Conj [C (Neq (v1,v2));C (Eq (v,const_int 1))]) sc fc s in 
      proc_vconstraint (Conj [C (Eq (v1,v2));C (Eq (v,const_int 0))]) sc fc s1
  | EqBinOp (v,v1,op,v2) -> 
      sc {eqs = []; neqs = []; others = [ac];} s


type trailelmt = 
  | Mark 
  | Undo of (unit -> unit)

let trail : trailelmt list ref = ref []

let rec undo_until_mark () =
  match !trail with
  | [] -> ()
  | Mark :: t' -> trail := t'
  | Undo f :: t' ->
      f ();
      trail := t';
      undo_until_mark ()

let add_undo f =
  trail := (Undo f) :: !trail

let add_mark () =
  trail := Mark :: !trail

let reset () = trail := []

type cell = 
    {sym : Cmm.flexsym;
     mutable value : Cmm.cvalue option; 
     mutable rank : int;
     mutable parent : cell; }

let add_map m x = 
  let rec c = {sym = x;value = None;rank = 0; parent = c; } in
  (x,c) :: m

let rec find_aux c =
  if c.parent == c then c
  else 
    let cp = find_aux c.parent in
    let () = 
      match cp.value,c.value with
      | Some vp,Some v -> if vp = v then () else raise Contra
      | None,Some v -> cp.value <- Some v; add_undo (fun () -> cp.value <- None)
      | _,_ -> ()
    in
    let prevparent = c.parent in
    let () = c.parent <- cp; add_undo (fun () -> c.parent <- prevparent) in
    cp

let find_cell m x = 
  let c = try List.assoc x m with Not_found -> internal_error "constraint variable not found" in
  find_aux c 

let find m x =
  let cp = find_cell m x in
  cp.sym


let union_aux c1 c2 =
  let cp1 = find_aux c1 in
  let cp2 = find_aux c2 in
  if cp1.rank > cp2.rank then
    let () = 
      match cp1.value,cp2.value with
      | Some vp,Some v -> if vp = v then () else raise Contra
      | None,Some v -> cp1.value <- Some v; add_undo (fun () -> cp1.value <- None)
      | _,_ -> ()
    in
    let prevparent = cp2.parent in
    let () = cp2.parent <- cp1 in
    add_undo (fun () -> cp2.parent <- prevparent)
  else if cp2.rank > cp1.rank then
    let () = 
      match cp2.value,cp1.value with
      | Some vp,Some v -> if vp = v then () else raise Contra
      | None,Some v -> cp2.value <- Some v; add_undo (fun () -> cp2.value <- None)
      | _,_ -> ()
    in
    let prevparent = cp1.parent in
    let () = cp1.parent <- cp2 in
    add_undo (fun () -> cp1.parent <- prevparent)
  else if cp1 != cp2 then
    begin
      let () = 
	match cp1.value,cp2.value with
	| Some vp,Some v -> if vp = v then () else raise Contra
	| None,Some v -> cp1.value <- Some v; add_undo (fun () -> cp1.value <- None)
	| _,_ -> ()
      in
      let prevparent = cp2.parent in
      let () = cp2.parent <- cp1 ;
        cp1.rank <- cp1.rank + 1 in
      add_undo (fun () -> cp2.parent <- prevparent; cp1.rank <- cp1.rank - 1)
    end
    

let union m x1 x2 = 
  let c1 = try List.assoc x1 m with Not_found -> internal_error "constraint variable not in map" in
  let c2 = try List.assoc x2 m with Not_found -> internal_error "constraint variable not in map" in
  union_aux c1 c2


let evaluate_bin_op op i1 i2 v1 =
  let put_eq v i =
    match v with
    | Cmm.Rigid (Cmm.Concrete vi) -> if vi == i then [],[] else raise Contra
    | Cmm.Rigid (Cmm.Symbolic _) -> raise Contra
    | Cmm.Flexible v -> [(Cmm.Flexible v,Cmm.Rigid (Cmm.Concrete i))],[]
  in
  let put_neq v i =
    match v with
    | Cmm.Rigid (Cmm.Concrete vi) -> if vi != i then [],[] else raise Contra
    | Cmm.Rigid (Cmm.Symbolic _) -> [],[]
    | Cmm.Flexible v -> [],[(Cmm.Flexible v,Cmm.Rigid (Cmm.Concrete i))]
  in
  match op with
  | Cabs.ADD -> put_eq v1 (i1 + i2)
  | Cabs.SUB -> put_eq v1 (i1 - i2)
  | Cabs.MUL -> put_eq v1 (i1 * i2)
  | Cabs.DIV -> put_eq v1 (i1 / i2)
  | Cabs.MOD -> put_eq v1 (i1 mod i2)
  | Cabs.AND -> put_eq v1 (i1 land i2)
  | Cabs.OR -> put_eq v1 (i1 lor i2)
  | Cabs.BAND -> put_eq v1 (i1 land i2)
  | Cabs.BOR -> put_eq v1 (i1 lor i2)
  | Cabs.XOR -> put_eq v1 (i1 lxor i2)
  | Cabs.SHL -> put_eq v1 (i1 lsl i2)
  | Cabs.SHR -> put_eq v1 (i1 lsr i2)
  | Cabs.LT -> if i1 < i2 then put_neq v1 0 else put_eq v1 0
  | Cabs.GT -> if i1 > i2 then put_neq v1 0 else put_eq v1 0
  | Cabs.LE -> if i1 <= i2 then put_neq v1 0 else put_eq v1 0
  | Cabs.GE -> if i1 >= i2 then put_neq v1 0 else put_eq v1 0
  | Cabs.EQ -> if i1 == i2 then put_neq v1 0 else put_eq v1 0
  | Cabs.NE -> if i1 != i2 then put_neq v1 0 else put_eq v1 0

let get_subst m = 
  List.fold_left
    (fun k (vsym,c) -> 
      match c.parent.value with
      | Some v -> (vsym,v) :: k
      | None -> 
	  if c.parent == c 
	  then k 
	  else
	    (vsym,Cmm.Flexible c.parent.sym) :: k
    ) [] m

let process_vcw_simplified_wmap sc fc m vcw summ =
    let rec add_eqs eqs = 
      match eqs with
      | [] -> ()
      | (Cmm.Flexible vs1,Cmm.Rigid (Cmm.Concrete c)) :: eqs'  
      | (Cmm.Rigid (Cmm.Concrete c),Cmm.Flexible vs1) :: eqs' -> 
	  begin
	    let cp = find_cell m vs1 in
	    match cp.value with
	    | None -> 
		begin 
		  cp.value <- Some (Cmm.Rigid (Cmm.Concrete c));
                  add_undo (fun () -> cp.value <- None); 
		  add_eqs eqs' 
		end
	    | Some (Cmm.Rigid (Cmm.Concrete c')) -> 
		if c' = c then add_eqs eqs' else raise Contra
	    | _ -> raise Contra
	  end
      | (Cmm.Flexible vs1,Cmm.Rigid (Cmm.Symbolic c)) :: eqs'  
      | (Cmm.Rigid (Cmm.Symbolic c),Cmm.Flexible vs1) :: eqs' -> 
	  begin
	    let cp = find_cell m vs1 in
	    match cp.value with
	    | None -> 
		begin 
		  cp.value <- Some (Cmm.Rigid (Cmm.Symbolic c));
                  add_undo (fun () -> cp.value <- None);
		  add_eqs eqs' 
		end
	    | Some (Cmm.Rigid (Cmm.Symbolic c')) -> 
		if c' = c then add_eqs eqs' else raise Contra
	    | _ -> raise Contra
	  end
      | (Cmm.Flexible vs1,Cmm.Flexible vs2) :: eqs' -> 
	  if vs1 = vs2 
	  then add_eqs eqs' 
	  else let () = union m vs1 vs2 in add_eqs eqs'
      | (Cmm.Rigid (Cmm.Concrete i1),Cmm.Rigid (Cmm.Concrete i2)) :: eqs' ->
	  if i1 = i2 
	  then add_eqs eqs'
	  else raise Contra
      | (Cmm.Rigid (Cmm.Symbolic s1),Cmm.Rigid (Cmm.Symbolic s2)) :: eqs' ->
	  if s1 = s2 
	  then add_eqs eqs'
	  else raise Contra
      | (Cmm.Rigid (Cmm.Concrete _),Cmm.Rigid (Cmm.Symbolic _)) :: eqs'
      | (Cmm.Rigid (Cmm.Symbolic _),Cmm.Rigid (Cmm.Concrete _)) :: eqs' ->
	  raise Contra
    in
    try
      let rec loop eqs neqs others =
        let () = add_eqs eqs in
        let sub = get_subst m in
        let neq_remains = 
          Auxl.option_map
            (fun (v1,v2) ->
              match subst_val sub v1,subst_val sub v2 with
              | Cmm.Rigid (Cmm.Concrete i1),Cmm.Rigid (Cmm.Concrete i2) -> if i1 <> i2 then None else raise Contra
              | Cmm.Rigid (Cmm.Symbolic s1),Cmm.Rigid (Cmm.Symbolic s2) -> if s1 <> s2 then None else raise Contra
              | Cmm.Flexible v1,Cmm.Flexible v2 -> if v1 <> v2 then Some (Cmm.Flexible v1,Cmm.Flexible v2) else raise Contra
              | vs1,vs2 -> Some (vs1,vs2)
            )
            neqs in
        let new_eqs, new_neqs, other_remains =
          let sub_others = List.map (subst_atom_constraint sub) others in
          List.fold_left
            (fun (eqs,neqs,remains) acon ->
              match acon with
              | EqBinOp (v,Cmm.Rigid (Cmm.Concrete i1),op,Cmm.Rigid (Cmm.Concrete i2)) -> 
                  let my_eqs,my_neqs = evaluate_bin_op op i1 i2 v in
                  (my_eqs @ eqs,my_neqs @ neqs,remains)
              | EqBinOp (v,v1,Cabs.ADD,Cmm.Rigid (Cmm.Concrete 0)) 
              | EqBinOp (v,Cmm.Rigid (Cmm.Concrete 0),Cabs.ADD,v1) ->
                  (v,v1) :: eqs,neqs,remains
	      | EqBinOp (v,v1,Cabs.SUB,v2) when v1 = v2 ->
		  (v,Cmm.Rigid (Cmm.Concrete 0)) :: eqs,neqs,remains
	      | EqBinOp (v,v1,Cabs.XOR,v2) when v1 = v2 ->
		  (v,Cmm.Rigid (Cmm.Concrete 0)) :: eqs,neqs,remains
              | EqBinOp (v,v1,op,v2) -> (eqs,neqs,acon :: remains)
              | _ -> internal_error "Wrong kinds of constraints postponed"
            ) 
            ([],[],[]) sub_others
        in
        if List.length new_eqs > 0 || List.length new_neqs > 0 
        then
          loop new_eqs (new_neqs @ neq_remains) other_remains
        else 
          let new_vcw = 
	    {eqs = []; (* All solved *)
	     neqs = neq_remains;
	     others = other_remains; (* TODO: Simplify all the trivials, (x = x + 0), and abort if anything remains *)} in
(*      let _ = print_string (Printf.sprintf "Solution subst = %a\n" pp_substitution sub) in *)
        new_vcw, m, sub
      in
      let new_vcw,m,sub = loop vcw.eqs vcw.neqs vcw.others in
      sc new_vcw m sub summ
    with Contra -> fc "Constants clash" summ 
  
let process_vcw_simplified sc fc vcw summ =
(*    let _ = print_string (Printf.sprintf "After simplifying = %a\n" pp_vconstraint (vconstraint_of vcw)) in *)
    let m = 
      List.fold_left 
	(fun m v ->
	  add_map m v) [] (all_varsyms_of vcw) in
    process_vcw_simplified_wmap sc fc m vcw summ

let solve vc sc fc s = 
(*  let _ = print_string (Printf.sprintf "Initial constraint = %a\n" pp_vconstraint vc) in *)
  reset ();
  proc_vconstraint vc (process_vcw_simplified sc fc) fc s 

let solve_more_eqs vcw_solved m_prev new_eqs sc fc s =
  let present_varsyms = 
    Auxl.option_map varsym_of_val (List.flatten (List.map (fun (v1,v2) -> [v1;v2]) new_eqs)) in
  let new_m = 
    List.fold_left
      (fun m vsym -> 
	if List.mem_assoc vsym m 
	then m
	else add_map m vsym) m_prev present_varsyms in
  let () = add_mark () in
(*  let _ = print_string (Printf.sprintf "Getting more eqs = %a\n" pp_vconstraint (vconstraint_of {vcw_solved with eqs = vcw_solved.eqs @ new_eqs})) in *)
  process_vcw_simplified_wmap sc fc new_m {vcw_solved with eqs = vcw_solved.eqs @ new_eqs;} s

