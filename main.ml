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
(* open Opsem *)
open Pp
open Printf
open Auxl
open Iso 
open Execfile
open Globals
open Error



(* ******************** argument processing ************** *)

type file_argument =
  | In 
  | Out

let file_arguments = ref ([]:(file_argument*string) list)

let raw_ppmode_items = ref []

let texdefaults = ref false

let execbatch = ref false

let terminalmode = ref VT220

let the_model = ref Cmm.cmm_memory_model
let the_model_name = ref Atomic.default_model_name

let options = Arg.align [
  ( "-i", 
    Arg.String (fun s -> file_arguments := (In,s) ::(!file_arguments)),
    "<filename>        Input file: .exc or .c (at most one of each), or .dsp" );
  ( "-o", 
    Arg.String (fun s -> file_arguments := (Out,s) ::(!file_arguments)),
    "<filename>        Output file: .dot or .thy or .exc or .tex (can be used multiple times)" );
  ( "-quiet",
    Arg.Unit (fun b -> quietmode := true),
    "                  Only print summary result to stdout");
  ( "-execbatch",
    Arg.Unit (fun b -> execbatch := true),
    "                  Batch consistency check of exec fles");
  ( "-terminal", 
    Arg.String (fun s -> match s with | "vt220" -> terminalmode:=VT220 | "plain" -> terminalmode:=Ascii | _ -> Auxl.error "terminal mode must be \"vt220\" or \"plain\""),
    "<mode>        Terminal mode for batch output: vt220 or plain" );
  ( "-texdefaults",
    Arg.Bool (fun b -> texdefaults := b),
    "                  use ppmode defaults for LaTeX");
  ( "-show",
    Arg.Int (fun i -> show := Some i),
    "                  Show a particular execution (number)");
  ( "-stopat",
    Arg.String (fun s ->
      let new_stop_at = 
        match parse_stop_at s with
        | Some s -> s
        | None -> Auxl.error ("Error: stopat arg must be one of never,always,on_candidates,on_solutions") in
      stop_at := new_stop_at), 
    "<mode>            Stopping mode (one of never,always,on_candidates,on_solutions) (default <"^pp_stop_at !stop_at^">)");
  let the_models = String.concat ", " (List.map fst Atomic.the_models) in
  ( "-model",
    Arg.String (fun s ->
      try
        the_model := List.assoc s Atomic.the_models;
        the_model_name := s;
      with
          Not_found -> Auxl.error ("Error: unknown model ``" ^ s ^ "''; has to be one of: " ^ the_models)
    ), "<model-name> use the given model (one of: " ^ the_models ^ ")");
] 

let usage_msg = 
    ("\n"
     ^ " example usage (batch):       main -texdefaults true -o test.dot -o test.thy  test.c test.dsp\n" 
     ^ " example usage (interactive): main test.c test.dsp\n" 
(*      ^ "  (use \"OCAMLRUNPARAM=p  ...\" to show the ocamlyacc trace)\n" *)
    )

let _ = 
  let extra_arguments = ref [] in
  Arg.parse options 
    (fun s -> 
      file_arguments := [(In,s)]@ !file_arguments)
    usage_msg

let m = apply_raw_ppmode_items (if !texdefaults then ppmode_default_tex else ppmode_default_ps) (!raw_ppmode_items) 

let init_instructions = {(empty_instructions m) with show = !show}

let input_extensions = ["exc";"c";"dsp"]
let output_extensions = ["dot";"thy";"exc";"dsp";"tex"]

type inputs = 
  | Exc of string
  | C of string
  | CDsp of string * string
  | ExcDsp of string * string
  | ExcBatch of string list
  
let file_extension name = 
  let start = 1+String.length (Filename.chop_extension name) in
  let extension = String.sub name start (String.length name - start) in
  if List.mem extension (input_extensions @ output_extensions) then 
    Some extension 
  else 
    None

let classify_file_argument arg =
  match arg with
  | (In,name) -> 
      (match file_extension name with
      | Some e when (List.mem e input_extensions) -> 
          (In,e,name)
      | _ -> Auxl.error 
            ("\nError: unrecognised extension of input file \""^name
             ^ "\" (must be one of " ^ String.concat "," input_extensions ^")\n"))
  | (Out,name) -> 
      (match file_extension name with
      | Some e when (List.mem e output_extensions) -> 
          (Out,e,name)
      | _ -> Auxl.error 
            ("\nError: unrecognised extension of output file \""^name
             ^ "\" (must be one of "^String.concat "," output_extensions ^")\n"))

(*   all_file_arguments collects together a list of                          *)
(*                                                                           *)
(*      (In,filetype,filename)                                               *)
(*      (Out,filetype,filename)                                              *) 
(*                                                                           *)                  
let all_file_arguments = 
  List.map classify_file_argument (List.rev (!file_arguments))

let inputs = 
  option_map 
    (function 
      |(In,"exc",name) -> Some ("exc",name)
      |(In,"dsp",name) -> Some ("dsp",name)
      |(In,"c",name)-> Some("c",name) 
      | _ -> None) 
    all_file_arguments

let outputs = option_map (function |(Out,ext,name) ->Some (ext,name)|_->None) all_file_arguments

let input = 
  match !execbatch,inputs with 
  | true,_ -> 
      if outputs<>[] then  Auxl.error "\nError: no output files allowed in -execbatch mode";
      List.iter (function 
        | "exc",_ -> ()
        | _,_ -> Auxl.error "\nError: no non-.exc input files allowed in -execbatch mode")
        inputs;
      ExcBatch (List.map snd inputs)
  | false,["c",name] -> (C name) 
  | false,["exc",name] -> (Exc name)
  | false,_ -> 
      if List.mem_assoc "c" inputs && List.mem_assoc "dsp" inputs 
      then
        CDsp (List.assoc "c" inputs,List.assoc "dsp" inputs)
      else 
        if List.mem_assoc "exc" inputs && List.mem_assoc "dsp" inputs 
        then
          ExcDsp (List.assoc "exc" inputs,List.assoc "dsp" inputs)
        else
          Auxl.error "\nError: there must be at most one c or exc file, possibly with a dsp file"



(* ******************** main work ************** *)


(* let run_test s =
  let _ = print_string (sprintf "Program:\n  %a\n\n" (pp_stmt ([],[])) s) in
  let exod = opsem s in
  let _ = print_string (sprintf "%a\n" pp_execution_opsem_data exod)  in
  let _ = print_string (sprintf "%a\n" (pp_dot m) (s,(exod,None))) in
  let _ = generate_dot_file m (s,(exod,None) )"out.dot" in
  ()
*)
(* let _ = List.iter run_test Test.tests *)



(*

*)
  

let write_file (model_name,testname,(exod,exedo,exddo),instrs) (ext,output_filename) =
  let m = instrs.mode in
  match ext with
  | "dot" -> 
      let exddo =
        match exddo with
        | None -> None
        | Some exdd -> 
            Some (filter_out_exdd exdd instrs)
      in
      let exedo =
        match exedo with
        | None -> None
        | Some exed -> 
            Some (filter_out_exed exed instrs)
      in
      let exod =
        filter_out_exod exod instrs
      in
      generate_dot_file (m,Some testname,(exod,exedo,exddo)) output_filename 
  | "thy" -> generate_thy_file (m,Filename.chop_extension output_filename,(exod,exedo,exddo)) output_filename 
  | "tex" -> generate_tex_file (m,Some (Filename.chop_extension output_filename),(exod,exedo,exddo)) output_filename 
  | "exc" -> generate_exec_file (model_name,m,Some testname,(exod,exedo,exddo)) output_filename 





let _ = 
  try
    match input with
    | (ExcBatch filenames) ->

        let process_exec_batch_file filename_pad filename = 
          let ((exod,exedo,exddo),output_instrs,ignorechecks) = read_exec_file (!the_model, !the_model_name) false filename None in
	  let exed =
	    match exedo with
	    | None -> empty_execution_existential_data !the_model
	    | Some exed -> exed in
	  (match exddo with
	  | None -> ()
	  | Some exdd -> 
              Auxl.error ("\nError: non-empty exddo in batch exc file "^filename));
          
	  let exdd = derive_data !the_model exod exed in

          (*let m = T.ppmode_default_tex in
          print_string (Printf.sprintf "%a"  Pp.pp_execfile (m,"TEST",(exod,Some exed,Some exdd)));*)

	  let ecr = Iso.check_result !the_model {exod = exod; exed = exed; exdd = exdd} in
          let mecr = Iso.mask_ecr ignorechecks ecr in
          
          (* Printf.printf "\n%s" (pp_execution_check_result VT220 ecr);*)
          Printf.printf "%s %s\n" (Auxl.pad filename_pad filename) (pp_masked_execution_check_result_tight !terminalmode mecr) in
        let filename_pad = List.fold_right max (List.map String.length filenames) 0 in
        List.iter (process_exec_batch_file filename_pad) filenames        
    | (Exc (filename)) -> 
        let ((exod,exedo,exddo),output_instrs,ignorechecks) = read_exec_file (!the_model, !the_model_name) false filename None in
	(* let exod = *)
	(*   (\* Hacked up, since we don't yet have syntax *\) *)
	(*   {exod with lk = List.map (fun l -> (l,LK_Atomic)) (remove_duplicates (option_map T.location_of exod.actions)) } *)
        (* in *)
	let exed =
	  match exedo with
	  | None -> empty_execution_existential_data !the_model
	  | Some exed -> exed 
	in
	let exp_exdd =
	  match exddo with
	  | None -> empty_execution_derived_data
	  | Some exdd -> exdd 
	in
	let actual_exdd = derive_data !the_model exod exed in
	let ecr = Iso.check_result !the_model {exod = exod; exed = exed; exdd = actual_exdd} in
        let mecr = Iso.mask_ecr ignorechecks ecr in

        let continuation instrs =
          (* suspicious use of exddo below? *)
          List.iter (write_file (!the_model_name,filename,(exod,exedo,exddo),instrs)) outputs in
	Interact.interact_user_checker (!the_model,!the_model_name) (Some mecr) (init_summ m) {exod=exod;exed=exed;exdd=actual_exdd} (empty_instructions m) (Some filename) continuation
        
    | (ExcDsp (filename,filenamedsp)) -> 
        let instrs = read_instructions_file filenamedsp (empty_execution_data !the_model) init_instructions in 
        let ((exod,exedo,exddo),output_instrs,ignorechecks) = read_exec_file (!the_model, !the_model_name) false filename None in
	(* let exod = *)
	(*   (\* Hacked up, since we don't yet have syntax *\) *)
	(*   {exod with lk = List.map (fun l -> (l,LK_Atomic)) (remove_duplicates (option_map T.location_of exod.actions)) } in *)
	let exed =
	  match exedo with
	  | None -> empty_execution_existential_data !the_model
	  | Some exed -> exed 
	in
	let exp_exdd =
	  match exddo with
	  | None -> empty_execution_derived_data
	  | Some exdd -> exdd 
	in
	let actual_exdd = derive_data !the_model exod exed in

	let ecr = Iso.check_result !the_model {exod = exod; exed = exed; exdd = actual_exdd} in
        let mecr = Iso.mask_ecr ignorechecks ecr in

        let continuation instrs' =
          List.iter (write_file (!the_model_name, filename,(exod,exedo,exddo),instrs')) outputs in
	Interact.interact_user_checker (!the_model, !the_model_name) (Some mecr) (init_summ m) {exod=exod;exed=exed;exdd=actual_exdd} instrs (Some filename) continuation

    | (C (filename)) -> 
        (* Always interactive, have a command-line option? *)
        let cilf = Eval.parse_cil filename in
        let summ = RunOpsem.proc_file (!the_model,!the_model_name) init_instructions (Some filename, cilf) Interact.interaction_handles RunOpsem.Collect_only_consistent_rf in
        let ninstrs,exresults = summ.instructions,summ.results in
        let m = ninstrs.mode in
        List.iter 
          (function (ext,output_filename) ->
            match ext with
            | "dot" ->  
	        let fd = open_out output_filename in
	        let dot_pics = 
	          List.map 
		    (fun (r, _) ->
		      sprintf "%a" (Pp.pp_dot) 
		        (m, Some filename, (r.cutdown_for_drawing.exod, Some r.cutdown_for_drawing.exed, Some r.cutdown_for_drawing.exdd)))
		    exresults in
	        let _ = output_string fd (String.concat "\n" dot_pics) in
	        let _ = close_out fd in
	        ()
            | "thy" -> 
	        let fd = open_out output_filename in
	        let isa_thys = 
	          List.map 
		    (fun (r, _) ->
		      sprintf "%a" Pp.pp_isa 
		        (m,filename,(r.actual.exod,Some r.actual.exed,Some r.actual.exdd)))
		    exresults in
	        let _ = output_string fd (String.concat "\n" isa_thys) in
	        let _ = close_out fd in
	        ()
            | "tex" -> 
	        let fd = open_out output_filename in
	        let tex = 
	          List.map 
		    (fun (r, _) ->
		      sprintf "%a" Pp.pp_tex
		        (m,Some filename,(r.actual.exod,Some r.actual.exed,Some r.actual.exdd)))
		    exresults in
	        let _ = output_string fd (String.concat "\n" tex) in
	        let _ = close_out fd in
	        ()
            | "exc" -> 
	        let fd = open_out output_filename in
	        let excs = 
	          List.map 
		    (fun (r, _) ->
		      sprintf "%a" Pp.pp_execfile 
		        (!the_model_name,m,Some filename,(r.actual.exod,Some r.actual.exed,Some r.actual.exdd)))
		    exresults in
	        let _ = output_string fd (String.concat "\n" excs) in
	        let _ = close_out fd in
	        ())
          outputs
    | (CDsp (filenamec,filenamedsp)) -> 
        let instrs = read_instructions_file filenamedsp (empty_execution_data !the_model) init_instructions in
        let cilf = Eval.parse_cil filenamec in
        let summ = RunOpsem.proc_file (!the_model,!the_model_name) instrs (Some filenamec, cilf) Interact.interaction_handles RunOpsem.Collect_only_consistent_rf in
        let ninstrs,exresults = summ.instructions,summ.results in
        let m = ninstrs.mode in
        List.iter 
          (function (ext,output_filename) ->
            match ext with
            | "dot" ->  
	        let fd = open_out output_filename in
	        let dot_pics = 
                  match ninstrs.show with
                  | None -> 
	              List.map 
		        (fun (r, _) ->
		          sprintf "%a" (Pp.pp_dot) 
		            (m,Some filenamec,(r.cutdown_for_drawing.exod,Some r.cutdown_for_drawing.exed,Some r.cutdown_for_drawing.exdd)))
		        exresults 
                  | Some i -> 
                      let (r, _) = 
                        try 
                          List.nth exresults (i-1) (* -1 to start at 1 *)
                        with 
                        | Failure "nth" -> user_error (sprintf "Asked to show result no. %d, not enough produced" i) 
                        | Invalid_argument "List.nth" -> user_error (sprintf "Invalid argument (count starts as 1, not 0)") 
                      in
		          [sprintf "%a" (Pp.pp_dot) 
		            (m,Some filenamec,(r.cutdown_for_drawing.exod,Some r.cutdown_for_drawing.exed,Some r.cutdown_for_drawing.exdd))]
                in
	        let _ = output_string fd (String.concat "\n" dot_pics) in
	        let _ = close_out fd in
	        ()
            | "thy" -> 
	        let fd = open_out output_filename in
	        let isa_thys = 
                  match ninstrs.show with
                  | None ->
	              List.map
		        (fun (r, _) ->
		          sprintf "%a" Pp.pp_isa 
		            (m,filenamec,(r.actual.exod,Some r.actual.exed,Some r.actual.exdd)))
		        exresults 
                  | Some i ->
                      let (r, _) =
                        try
                          List.nth exresults (i-1) (* -1 to start at 1 *)
                        with
                        | Failure "nth" -> user_error (sprintf "Asked to show result no. %d, not enough produced" i) 
                        | Invalid_argument "List.nth" -> user_error (sprintf "Invalid argument (count starts as 1, not 0)") 
                      in
		      [sprintf "%a" (Pp.pp_isa) 
		         (m,filenamec,(r.actual.exod,Some r.actual.exed,Some r.actual.exdd))]
                in
	        let _ = output_string fd (String.concat "\n" isa_thys) in
	        let _ = close_out fd in
	        ()
            | "tex" -> 
	        let fd = open_out output_filename in
	        let tex = 
                  match ninstrs.show with
                  | None ->
	              List.map
		        (fun (r, _) ->
		          sprintf "%a" Pp.pp_tex
		            (m,Some filenamec,(r.actual.exod,Some r.actual.exed,Some r.actual.exdd)))
		        exresults
                  | Some i ->
                      let (r, _) =
                        try
                          List.nth exresults (i-1) (* -1 to start at 1 *)
                        with
                        | Failure "nth" -> user_error (sprintf "Asked to show result no. %d, not enough produced" i) 
                        | Invalid_argument "List.nth" -> user_error (sprintf "Invalid argument (count starts as 1, not 0)") 
                      in
		      [sprintf "%a" (Pp.pp_tex) 
		         (m,Some filenamec,(r.actual.exod,Some r.actual.exed,Some r.actual.exdd))]
                in
	        let _ = output_string fd (String.concat "\n" tex) in
	        let _ = close_out fd in
	        ()
            | "exc" -> 
	        let fd = open_out output_filename in
	        let excs = 
                  match ninstrs.show with
                  | None ->
	              List.map 
		        (fun (r, _) ->
		          sprintf "%a" Pp.pp_execfile 
		            (!the_model_name,m,Some filenamec,(r.actual.exod,Some r.actual.exed,Some r.actual.exdd)))
		        exresults 
                  | Some i -> 
                      let (r, _) =
                        try
                          List.nth exresults (i-1) (* -1 to start at 1 *)
                        with
                        | Failure "nth" -> user_error (sprintf "Asked to show result no. %d, not enough produced" i) 
                        | Invalid_argument "List.nth" -> user_error (sprintf "Invalid argument (count starts as 1, not 0)") 
                      in
		      [sprintf "%a" (Pp.pp_execfile) 
		         (!the_model_name,m,Some filenamec,(r.actual.exod,Some r.actual.exed,Some r.actual.exdd))]
		in
	        let _ = output_string fd (String.concat "\n" excs) in
	        let _ = close_out fd in
	        ()
            | "dsp" ->
              (* TODO: jp: ppmode_default_tex is hardcoded; bad *)
              Pp.generate_display_file (Pp.Dsp_quiet, ppmode_default_tex, empty_execution_opsem_data, ninstrs) output_filename
          )
          outputs
  with 
    Interact.UserQuit -> ()
    | Execfile.Exc_read_error s ->
      output_string stderr (s ^ "\n");
      exit 1
    | Auxl.Exc_lex_error (c, line, col) ->
      output_string stderr (Printf.sprintf "unexpected character `%c', line %d, column %d\n" c line col);
      exit 1
