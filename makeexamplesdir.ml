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

let bad = Str.regexp ".*\\.svn$"

let dotc = Str.regexp ".*\\.c$"
let dotexc = Str.regexp ".*\\.exc$"

let is_ok_file f =
  (Str.string_match dotc f 0)
  || (Str.string_match dotc f 0)

let is_ok_dir f =
  not (Str.string_match bad f 0)

let rec walk (dirshort, dir) =
  let dirs_and_files = List.sort compare (Array.to_list (Sys.readdir dir)) in
  let (dirs, files) =
    List.fold_left
      (fun (dirs, files) f ->
        (let ff = Filename.concat dir f in
         match (Unix.stat ff).Unix.st_kind with
           | Unix.S_REG ->
             if is_ok_file f then (dirs, f :: files)
             else (dirs, files)
           | Unix.S_DIR ->
              if is_ok_dir f then ((Filename.concat dirshort f, ff) :: dirs, files)
              else (dirs, files)
           | _ -> (dirs, files)))
      ([],[]) dirs_and_files in
  let (dirs, files) = (List.rev dirs, List.rev files) in
  (if List.length files > 0 then [(dirshort, dir, files)] else []) @ List.concat (List.map walk dirs)

let main () =
  if Array.length Sys.argv <> 3 then
    (output_string stderr "usage: src tgt\n";
     exit 1)
  else
    let src = Sys.argv.(1) in
    let tgt = Sys.argv.(2) in
    let t = walk ("examples",src) in
    List.iter
      (fun (dirshort, dir, files) ->
        let dd = Filename.concat tgt dirshort in
        Unix.system ("mkdir -p " ^ dd);
        List.iter (fun f -> Unix.system ("cp " ^ Filename.concat dir f ^ " " ^ Filename.concat dd f); ()) files;
        ())
      t;
    let json = open_out (tgt ^ "/examples.json") in
    output_string json
      ("[0,"
       ^ String.concat ","
         (List.map
            (fun (dirshort, dir, files) ->
              "[0,\"" ^ dirshort ^ "\","
              ^ String.concat "," (List.map (fun f -> ("\"" ^ f ^ "\"")) files)
              ^ "]")
            t)
       ^ "]\n");
    close_out json

let _ =
  main ()
