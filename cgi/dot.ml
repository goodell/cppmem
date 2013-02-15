(* Copyright (C) 2012 Jean Pichon *)
(*
Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions 
are met:                                                           
1. Redistributions of source code must retain the above copyright  
notice, this list of conditions and the following disclaimer.      
2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
3. The names of the authors may not be used to endorse or promote  
products derived from this software without specific prior written 
permission.                                                        
                                                                   
THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS 
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED  
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY    
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE  
GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS      
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHE
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR    
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.            
*)
let rec drop n l =
  if n <= 0 then l
  else drop (n - 1) (List.tl l)

let start_svg = Str.regexp "^<svg"

let stop_svg = Str.regexp "^</svg>"

type kind = Dot | Neato

let path_of_kind = function
  | Dot -> "/usr/local/bin/dot"
  | Neato -> "/usr/local/bin/neato"

let call_graphviz kind s =
  let (inc, outc) = Unix.open_process (path_of_kind kind ^ " -Tsvg") in
        output_string outc (s ^ "\n"); flush outc;
        let read () =
          let rec f started acc =
            try
              let l = input_line inc ^ "\n" in
              if started then
                (if Str.string_match stop_svg l 0 then l :: acc
                 else f true (l :: acc))
              else
                (if Str.string_match start_svg l 0 then f true (l :: acc)
                 else f false acc)
            with
                End_of_file -> List.rev acc in
          List.rev (f true []) in
        try
          let lines = drop 6 (read ()) in
          let out = String.concat "" lines in
          Unix.close_process (inc, outc);
          (*print_string (BatBase64.str_encode s)*)
          out
        with
            _ -> "error"

let invalid () = print_string "invalid request\n"

let _ =
  try
    let args = Cgi.parse_args () in
    match args with
      | ["file",s; "kind",k] ->
        let k = (match k with
          | "dot" -> Some Dot
          | "neato" -> Some Neato
          | _ -> None) in
        (match k with
          | None -> invalid ()
          | Some k -> print_string (call_graphviz k s))
      | _ -> invalid ()
  with
      s -> print_string ("corrupted request: " ^ Printexc.to_string s ^ "\n")
