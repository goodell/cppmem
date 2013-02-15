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

type t = 
    { loc_start : Lexing.position;
      loc_end : Lexing.position }

let loc_of_filename name len  = 
  [ {loc_start =  
     {
      Lexing.pos_fname =name;
      Lexing.pos_lnum = 0;
      Lexing.pos_bol =0;
      Lexing.pos_cnum =0 };
     loc_end =
     {
      Lexing.pos_fname =name;
      Lexing.pos_lnum =0;
      Lexing.pos_bol =0;
      Lexing.pos_cnum =len }
} ]

let pp_position 
    { Lexing.pos_fname = f;
      Lexing.pos_lnum = l;
      Lexing.pos_bol = b;
      Lexing.pos_cnum = c } = 
  "fname=" ^ f ^ "  lnum=" ^ string_of_int l
  ^ "  bol="^string_of_int b^"  cnum=" ^string_of_int c

let pp_position2  
    { Lexing.pos_fname = f;
      Lexing.pos_lnum = l;
      Lexing.pos_bol = b;
      Lexing.pos_cnum = c } = 
  (if f="" then "" else "file=" ^ f ^ "  ")
  ^ "line=" ^ string_of_int l ^ "  char=" ^ string_of_int (c-b)

let pp_t {loc_start=ls;loc_end=le} = 
  if ls.Lexing.pos_fname = le.Lexing.pos_fname 
      && ls.Lexing.pos_lnum = le.Lexing.pos_lnum 
  then
    (if ls.Lexing.pos_fname="" then "" else "file " ^ ls.Lexing.pos_fname ^ "  ")
    ^ "line "
    ^ string_of_int ls.Lexing.pos_lnum
    ^ " char "
    ^ string_of_int (ls.Lexing.pos_cnum - ls.Lexing.pos_bol)
    ^ " - "
    ^ string_of_int (le.Lexing.pos_cnum - le.Lexing.pos_bol)
  else if ls.Lexing.pos_fname = le.Lexing.pos_fname 
      && ls.Lexing.pos_cnum - ls.Lexing.pos_bol = 0
      && le.Lexing.pos_cnum - le.Lexing.pos_bol = 0 
  then
    (if ls.Lexing.pos_fname="" then "" else "file " ^ ls.Lexing.pos_fname ^ "  ")
    ^ "line "
    ^ string_of_int ls.Lexing.pos_lnum
    ^ " - "
    ^ string_of_int le.Lexing.pos_lnum
  else 
    (if ls.Lexing.pos_fname="" then "" else "file " ^ ls.Lexing.pos_fname ^ "  ")
    ^ "line "
    ^ string_of_int ls.Lexing.pos_lnum
    ^ " char "
    ^ string_of_int (ls.Lexing.pos_cnum - ls.Lexing.pos_bol)
    ^ " - " 
    ^ "line "
    ^ string_of_int le.Lexing.pos_lnum
    ^ " char "
    ^ string_of_int (le.Lexing.pos_cnum - le.Lexing.pos_bol)

let dummy_pos = 
  { Lexing.pos_fname = "dummy";
    Lexing.pos_lnum = 0;
    Lexing.pos_bol = 0;
    Lexing.pos_cnum = 0 }

let dummy_t =  
  { loc_start=dummy_pos;
    loc_end=dummy_pos }

let pp_loc l = String.concat " " ((List.map pp_t) l)

