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

type stops =
  | Never
  | Always
  | OnCandidates
  | OnSolutions

let stop_at = ref OnCandidates

let parse_stop_at = function
  | "never"          -> Some Never
  | "always"         -> Some Always
  | "on_candidates"  -> Some OnCandidates
  | "on_solutions"   -> Some OnSolutions
  | _ -> None

let pp_stop_at = function
  | Never            -> "never"          
  | Always           -> "always"         
  | OnCandidates     -> "on_candidates"  
  | OnSolutions      -> "on_solutions"   

let tmpfilename = ref "out" (* Used to generate pics on the fly *)
(* TODO: command-line option to set it *)

let quietmode = ref false

let show : int option ref = ref None


type filetype =
  | Dot
  | Isa
  | Exc
  | Tex
  | Instructions

type filename = string
