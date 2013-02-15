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

type thread_id = Cmm.tid
type action_id = Cmm.aid

(* To add additional models, change this list *)
let the_models = [
  "standard", Cmm.cmm_memory_model;
  "preferred", Cmm.no_vsse_memory_model;
  "Release_Acquire", Cmm.release_acquire_memory_model;
  (*
  "separate_lo", Cmm.separate_lo_memory_model;
  "multi_lo", Cmm.multi_lo_memory_model;
  *)
  "tot", Cmm.tot_memory_model;
  "sc_coherent", Cmm.sc_coherent_memory_model;
  "too_strong_coherence", Cmm.strong_coherence_memory_model;
]

let default_model_name = "preferred"

(* TODO: jp: is this still useful? + should this translation take place here? *)
let long_names = [
  "sw","synchronized-with";
  "rs","release-sequence";
  "hrs","hypothetical-release-sequence";
  "cad","carries-a-dependency-to";
  "dob","dependency-ordered-before";
  "ithb","inter-thread-happens-before";
  "hb","happens-before";
  "vse","visible-side-effect";
  "vsses","visible-sequences-of-side-effects";
  (**)
  "dr","data_races";
  "ur","unsequenced_races";
  "ir","indeterminate_reads";
  "bm","bad_mutexes";
]

let short_names = List.map (fun (a,b) -> (b,a)) long_names
