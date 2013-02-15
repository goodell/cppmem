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

(* lexer for executions *)


{
(*open Location*)
open Types
open Auxl
open Execparser

exception Eof
exception CannotHappen

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- 
    { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum }

type lexer = Lexing.lexbuf -> Execparser.token

let keyword_table = Hashtbl.create 53
let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    ([
      "model", MODEL;
      "location_kinds" , LOCATION_KINDS;
      "atomic"         , ATOMIC;
      "nonatomic"      , NONATOMIC;
      "mutex"          , MUTEX;
      "sameloc"        , SAMELOC ;
      "atomiclocs"     , ATOMICLOCS ;
      "help"           , HELP ;
      "relabel"        , RELABEL ;
      "ignore"         , IGNORE ;
      "consider"       , CONSIDER ;
      "display_action", DISPLAY_ACTION;
      "display_edge", DISPLAY_EDGE;
      "suppress_action", SUPPRESS_ACTION;
      "suppress_edge", SUPPRESS_EDGE;
      "add"            , ADD ;
      "remove"         , REMOVE ;
      "generate"       , GENERATE ;
      "quit"           , QUIT ;
      "next"           , NEXT ;
      "intermediate"   , INTERMEDIATE ;
      "next_candidate" , NEXTCANDIDATE ;
      "next_consistent" , NEXTCONSISTENT ;
      "non_stop"       , NONSTOP ; 
      "set"            , SET ;
      "fontsize"       , FONTSIZE ;
      "node_height"    , NODE_HEIGHT  ;
      "node_width"     , NODE_WIDTH ;
      "penwidth"       , PENWIDTH ;
      "filled"         , FILLED ;
      "xscale"         , XSCALE ;
      "yscale"         , YSCALE ;
      "ranksep"        , RANKSEP;
      "nodesep"        , NODESEP;
      "legend"         , LEGEND ;
      "layout"         , LAYOUT ;
      "neato_par"      , NEATO_PAR ;
      "neato_par_init" , NEATO_PAR_INIT ;
      "neato_downwards", NEATO_DOWNWARDS ;
      "dot"            , DOTFILE ;
      "neato"          , NEATO ;
      "fig"            , FIG ;
      "ps"             , PS ;
      "tex"            , TEX ;
      "thread_ids"            , THREAD_IDS ;
      "txt"            , TXT ;
      "isa"            , ISAFILE ;
      "exc"            , EXCFILE ;
      "dsp"            , INSTRFILE ;
      "init"           , INIT ;
      "par"            , PAR ;
      "from"           , FROM ;
      "to"             , TO ;
      "all"            , ALL ;
      "show"           , SHOW ;
      "true"           , BOOL(true) ;
      "false"          , BOOL(false) ;
      "L"  ,  L ;
      "U"  ,  U ;
      "R"  ,  RSKEL ;
      "W"  ,  WSKEL ;
      "RMW"  ,  RMWSKEL ;
      "F"  ,  FSKEL ;
]
@
   (let action_types = [
   "R"  ,  (function ot -> R ot );
   "W"  ,  (function ot -> W ot);
   "RMW",  (function ot -> RMW ot);
   "F"  ,  (function ot -> F ot );
   ] in 
   let order_types = [
   "na"             , Nonatomic        ;
   "NA"             , Nonatomic        ;
   "sc"             , Atomic Cmm.Seq_cst;
   "SC"             , Atomic Cmm.Seq_cst;
   "rlx"            , Atomic Cmm.Relaxed;
   "RLX"            , Atomic Cmm.Relaxed;
   "rel"            , Atomic Cmm.Release;
   "REL"            , Atomic Cmm.Release;
   "acq"            , Atomic Cmm.Acquire;
   "ACQ"            , Atomic Cmm.Acquire;
   "con"            , Atomic Cmm.Consume;
   "CON"            , Atomic Cmm.Consume;
   "a/r"            , Atomic Cmm.Acq_rel;
   "A/R"            , Atomic Cmm.Acq_rel;
   ] in
   List.flatten 
     (List.map 
        (function (at,ac) -> List.map 
                               (function (ot,oc) -> at^ot, ac oc)
                               order_types) 
        action_types))
  @ [
   "na"             , MO(Nonatomic        );
   "NA"             , MO(Nonatomic        );
   "sc"             , MO(Atomic Cmm.Seq_cst);
   "SC"             , MO(Atomic Cmm.Seq_cst);
   "rlx"            , MO(Atomic Cmm.Relaxed);
   "RLX"            , MO(Atomic Cmm.Relaxed);
   "rel"            , MO(Atomic Cmm.Release);
   "REL"            , MO(Atomic Cmm.Release);
   "acq"            , MO(Atomic Cmm.Acquire);
   "ACQ"            , MO(Atomic Cmm.Acquire);
   "con"            , MO(Atomic Cmm.Consume);
   "CON"            , MO(Atomic Cmm.Consume);
   "a/r"            , MO(Atomic Cmm.Acq_rel);
   "A/R"            , MO(Atomic Cmm.Acq_rel);
   ]
 @ [
   "Success", LOCKOUTCOME Cmm.Locked;
   "Blocked", LOCKOUTCOME Cmm.Blocked
   ]
)

} 


let whitespace = (' ' | '\t' | '\010' | '\012' | '\013')
let newline = ('\010' | '\013' | "\013\010")
let non_newline = [^ '\010' '\013']
let non_newline_whitespace = (' ' | '\t' |'\012')
let identchar = (['A'-'Z'] | ['a'-'z'] | ['0'-'9'] | "_" | "'")
let ident = identchar (identchar)*
let filename = identchar (identchar)* ('.' (identchar)*)+
let digit = ['0'-'9']
let num = digit+
let float = digit+ | digit+ '.' digit+

rule mylexer = parse

  ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '/'] * as id
                                                        { try
                                                          Hashtbl.find keyword_table id
                                                        with Not_found ->
                                                          IDENT id }
| ident "." ident as lxm                                { FILENAME(lxm) } 
| ident as lxm                                          { IDENT(lxm) }
| "-->"                                                 { EDGEHEAD }
| "--"                                                  { EDGETAIL }
| ":"                                                   { COLON } 
| ";"                                                   { SEMICOLON }
| "."                                                   { DOT } 
| "="                                                   { EQUALS }
| "/"                                                   { SLASH }
| "{"                                                   { LBRACE }
| "}"                                                   { RBRACE }
| ","                                                   { COMMA }
| "*"                                                   { STAR }
| '"' ([^'"']* as lxm) '"'                              { STRING(lxm) }
| eof                                                   { EOF }
| (non_newline_whitespace non_newline_whitespace*)      { mylexer lexbuf} 
| newline                                               { incr_linenum lexbuf; mylexer lexbuf }
| (non_newline_whitespace* "%"[^'\n' '\013']* newline)  { incr_linenum lexbuf; mylexer lexbuf } 
| (non_newline_whitespace* "%"[^'\n' '\013']* eof)      { EOF }
| _ as c                                                { let p = lexbuf.Lexing.lex_curr_p in raise (Exc_lex_error (c, p.Lexing.pos_lnum, p.Lexing.pos_cnum - p.Lexing.pos_bol)) }

(* 
| "--" (edge_type as lxm) "-->"                         { EDGE(lxm) } 
| num as lxm                                            { NUM(int_of_string lxm) }
| float as lxm                                          { FLOAT(float_of_string lxm) }
*)



