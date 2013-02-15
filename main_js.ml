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

module Html = Dom_html

(* TODO: jp: be more careful about possible interferences => use more locks *)

open Eval
open Types

let set_class n clas =
  n##setAttribute(Js.string "class",Js.string clas)

let (>>=) = Lwt.bind

let http_get url =
  XmlHttpRequest.get url
    >>= fun {XmlHttpRequest.code = cod; content = msg} ->
  if cod = 0 || cod = 200
  then Lwt.return msg
  else fst (Lwt.wait ())

let json : < parse : Js.js_string Js.t -> 'a> Js.t = Js.Unsafe.variable "JSON"

let createTextBox doc name =
  Html.createInput ~_type:(Js.string "text") ~name:(Js.string name) doc

let make_space doc =
  doc##createTextNode (Js.string " ")

let example_c_program =
  "int main() {\n int x = 2;\n int y = 0;\n y = (x == (x=3));\n return 0; }"

let createRadioButton doc name =
  Html.createInput ~_type:(Js.string "radio") ~name:(Js.string name) doc

let createCheckbox doc name =
  Html.createInput ~_type:(Js.string "checkbox") ~name:(Js.string name) doc

let set_as_new_first_child parent child =
  Js.Opt.case (parent##firstChild)
    (fun () -> Dom.appendChild parent child)
    (fun old -> Dom.replaceChild parent child old)

type state = {
  mutable model: Cmm.memory_model;
  mutable model_name: string;
  mutable instructions: instructions;
  (* TODO: jp:
     let 'a = (result * execution_check_result) in
     (('a, ('a list * int)) either) option *)
  mutable st_results: (result * execution_check_result) list;
  mutable res: (result * execution_check_result) option;
  mutable result_index: int;
}

type dsp_rec = {
  dsp_button: Html.buttonElement Js.t;
  dsp_ta: Html.textAreaElement Js.t;
  dsp_ta_button: Html.buttonElement Js.t;
  dsp_ta_div: Html.divElement Js.t;
  dsp_div: Html.divElement Js.t;
}

type predicates_rec = {
  predicates_div: Html.divElement Js.t;
  mutable predicates_items: (Html.inputElement Js.t * Html.labelElement Js.t) list;
  mutable predicates_p: Html.paragraphElement Js.t;
}

type prog_rec = {
  prog_h: Html.element Js.t;
  input_type_selector_div: Html.divElement Js.t;
  input_type_selector_items: Html.inputElement Js.t list;
  examples_group_selector: Html.selectElement Js.t;
  example_selector: Html.selectElement Js.t;
  ta: Html.textAreaElement Js.t;
  prog_div: Html.divElement Js.t;
  interact_button: Html.buttonElement Js.t;
  reset_button: Html.buttonElement Js.t;
}

type tab_rec = {
  tab: Html.tableElement Js.t;
  tr_top: Html.tableRowElement Js.t;
  tr_bot: Html.tableRowElement Js.t;
  td_top_left: Html.tableCellElement Js.t;
  td_top_right: Html.tableCellElement Js.t;
  td_bot_left: Html.tableCellElement Js.t;
  td_bot_right: Html.tableCellElement Js.t;
}

type navigation_rec = {
  navigation_div: Html.divElement Js.t;
  prev_consistent: Html.buttonElement Js.t;
  prev: Html.buttonElement Js.t;
  next: Html.buttonElement Js.t;
  next_consistent: Html.buttonElement Js.t;
  goto_label: Html.inputElement Js.t;
  goto_button: Html.buttonElement Js.t;
}

type model_rec = {
  model_div: Html.divElement Js.t;
  model_h: Html.element Js.t;
  model_items: Html.inputElement Js.t list;
}

type start_screen = {
  title: Html.element Js.t;
  execution_info: Html.element Js.t;
  program_info: Html.element Js.t;
  model_rec: model_rec;
  msg: Html.paragraphElement Js.t;
  navigation_rec: navigation_rec;
  mutable relations_items: Html.inputElement Js.t list;
  mutable relations_div: Html.divElement Js.t;
  predicates_rec: predicates_rec;
  layout_items: Html.inputElement Js.t list;
  layout_div: Html.divElement Js.t;
  tex_item: Html.inputElement Js.t;
  tex_div: Html.divElement Js.t;
  dsp_rec: dsp_rec;
  prog_rec: prog_rec;
  tab_rec: tab_rec;
  (**)
  mutable div: Html.divElement Js.t;
}

let layout_dsp_rec doc dsp_rec =
  Dom.appendChild dsp_rec.dsp_div dsp_rec.dsp_button;
  Dom.appendChild dsp_rec.dsp_ta_div dsp_rec.dsp_ta;
  let br = Html.createBr doc in
  Dom.appendChild dsp_rec.dsp_ta_div br;
  Dom.appendChild dsp_rec.dsp_ta_div dsp_rec.dsp_ta_button

let set_navigation_rec navigation_rec m =
  navigation_rec.prev_consistent##disabled <- m;
  navigation_rec.prev##disabled <- m;
  navigation_rec.next##disabled <- m;
  navigation_rec.next_consistent##disabled <- m;
  navigation_rec.goto_label##disabled <- m;
  navigation_rec.goto_button##disabled <- m

let make_info_empty doc info =
  Js.Opt.case (info##firstChild)
    (fun () -> ())
    (fun txt ->
      Dom.removeChild info txt;
      let txt = doc##createTextNode(Js.string "-") in
      set_class info "invisible";
      Dom.appendChild info txt)

let layout_start_screen doc screen =
  Dom.appendChild screen.tab_rec.td_top_left screen.title;
  let d = Html.createDiv doc in
  d##id <- Js.string "start_screen";
  Dom.appendChild d screen.tab_rec.tab;
  Dom.appendChild screen.tab_rec.td_top_left screen.model_rec.model_div;
  Dom.appendChild screen.tab_rec.td_top_left screen.prog_rec.prog_div;
  Dom.appendChild screen.prog_rec.prog_div screen.program_info;
  set_navigation_rec screen.navigation_rec Js._true;
  screen.navigation_rec.goto_label##value <- Js.string "";
  Dom.appendChild screen.tab_rec.td_top_right screen.execution_info;
  Dom.appendChild screen.tab_rec.td_top_right screen.navigation_rec.navigation_div;
  Dom.appendChild screen.tab_rec.td_top_right screen.predicates_rec.predicates_div;
  let config_div = Html.createDiv doc in
  config_div##id <- Js.string "config_div";
  Dom.appendChild config_div screen.msg;
  Dom.appendChild config_div screen.relations_div;
  Dom.appendChild config_div screen.layout_div;
  Dom.appendChild config_div screen.tex_div;
  Dom.appendChild config_div screen.dsp_rec.dsp_div;
  Dom.appendChild screen.tab_rec.td_bot_left config_div;
  (**)
  screen.div <- d;
  Dom.appendChild doc##body d;
  (**)
  make_info_empty doc screen.execution_info;
  make_info_empty doc screen.program_info;
  (**)
  d

type interaction_screen = {
  is_title: Html.element Js.t;
  is_execution_info: Html.element Js.t;
  is_program_info: Html.element Js.t;
  is_model_rec: model_rec;
  is_msg: Html.paragraphElement Js.t;
  is_navigation_rec: navigation_rec;
  is_exports: Html.paragraphElement Js.t;
  is_execution: Html.divElement Js.t;
  is_relations_items: Html.inputElement Js.t list;
  is_relations_div: Html.divElement Js.t;
  is_predicates_rec: predicates_rec;
  is_layout_items: Html.inputElement Js.t list;
  is_layout_div: Html.divElement Js.t;
  is_tex_item: Html.inputElement Js.t;
  is_tex_div: Html.divElement Js.t;
  is_dsp_rec: dsp_rec;
  is_tab_rec: tab_rec;
  is_prog_rec: prog_rec;
}

let kind_of_layoutmode = function
  | LO_dot -> "dot"
  | _ -> "neato"

let query_graphviz_server ?(timeout=None) layoutmode file =
  let cont = ref ["file",`String (Js.string file);"kind",`String (Js.string (kind_of_layoutmode layoutmode))] in
  (XmlHttpRequest.perform_raw_url ~form_arg:(`Fields cont) ~timeout:timeout "dot.cgi")

let server_is_alive = Lwt_mvar.create_empty ()

let btoa (data : Js.Unsafe.any) : Js.js_string Js.t =
  let wb = Js.Unsafe.variable "window.btoa" in
  Js.Unsafe.coerce (Js.Unsafe.fun_call wb (Array.make 1 data))

let exportLink doc name typ content =
  let exportLink = Html.createA doc in
  let x = btoa (Js.Unsafe.inject (Js.string content)) in
  exportLink##setAttribute(Js.string "href", Js.string ("data:" ^ typ ^ ";base64," ^ Js.to_string x));
  let tn = doc##createTextNode (Js.string name) in
  Dom.appendChild exportLink tn;
  exportLink

let interact_user_w_raw_candidate sofar cand m = ()
let interact_user_w_preliminary_soln sofar raw_cand sub sub_cand count_rf_poss m = ()
let interact_user_w_rfmap model sofar raw_cand sub_cand rfmap eqs m = ()
let interact_user_w_rfmap_soln model sofar raw_cand sub_rfm sub_cand rfmap count_mo_poss m = ()
let interact_user_w_mo sofar raw_cand sub_cand rfmap mo_rel count_sc_poss = ()
let interact_user_w_sc sofar raw_cand sub_cand rfmap mo_rel sc_rel = ()
let interact_user_checker model ecro sofar exwit instrs filename continuation = continuation instrs
let final_action filename cand = ()
let interaction_handles = {
  RunOpsem.interact_user_w_raw_candidate = interact_user_w_raw_candidate;
  RunOpsem.interact_user_w_preliminary_soln = interact_user_w_preliminary_soln;
  RunOpsem.interact_user_w_rfmap = interact_user_w_rfmap;
  RunOpsem.interact_user_w_rfmap_soln = interact_user_w_rfmap_soln;
  RunOpsem.interact_user_w_sc = interact_user_w_sc;
  RunOpsem.interact_user_w_mo = interact_user_w_mo;
  RunOpsem.interact_user_checker = interact_user_checker;
  RunOpsem.final_action = final_action;
}


let remove_screen doc screen_div =
  Dom.removeChild doc##body screen_div

(* TODO: jp: make this less redundant *)
let error_ss screen s =
  screen.msg##innerHTML <- Js.string s;
  set_class screen.msg "error"

let error_is screen s =
  screen.is_msg##innerHTML <- Js.string s;
  set_class screen.is_msg "error"

let warning_ss screen s =
  screen.msg##innerHTML <- Js.string s;
  set_class screen.msg "warning"

let log_ss screen s =
  screen.msg##innerHTML <- Js.string s;
  set_class screen.msg "normal"

let log_is screen s =
  screen.is_msg##innerHTML <- Js.string s;
  set_class screen.is_msg "normal"


let fetch_image doc (dot,exc_filtered) st screen (lock,(get,set)) stop_anim =
  let instrs = st.instructions in
  let layoutmode = instrs.mode.layout in
  let put_new_image mv msg =
    set (Some (instrs,Auxl.Right msg));
    Lwt_mutex.unlock lock;
    Lwt_mvar.put mv () >>= (fun _ ->
      Lwt.return ()) in
  let perform_own_fetch mv =
    query_graphviz_server layoutmode dot
    >>= fun {XmlHttpRequest.code = cod; content = msg} ->
    if cod = 0 || cod = 200
    then
      (let msg = Js.string msg in
       stop_anim ();
       screen.is_execution##innerHTML <- msg;
       Lwt_mutex.lock lock >>= (fun () ->
         let res = get () in
         match res with
           | None -> put_new_image mv msg
           | Some (instrs2,_) ->
             if instrs2 = instrs then put_new_image mv msg
             else
               (Lwt_mutex.unlock lock;
                Lwt.return ())))
    else
      (error_is screen ("problem " ^ string_of_int cod ^ " " ^ msg);
       fst (Lwt.wait ())) in
  let perform_fresh_fetch () =
    let mv = Lwt_mvar.create_empty () in
    set (Some (instrs,Auxl.Left mv));
    Lwt_mutex.unlock lock;
    perform_own_fetch mv in
  let perform_fetch () =
    Lwt_mutex.lock lock >>= (fun () ->
      let el = get () in
      match el with
        | None -> perform_fresh_fetch ()
        | Some (th_instrs,n) ->
          if th_instrs = instrs then
            (match n with
              | Auxl.Left mv ->
                (Lwt_mutex.unlock lock;
                 Lwt_mvar.take mv >>= (fun () ->
                   Lwt_mvar.put mv () >>= (fun () ->
                     Lwt_mutex.lock lock >>= (fun () ->
                       let res = get () in
                       Lwt_mutex.unlock lock;
                       (match res with
                         | Some (instrs2,Auxl.Right n) ->
                           if instrs2 = instrs then
                             (stop_anim ();
                              screen.is_execution##innerHTML <- n;
                              Lwt.return ())
                           else Lwt.return ()
                         | _ -> Lwt.return ())))))
              | Auxl.Right n ->
                (Lwt_mutex.unlock lock;
                 stop_anim ();
                 screen.is_execution##innerHTML <- n;
                 Lwt.return ()))
          else
            (perform_fresh_fetch ())) in
  Lwt_mvar.take server_is_alive >>= (fun is_alive ->
    Lwt_mvar.put server_is_alive is_alive;
    match is_alive with
      | true -> perform_fetch ()
      | false ->
        (let pre = Html.createPre doc in
         stop_anim ();
         let txt = doc##createTextNode (Js.string exc_filtered) in
         Dom.appendChild pre txt;
         Dom.appendChild screen.is_execution pre;
         Lwt.return ()))

let export_files doc screen instrs (res,ex_some,exc,dot) =
  try
  let txt = doc##createTextNode (Js.string "Files: ") in
  Dom.appendChild screen.is_exports txt;
  (**)
  let excLink = exportLink doc "out.exc" "text/plain" exc in
  Dom.appendChild screen.is_exports excLink;
  (**)
  let sep1 = doc##createTextNode (Js.string ", ") in
  Dom.appendChild screen.is_exports sep1;
  (**)
  let dotLink = exportLink doc "out.dot" "text/plain" dot in
  Dom.appendChild screen.is_exports dotLink;
  (**)
  let sep2 = doc##createTextNode (Js.string ", ") in
  Dom.appendChild screen.is_exports sep2;
  (**)
  let dspinstrs = { instrs with 
    add_actions = [];
    remove_actions = []; 
    add_rels = []; 
    remove_rels = []; 
    commands = [];
  } in
  let dsp = Pp.pp_dsp () (Pp.Dsp_verbose, ppmode_default_web, res.actual.exod, dspinstrs) in
  let dspLink = exportLink doc "out.dsp" "text/plain" dsp in
  Dom.appendChild screen.is_exports dspLink;
  (**)
  let sep3 = doc##createTextNode (Js.string ", ") in
  Dom.appendChild screen.is_exports sep3;
  (**)
  let tex = (try Pp.pp_tex () (instrs.mode,None,ex_some) with e -> error_is screen "problem with tex"; "error") in
  let texLink = exportLink doc "out.tex" "text/plain" tex in
  Dom.appendChild screen.is_exports texLink
  with e -> error_is screen ("Problem with exporting: " ^ Printexc.to_string e)

let base_instructions =
  empty_instructions ppmode_default_web

let initial_instructions = { base_instructions with
  edge_instructions =
    List.map (fun nm -> Suppress_e (nm, Any)) ["asw";"dd";"cd";"hb";"vse";"ithb";"cad";"hrs";"rs"]
  @ base_instructions.edge_instructions;
}

type kind = Is_leaf | Is_node

(* Note: this is close to Pp.pp_execution_check_result *)
let make_execution_check_result_p doc d eval (consistent_checked, ecr) =

  let ppb () b = if b then "true" else "false" in
  
  let present_absent = function
  | true -> "present"
  | false -> "absent" in
  
  let pad l n =
    l ^ String.make (n - String.length l) ' ' in
  
  let max_len_x l =
    List.fold_left (fun acc (nm, pre, _, _, _, _) -> max acc (String.length pre + String.length nm)) 0 l in
  
  let indent n nm = String.make (2 * n) ' ' ^ nm in
  let rec pp_pred_tree_list n l active =
    List.concat
      (List.map
         (fun (nm, t) -> match t with
           | Masked_pred_leaf b -> [nm, indent n "", b, true, active, Is_leaf]
           | Masked_pred_node (b, l2) ->
             let children = pp_pred_tree_list (n + 1) l2 active in
             (nm, indent n "", b, true, active, Is_node) :: children
           | Masked_dead_pred_leaf b -> [nm, indent n "", b, false, active, Is_leaf]
           | Masked_dead_pred_node (b, l2) ->
             let children = pp_pred_tree_list (n + 1) l2 false in
             (nm, indent n "", b, false, active, Is_node) :: children)
         l) in

  let add_core n nm kind =
    let txt = doc##createTextNode(Js.string nm) in
    match kind with
      | Is_leaf ->
        let a = Html.createA doc in
        set_class a "predicate";
        a##href <- Js.string ("cmm.html#" ^ nm);
        a##target <- Js.string "_cmm";
        Dom.appendChild a txt;
        Dom.appendChild n a
      | Is_node ->
        Dom.appendChild n txt in
  
  let make_item_aux nm (pre,suf) kind =
    let pre = doc##createTextNode(Js.string pre) in
    let suf = doc##createTextNode(Js.string suf) in
    let cb = createCheckbox doc nm in
    cb##value <- Js.string nm;
    cb##checked <- Js._true;
    let lbl = Html.createLabel doc in
    Dom.appendChild lbl cb;
    Dom.appendChild lbl pre;
    add_core lbl nm kind;
    Dom.appendChild lbl suf;
    (cb,lbl) in

  let add_result ?(color=false) lbl (sep, b, bb) active enabled =
    let x = doc##createTextNode(Js.string sep) in
    Dom.appendChild lbl x;
    let res = Html.createSpan doc in
    if enabled && active then set_class res ("result" ^ if color then (" " ^ if b then "valid" else "invalid") else "")
    else ();
    let txt = doc##createTextNode(Js.string bb) in
    Dom.appendChild res txt;
    Dom.appendChild lbl res in

  let make_item ?(color=false) nm (pre, suf) (sep, b, bb) active enabled kind =
    let (cb,lbl) = make_item_aux nm (pre, suf) kind in
    if eval then add_result ~color:color lbl (sep, b, bb) active enabled else ();
    if active then (cb##checked <- Js._true)
    else (cb##checked <- Js._false);
    let nl = doc##createTextNode(Js.string "\n") in
    Dom.appendChild lbl nl;
    (cb,lbl) in

  let pp_pred_tree n enabled = function
    | Masked_pred_leaf _ -> assert false
    | Masked_dead_pred_leaf _ -> assert false
    | Masked_dead_pred_node (b, l)  (* TODO: jp: not sure *)
    | Masked_pred_node (b, l) ->
      let base = pp_pred_tree_list n l enabled in
      let max_len = max_len_x base in
      let lines =
        List.map
          (fun (nm,pre,b,active,enabled,kind) ->
            let suf = pad "" (max_len - String.length nm - String.length pre) in
            let bb = ppb () b in
            make_item nm (pre,suf) (" = ", b, bb) active enabled kind)
          base in
      lines
  in

  let make_item2 ?(color=false) nm (pre, suf) (sep, b, bb) active enabled kind =
    let pre = doc##createTextNode(Js.string pre) in
    let suf = doc##createTextNode(Js.string suf) in
    let x = Html.createSpan doc in
    Dom.appendChild x pre;
    add_core x nm kind;
    Dom.appendChild x suf;
    if eval then add_result ~color:color x (sep, b, bb) active enabled else ();
    let nl = doc##createTextNode(Js.string "\n") in
    Dom.appendChild x nl;
    x in

  let max_len_fst3 l =
    List.fold_left (fun acc (a, _, _) -> max acc (String.length a)) 0 l in

  let pp_undef undef =
    let s_undef =
      List.map
        (fun (nm, has) ->
          (nm, has, present_absent has))
        undef in
    let max_len = max_len_fst3 s_undef in
    List.map
      (fun (nm, has, phas) ->
        let suf = pad "" (max_len - String.length nm) in
        make_item2 ~color:true nm ("    ", suf) (" are ", not has, phas) true true Is_leaf)
      s_undef in

  let toplevel =
    let nm = "consistent_race_free_execution" in
    let b = ecr.Types.masked_ecr_consistent_race_free in
    let pb = ppb () b in
    make_item2 ~color:true nm ("","") (" = ", b, pb) true true Is_node in
  let item_consistent =
    let nm = "consistent_execution" in
    let s = Printf.sprintf "  %s" nm in
    let b = ecr.Types.masked_ecr_consistent_real in
    let pb = ppb () b in
    make_item ~color:true nm ("  ","") (" = ", b, pb) consistent_checked true Is_node in
  let items =
    item_consistent
    :: pp_pred_tree 2 consistent_checked ecr.Types.masked_ecr_pieces_of_consistent in
  let races = pp_undef ecr.masked_ecr_undefined_behaviour in
 
  Dom.appendChild d toplevel;
  List.iter (fun (_, l) -> Dom.appendChild d l) items;
  List.iter (fun l -> Dom.appendChild d l) races;
  (toplevel, items, races)


let make_dummy_masked_ecr model ignorechecks =
  let rec f n active =
    Iso.use_pred_tree
      (fun _ ->
        if active then Masked_pred_leaf false
        else Masked_dead_pred_leaf false)
      (fun bs ->
        let bs2 =
          List.map
            (fun (nm, t) ->
              if (not active) || List.mem nm ignorechecks then (nm, f t false)
              else (nm, f t active))
            bs in
        if active then Masked_pred_node (false, bs2)
        else Masked_dead_pred_node (false, bs2))
      n in
  let x = f model.Cmm.consistent (not (List.mem "consistent_execution" ignorechecks)) in
  let y =
    List.map
      (function
        | Cmm.One (nm, _) -> (nm, false)
        | Cmm.Two (nm, _) -> (nm, false))
      model.Cmm.undefined in
  {
    masked_ecr_pieces_of_consistent = x;
    masked_ecr_undefined_behaviour = y;
    masked_ecr_consistent_real = false;
    masked_ecr_consistent_masked = false;
    masked_ecr_consistent_race_free = false;
  }

let createPredicatesSwitches model doc (eval, ecr_x) =
  let pre_p = Html.createP doc in
  pre_p##id <- Js.string "predicates_p";
  let (top, items, races) = make_execution_check_result_p doc pre_p eval ecr_x in
  set_class pre_p "formatted";
  (top, items, races, pre_p)

let change_relations_handler doc st screen item redraw =
  let nm = Js.to_string item##value in
  if Js.to_bool item##checked then
    st.instructions <- { st.instructions with
      edge_instructions =
        List.filter
          (fun e -> match e with
            | Suppress_e (nm2,Any) -> nm <> nm2
            | _ -> true)
          st.instructions.edge_instructions
    }
  else
    st.instructions <- { st.instructions with
      edge_instructions = (Suppress_e (Js.to_string item##value,Any)) :: st.instructions.edge_instructions
    };
  redraw ()

let initialise_relations_items doc st screen redraw =
  List.iter
    (fun item ->
      item##disabled <- Js._false;
      item##checked <- Js._true;
      if List.mem (Suppress_e (Js.to_string (item##value), Any)) st.instructions.edge_instructions
      then item##checked <- Js._false
      else ())
    screen.relations_items;
  List.iter (fun item -> item##onchange <- Html.handler (fun _ -> change_relations_handler doc st screen item redraw; Js._false)) screen.relations_items


type summary2 = {
  nb_candidates: int;
  nb_consistent: int;
  nb_consistent_race_free: int;
}

let initial_summary2 = {
  nb_candidates = 0;
  nb_consistent = 0;
  nb_consistent_race_free = 0;
}

let make_summ results instructions =
  List.fold_left
    (fun summ (_, ecr) ->
      let mecr = Iso.mask_ecr instructions.ignorechecks ecr in
      {
        nb_candidates = summ.nb_candidates + 1;
        nb_consistent = summ.nb_consistent + (if mecr.masked_ecr_consistent_masked then 1 else 0);
        nb_consistent_race_free = summ.nb_consistent_race_free + (if mecr.masked_ecr_consistent_race_free then 1 else 0);
      })
    initial_summary2 results

let make_info_str doc i n summ =
  let consistent_txt = if summ.nb_consistent > 0 then string_of_int summ.nb_consistent else "no" in
  let add =
    if summ.nb_consistent > 0 then
      (if summ.nb_consistent_race_free = summ.nb_consistent then (", " ^ (if summ.nb_consistent = 1 then "" else "all ") ^ "race free")
       else (", "
             ^ (if summ.nb_consistent_race_free = 0 then ((if summ.nb_consistent = 1 then "not" else "no") ^ " ") else ("only " ^ string_of_int summ.nb_consistent_race_free))
             ^ " race free"))
    else "" in
  let sp = Html.createSpan doc in
  let txt1 = doc##createTextNode(Js.string ("Execution candidate no. " ^ string_of_int (i + 1) ^ " of " ^ string_of_int n)) in
  Dom.appendChild sp txt1;
  let foo = string_of_int summ.nb_candidates ^ " executions; " in
  let txt2 = doc##createTextNode(Js.string (foo ^ consistent_txt ^ " consistent" ^ add)) in
  let sp2 = Html.createSpan doc in
  if (summ.nb_consistent > 0 && summ.nb_consistent_race_free = summ.nb_consistent) then set_class sp2 "valid"
  else set_class sp2 "invalid";
  Dom.appendChild sp2 txt2;
  (sp, sp2)

let compute_masked_ecr st =
  let consistent_checked = not (List.mem "consistent_execution" st.instructions.ignorechecks) in
  let eval, mecr =
    match st.res with
      | None -> false, make_dummy_masked_ecr st.model st.instructions.ignorechecks
      | Some (res, ecr) -> true, Iso.mask_ecr st.instructions.ignorechecks ecr in
  (eval, (consistent_checked, mecr))
        

let rec initialise_predicates_items doc st predicates_rec execution_info program_info =
  List.iter
    (fun (item,_) ->
      if List.mem (Js.to_string item##value) st.instructions.ignorechecks then item##checked <- Js._false
      else item##checked <- Js._true)
    predicates_rec.predicates_items;
  List.iter
    (fun (item,_) ->
      item##onchange <- Html.handler (fun _ ->
        change_predicates_handler st.model doc st predicates_rec execution_info program_info item; Js._false))
    predicates_rec.predicates_items

and renew_predicates_switches doc st predicates_rec execution_info program_info =
  let ecr = compute_masked_ecr st in
  let (top, predicates_items, races, predicates_p) = createPredicatesSwitches st.model doc ecr in
  let old = predicates_rec.predicates_p in
  predicates_rec.predicates_p <- predicates_p;
  predicates_rec.predicates_items <- predicates_items;
  initialise_predicates_items doc st predicates_rec execution_info program_info;
  (match st.res with
    | None -> ()
    | Some _ ->
      let summ = make_summ st.st_results st.instructions in
      let (exec_info_txt, prog_info_txt) = make_info_str doc st.result_index (List.length st.st_results) summ in
      set_as_new_first_child execution_info exec_info_txt;
      set_as_new_first_child program_info prog_info_txt;
      set_class execution_info "visible");
  Dom.replaceChild predicates_rec.predicates_div predicates_p old

and change_predicates_handler model doc st predicates_rec execution_info program_info item =
  let nm = Js.to_string item##value in
  if Js.to_bool item##checked then
    st.instructions <- { st.instructions with
      ignorechecks = List.filter (fun x -> x <> nm) st.instructions.ignorechecks
    }
  else
    st.instructions <- { st.instructions with
      ignorechecks = nm :: st.instructions.ignorechecks
    };
  renew_predicates_switches doc st predicates_rec execution_info program_info;
  ()

let insertAfter x y =
  Js.Opt.iter (x##parentNode)
    (fun p ->
      Js.Opt.case (x##nextSibling)
        (fun () -> Dom.appendChild p y)
        (fun ns -> Dom.insertBefore p y (Js.some ns)))

let interact_with_execution doc lock_data st screen =
  try
    let (res, ecr) = (match st.res with | None -> assert false | Some x -> x) in
    let instrs = st.instructions in
    let ex_filtered = Execfile.filter_out_exwit_wrap res.cutdown_for_drawing instrs in
    let dot = Pp.pp_dot () (instrs.mode,None,ex_filtered) in
    let exc_filtered = Pp.pp_execfile () (st.model_name, instrs.mode, None, ex_filtered) in
    let ex_some = (res.actual.exod, Some res.actual.exed, Some res.actual.exdd) in
    let exc = Pp.pp_execfile () (st.model_name, instrs.mode, None, ex_some) in
    export_files doc screen instrs (res, ex_some, exc, dot);
    (**)
    renew_predicates_switches doc st screen.is_predicates_rec screen.is_execution_info screen.is_program_info;
    Dom.appendChild screen.is_tab_rec.td_top_right screen.is_predicates_rec.predicates_div;
    (**)
    let anim = Html.createImg doc in
    anim##alt <- Js.string "waiting animation";
    anim##src <- Js.string "anim.gif";
    Dom.appendChild screen.is_execution anim;
    let stop_anim () = Dom.removeChild screen.is_execution anim in
    (**)
    fetch_image doc (dot,exc_filtered) st screen lock_data stop_anim
  with
      e ->
        (error_is screen ("Problem with execution: " ^ Printexc.to_string e);
         Lwt.return ())
;;

let make_exports_p doc =
  let exports = Html.createP doc in
  exports##id <- Js.string "exports";
  exports

let make_execution_div doc =
  let d = Html.createDiv doc in
  d##id <- Js.string "execution";
  d

let make_tab doc =
  let tab = Html.createTable doc in
  let tr_top = Html.createTr doc in
  tr_top##id <- Js.string "tr_top";
  let tr_bot = Html.createTr doc in
  tr_bot##id <- Js.string "tr_bot";
  Dom.appendChild tab tr_top;
  Dom.appendChild tab tr_bot;
  let td_top_left = Html.createTd doc in
  td_top_left##id <- Js.string "td_top_left";
  let td_top_right = Html.createTd doc in
  td_top_right##id <- Js.string "td_top_right";
  let td_bot_left = Html.createTd doc in
  td_bot_left##id <- Js.string "td_bot_left";
  let td_bot_right = Html.createTd doc in
  td_bot_right##id <- Js.string "td_bot_right";
  Dom.appendChild tr_top td_top_left;
  Dom.appendChild tr_top td_top_right;
  Dom.appendChild tr_bot td_bot_left;
  Dom.appendChild tr_bot td_bot_right;
  {
    tab = tab;
    tr_top = tr_top;
    tr_bot = tr_bot;
    td_top_left = td_top_left;
    td_top_right = td_top_right;
    td_bot_left = td_bot_left;
    td_bot_right = td_bot_right;
  }

let make_navigation_rec doc =
  let d = Html.createDiv doc in
  d##id <- Js.string "navigation";
  let prev_consistent = Html.createButton doc in
  let txt = doc##createTextNode (Js.string "previous consistent") in
  Dom.appendChild prev_consistent txt;
  Dom.appendChild d prev_consistent;
  let prev = Html.createButton doc in
  let txt = doc##createTextNode (Js.string "previous candidate") in
  Dom.appendChild prev txt;
  Dom.appendChild d prev;
  let next = Html.createButton doc in
  let txt = doc##createTextNode (Js.string "next candidate") in
  Dom.appendChild next txt;
  Dom.appendChild d next;
  let next_consistent = Html.createButton doc in
  let txt = doc##createTextNode (Js.string "next consistent") in
  Dom.appendChild next_consistent txt;
  Dom.appendChild d next_consistent;
  let goto_label = createTextBox doc "goto" in
  goto_label##id <- Js.string "goto_label";
  Dom.appendChild d goto_label;
  let goto_button = Html.createButton doc in
  let txt = doc##createTextNode (Js.string "goto") in
  Dom.appendChild goto_button txt;
  Dom.appendChild d goto_button;
  {
    navigation_div = d;
    prev_consistent = prev_consistent;
    prev = prev;
    next = next;
    next_consistent = next_consistent;
    goto_label = goto_label;
    goto_button = goto_button;
  }

let make_interaction_screen doc start_screen =
  let exports_p = make_exports_p doc in
  let execution_div = make_execution_div doc in
  let tab_rec = make_tab doc in
  {
    is_title = start_screen.title;
    is_execution_info = start_screen.execution_info;
    is_program_info = start_screen.program_info;
    is_prog_rec = start_screen.prog_rec;
    is_model_rec = start_screen.model_rec;
    is_msg = start_screen.msg;
    is_navigation_rec = start_screen.navigation_rec;
    is_exports = exports_p;
    is_execution = execution_div;
    is_relations_items = start_screen.relations_items;
    is_relations_div = start_screen.relations_div;
    is_predicates_rec = start_screen.predicates_rec;
    is_layout_items = start_screen.layout_items;
    is_layout_div = start_screen.layout_div;
    is_tex_item = start_screen.tex_item;
    is_tex_div = start_screen.tex_div;
    is_dsp_rec = start_screen.dsp_rec;
    is_tab_rec = tab_rec;
  }


let make_interaction_screen2 doc old_screen =
  let exports_p = make_exports_p doc in
  let execution_div = make_execution_div doc in
  {
    is_title = old_screen.is_title;
    is_execution_info = old_screen.is_execution_info;
    is_program_info = old_screen.is_program_info;
    is_prog_rec = old_screen.is_prog_rec;
    is_model_rec = old_screen.is_model_rec;
    is_msg = old_screen.is_msg;
    is_navigation_rec = old_screen.is_navigation_rec;
    is_exports = exports_p;
    is_execution = execution_div;
    is_relations_items = old_screen.is_relations_items;
    is_relations_div = old_screen.is_relations_div;
    is_predicates_rec = old_screen.is_predicates_rec;
    is_layout_items = old_screen.is_layout_items;
    is_layout_div = old_screen.is_layout_div;
    is_tex_item = old_screen.is_tex_item;
    is_tex_div = old_screen.is_tex_div;
    is_dsp_rec = old_screen.is_dsp_rec;
    is_tab_rec = old_screen.is_tab_rec;
  }

let layout_interaction_screen doc screen =
  Dom.appendChild screen.is_tab_rec.td_top_left screen.is_title;
  let d = Html.createDiv doc in
  d##id <- Js.string "interaction_screen";
  (**)
  Dom.appendChild d screen.is_tab_rec.tab;
  (**)
  Dom.appendChild screen.is_tab_rec.td_top_left screen.is_model_rec.model_div;
  Dom.appendChild screen.is_tab_rec.td_top_left screen.is_prog_rec.prog_div;
  let config_div = Html.createDiv doc in
  config_div##id <- Js.string "config_div";
  Dom.appendChild config_div screen.is_msg;
  Dom.appendChild config_div screen.is_relations_div;
  Dom.appendChild config_div screen.is_layout_div;
  Dom.appendChild config_div screen.is_tex_div;
  Dom.appendChild config_div screen.is_dsp_rec.dsp_div;
  Dom.appendChild screen.is_tab_rec.td_bot_left config_div;
  (**)
  set_navigation_rec screen.is_navigation_rec Js._false;
  Dom.appendChild screen.is_tab_rec.td_top_right screen.is_execution_info;
  Dom.appendChild screen.is_tab_rec.td_top_right screen.is_navigation_rec.navigation_div;
  let exec_div = Html.createDiv doc in
  exec_div##id <- Js.string "exec_div";
  Dom.appendChild exec_div screen.is_execution;
  Dom.appendChild exec_div screen.is_exports;
  Js.Opt.case (screen.is_tab_rec.td_bot_right##firstChild)
    (fun () -> Dom.appendChild screen.is_tab_rec.td_bot_right exec_div)
    (fun n -> Dom.replaceChild screen.is_tab_rec.td_bot_right exec_div n);
  Dom.appendChild doc##body d;
  d

let js_not b =
  match Js.to_bool b with
    | true -> Js._false
    | false -> Js._true

let set_model_disabled model_rec b =
  List.iter (fun it -> it##disabled <- b) model_rec.model_items;
  set_class model_rec.model_h (if Js.to_bool b then "disabled" else "enabled");
  ()

let set_prog_rec_disabled prog_rec b =
  List.iter (fun it -> it##disabled <- b) prog_rec.input_type_selector_items;
  prog_rec.examples_group_selector##disabled <- b;
  prog_rec.example_selector##disabled <- b;
  prog_rec.ta##readOnly <- b;
  prog_rec.interact_button##disabled <- b;
  prog_rec.reset_button##disabled <- (js_not b);
  set_class prog_rec.prog_h (if Js.to_bool b then "disabled" else "enabled");
  ()

let interact_core doc st predicates_rec (screen,start_screen) redraw_mv =
  let screen_div = layout_interaction_screen doc screen in
  screen.is_prog_rec.reset_button##onclick <- Html.handler (fun _ ->
    st.res <- None;
    st.result_index <- -1;
    st.st_results <- [];
    set_model_disabled screen.is_model_rec Js._false;
    set_prog_rec_disabled screen.is_prog_rec Js._false;
    remove_screen doc screen_div;
    let f () = Lwt.return () in
    Lwt_mvar.take redraw_mv >>= (fun _ ->
      Lwt_mvar.put redraw_mv f >>= (fun () ->
        Lwt.return ()));
    let d = layout_start_screen doc start_screen in
    renew_predicates_switches doc st predicates_rec screen.is_execution_info screen.is_program_info;
    Js._false);
  screen_div

let rec interact_with_single_execution doc st lock_data (start_screen,old_screen_div) redraw_mv =
  remove_screen doc old_screen_div;
  let screen = make_interaction_screen doc start_screen in
  let screen_div = interact_core doc st start_screen.predicates_rec (screen,start_screen) redraw_mv in
  interact_with_execution doc lock_data st screen;
  let f () = interact_with_single_execution doc st lock_data (start_screen,screen_div) redraw_mv in
  Lwt_mvar.take redraw_mv >>= (fun _ ->
    Lwt_mvar.put redraw_mv f >>= (fun () ->
      Lwt.return ()))

let rec find_next_consistent instrs results i next stop =
  let i = next i in
  if i = stop then None
  else
    let (_, ecr) = (List.nth results i) in
    if (Iso.mask_ecr instrs.ignorechecks ecr).Types.masked_ecr_consistent_masked then Some i
    else find_next_consistent instrs results i next stop

let rec interact_with_program_aux doc st ((svg_lock,svg_array)) ((start_screen,start_screen_div) as start,old_screen) redraw_mv =
  let screen =
    (match old_screen with
      | Auxl.Left (screen,screen_div) ->
        remove_screen doc screen.div;
        make_interaction_screen doc screen
      | Auxl.Right (screen,screen_div) ->
        remove_screen doc screen_div;
        make_interaction_screen2 doc screen) in
  let screen_div = interact_core doc st screen.is_predicates_rec (screen,start_screen) redraw_mv in
  (**)
  let n = List.length st.st_results in
  let res = List.nth st.st_results st.result_index in
  let get () = Array.get svg_array st.result_index in
  let set x =  Array.set svg_array st.result_index x in
  st.res <- Some res;
  interact_with_execution doc (svg_lock,(get,set)) st screen;
  (**)
  screen.is_navigation_rec.prev_consistent##onclick <- Html.handler (fun _ ->
    (match find_next_consistent st.instructions st.st_results st.result_index (fun i -> i - 1) (-1) with
      | None -> (error_is screen "No previous consistent."; Lwt.return ())
      | Some j ->
        (log_is screen "Jumping to previous consistent.";
         st.result_index <- j;
         interact_with_program_aux doc st ((svg_lock,svg_array)) (start,Auxl.Right (screen,screen_div)) redraw_mv));
    Js._false);
  (**)
  screen.is_navigation_rec.prev##onclick <- Html.handler (fun _ ->
    st.result_index <- st.result_index - 1;
    interact_with_program_aux doc st ((svg_lock,svg_array)) (start,Auxl.Right (screen,screen_div)) redraw_mv;
    Js._false);
  if st.result_index = 0 then
    (screen.is_navigation_rec.prev_consistent##disabled <- Js._true;
     screen.is_navigation_rec.prev##disabled <- Js._true)
  else ();
  (**)
  screen.is_navigation_rec.next##onclick <- Html.handler (fun _ ->
    st.result_index <- st.result_index + 1;
    interact_with_program_aux doc st ((svg_lock,svg_array)) (start,Auxl.Right (screen,screen_div)) redraw_mv;
    Js._false);
  (**)
  screen.is_navigation_rec.next_consistent##onclick <- Html.handler (fun _ ->
    (match find_next_consistent st.instructions st.st_results st.result_index (fun i -> i + 1) n with
      | None -> (error_is screen "No next consistent."; Lwt.return ())
      | Some j ->
        (log_is screen "Jumping to next consistent.";
         st.result_index <- j;
         interact_with_program_aux doc st ((svg_lock,svg_array)) (start,Auxl.Right (screen,screen_div)) redraw_mv));
    Js._false);
  if st.result_index + 1 = n then
    (screen.is_navigation_rec.next_consistent##disabled <- Js._true;
     screen.is_navigation_rec.next##disabled <- Js._true)
  else ();
  screen.is_navigation_rec.goto_label##value <- Js.string (string_of_int (st.result_index + 1));
  screen.is_navigation_rec.goto_button##onclick <- Html.handler (fun _ ->
    let js = Js.to_string (screen.is_navigation_rec.goto_label##value) in
    let j = (try Some (int_of_string js) with Failure "int_of_string" -> None) in
    (match j with
      | None -> (error_is screen ("Malformed execution candidate no: \"" ^ js ^ "\"."); Lwt.return ())
      | Some j ->
        let j = j - 1 in
        if j < 0 || j >= n then (error_is screen ("There is no execution candidate no. " ^ js ^ "."); Lwt.return ())
        else
          (st.result_index <- j;
           interact_with_program_aux doc st ((svg_lock,svg_array)) (start,Auxl.Right (screen,screen_div)) redraw_mv));
    Js._false);
  (**)
  let f () = interact_with_program_aux doc st ((svg_lock,svg_array)) (start,Auxl.Right (screen,screen_div)) redraw_mv in
  Lwt_mvar.take redraw_mv >>= (fun _ ->
    Lwt_mvar.put redraw_mv f >>= (fun () ->
      Lwt.return ()))

let make_svg_array n =
  Array.make n None

let interact_with_program doc st cilf (screen,screen_div) redraw_mv =
  Lwt.return () >>= (fun _ ->
    try
      (try
         Value.reset_symbol_generation ();
         let summ = RunOpsem.proc_file (st.model, st.model_name) base_instructions (None, cilf) interaction_handles RunOpsem.Collect_all in
         st.st_results <- List.rev summ.results;
         (match find_next_consistent st.instructions st.st_results (-1) (fun i -> i + 1) (List.length summ.results) with
           | None -> st.result_index <- 0
           | Some j -> st.result_index <- j);
         log_ss screen "Computed executions";
         if List.length st.st_results = 0 then
           (log_ss screen "The program has no executions.";
            set_model_disabled screen.model_rec Js._false;
            set_prog_rec_disabled screen.prog_rec Js._false;
            Lwt.return ())
         else
           let svg_lock = Lwt_mutex.create () in
           let svg_array = make_svg_array (List.length st.st_results) in
           (try interact_with_program_aux doc st ((svg_lock,svg_array)) ((screen,screen_div), Auxl.Left (screen,screen_div)) redraw_mv
            with e -> (error_ss screen ("Problem interacting: " ^ Printexc.to_string e); Lwt.return ()));
           Lwt.return ()
       with
           e ->
             (error_ss screen ("Problem computing executions: " ^ Printexc.to_string e);
              Lwt.return ()))
    with
        e -> (error_ss screen ("Problem parsing file: " ^ Printexc.to_string e);
              Lwt.return ()));
  ()
;;


let example_type s =
  let x =
    Auxl.fold_right_string
      (fun c acc -> match acc with
        | Auxl.Left l ->
          if c = '.' then Auxl.Right (Auxl.string_of_char_list l)
          else Auxl.Left (c :: l)
        | Auxl.Right s -> Auxl.Right s)
      s (Auxl.Left []) in
  match x with
    | Auxl.Left _ -> Auxl.Left ()
    | Auxl.Right s -> Auxl.Right s

type input_type = C | Exc

let string_of_input_type = function
  | C -> "c"
  | Exc -> "exc"

let name_of_input_type = function
  | C -> "C"
  | Exc -> "Execution"

let input_types = [C; Exc]

let input_types_map = List.map (fun ty -> (string_of_input_type ty, ty)) input_types

let input_types_map2 = List.map (fun ty -> (name_of_input_type ty, string_of_input_type ty)) input_types

let input_type_of_string s =
  try
    Some (List.assoc s input_types_map)
  with
      Not_found -> None


let set_selected_input_type input_type_selector_items ty =
  let s = string_of_input_type ty in
  List.iter
    (fun item ->
      if Js.to_string item##value = s then item##checked <- Js._true
      else item##checked <- Js._false)
    input_type_selector_items

let load_example name ta input_type_selector_items =
  let ex = name in
  (http_get ex >>= fun s ->
   ta##value <- Js.string s;
   (match example_type ex with
     | Auxl.Left () -> () (* TODO: signal error *)
     | Auxl.Right ext ->
       let ty =
         (match input_type_of_string ext with
           | None ->  C
           | Some ty -> ty) in
       set_selected_input_type input_type_selector_items ty);
   Lwt.return ())


let createLabeledRadioButton ?(link=false) doc name label value =
  let rb = createRadioButton doc name in
  let lab = Html.createLabel doc in
  rb##value <- Js.string value;
  Dom.appendChild lab rb;
  let txt = doc##createTextNode(Js.string label) in
  if link then
    (let a = Html.createA doc in
     set_class a "predicate";
     a##href <- Js.string ("cmm.html#" ^ label);
     a##target <- Js.string "_cmm";
     Dom.appendChild a txt;
     Dom.appendChild lab a)
  else Dom.appendChild lab txt;
  (rb, lab)

let createSelector ?(link=false) doc name dict =
  let rbs =
    List.map
      (fun (label,value) ->
        createLabeledRadioButton ~link:link doc name label value)
      dict in
  let div = Html.createDiv doc in
  List.iter
    (fun lb ->
      Dom.appendChild div lb;
     let space = make_space doc in
     Dom.appendChild div space)
    (List.map snd rbs);
  (List.map fst rbs,div)

let createModelSelector doc st selected_model_name =
  let models = List.map fst Atomic.the_models in
  let (items,div) = createSelector ~link:true doc "model" (List.map (fun x -> (x,x)) models) in
  div##id <- Js.string "model_div";
  let h2 = Html.createH2 doc in
  Dom.appendChild h2 (doc##createTextNode(Js.string "Model"));
  Js.Opt.iter (div##firstChild)
    (fun n -> Dom.insertBefore div h2 (Js.some n));
  List.iter
    (fun it ->
      if Js.to_string it##value = selected_model_name then
        it##checked <- Js._true
      else ())
    items;
  {
    model_div = div;
    model_h = h2;
    model_items = items;
  }

let createInputTypeSelector doc =
  let (items,div) = createSelector doc "input_type" input_types_map2 in
  div##id <- Js.string "input_type_div";
  (List.nth items 0)##checked <- Js._true;
  (items,div)

let layouts = [LO_dot; LO_neato_par; LO_neato_par_init; LO_neato_downwards]

let layout_map = List.map (fun lo -> (Pp.pp_layout () lo,lo)) layouts

let createLayoutSelector doc =
  let (items,div) = createSelector doc "layout" (List.map (fun x -> (x,x)) (List.map (Pp.pp_layout ()) layouts)) in
  div##id <- Js.string "layout_div";
  let h2 = Html.createH2 doc in
  Dom.appendChild h2 (doc##createTextNode(Js.string "Display Layout"));
  Js.Opt.iter (div##firstChild)
    (fun n -> Dom.insertBefore div h2 (Js.some n));
  (List.nth items 0)##checked <- Js._true;
  (items,div)

let createLabeledCheckbox doc name label value =
  let cb = createCheckbox doc name in
  let lab = Html.createLabel doc in
  cb##value <- Js.string value;
  Dom.appendChild lab cb;
  Dom.appendChild lab (doc##createTextNode (Js.string label));
  (cb, lab)

let create_tex_switch doc =
  let n = "tex" in
  let (cb, lab) = createLabeledCheckbox doc n n n in
  cb##checked <- Js._false;
  let d = Html.createDiv doc in
  Dom.appendChild d lab;
  (cb, d)

let create_switches_items doc names =
  List.map
    (fun n ->
      let (cb, lab) = createLabeledCheckbox doc n n n in
      cb##checked <- Js._true;
      (cb, lab))
    names

let createRelationsSwitches model doc =
  let items1 = create_switches_items doc ["sb";"asw";"dd";"cd"] in
  let items2 =
    create_switches_items doc
    ((if model.Cmm.relation_flags.Cmm.rf_flag then ["rf"] else [])
     @ (if model.Cmm.relation_flags.Cmm.mo_flag then ["mo"] else [])
     @ (if model.Cmm.relation_flags.Cmm.sc_flag then ["sc"] else [])
     @ (match model.Cmm.relation_flags.Cmm.lo_flag with | Some _ -> ["lo"] | None -> [])
     @ (if model.Cmm.relation_flags.Cmm.ao_flag then ["ao"] else [])
     @ (if model.Cmm.relation_flags.Cmm.tot_flag then ["tot"] else [])) in
  let items3 = create_switches_items doc (Iso.names_of_derived_relations model) in
  let items4 = create_switches_items doc (Iso.names_of_undefined_behaviour_relations model) in
  let div = Html.createDiv doc in
  div##id <- Js.string "edges_div";
  let h2 = Html.createH2 doc in
  Dom.appendChild h2 (doc##createTextNode(Js.string "Display Relations"));
  Dom.appendChild div h2;
  let add_line items = 
    List.iter
      (fun (cb, lb) ->
        Dom.appendChild div lb;
      let space = make_space doc in
      Dom.appendChild div space) items in
  add_line items1;
  let br1 = Html.createBr doc in
  Dom.appendChild div br1;
  add_line items2;
  let br2 = Html.createBr doc in
  Dom.appendChild div br2;
  add_line items3;
  let br3 = Html.createBr doc in
  Dom.appendChild div br3;
  add_line items4;
  (List.map fst (items1 @ items2 @ items3 @ items4), div)

type file_type = C_file of Cabs.file | Exc_file of execution_data

let input_type_of_string = function
  | "c" -> Some C
  | "exc" -> Some Exc
  | _ -> None

let input_type_of its =
  List.fold_left (fun acc rb ->
    match acc with
      | Some t -> Some t
      | None ->
        if (Js.to_bool rb##checked) then input_type_of_string (Js.to_string rb##value)
        else None) None its

let parse_c s =
  try Auxl.Right (C_file (Frontc.parse_string_to_cabs s))
  with e -> Auxl.Left (Printexc.to_string e)

let parse_exc (model, model_name) s =
  try
    (* TODO: jp: bad duplication *)
    let ((exod,exedo,exddo),_,ignorechecks) = Execfile.read_exec_string (model, model_name) false s "" None in
    let exed =
      match exedo with
        | None -> empty_execution_existential_data model
        | Some exed -> exed 
    in
    let exp_exdd =
      match exddo with
        | None -> empty_execution_derived_data
        | Some exdd -> exdd 
    in
    let actual_exdd = Iso.derive_data model exod exed in
    let exwit = {exod=exod;exed=exed;exdd=actual_exdd} in
    Auxl.Right (Exc_file exwit)
  with
    | Auxl.Exc_lex_error (c, line, col) ->
      Auxl.Left (Printf.sprintf "unexpected character `%c', line %d, column %d" c line col)
    | Execfile.Exc_read_error s -> Auxl.Left s
    | e -> Auxl.Left (Printexc.to_string e)

let interact_with doc st res (screen,screen_div) redraw_mv =
  match res with
    | Exc_file exwit -> begin
      let get () = None in
      let set x = () in
      let ecr = Iso.check_result st.model exwit in
      (* TODO: jp: cut down *)
      st.res <- Some ({actual=exwit;cutdown_for_drawing=exwit}, ecr);
      interact_with_single_execution doc st ((Lwt_mutex.create (),(get,set))) (screen,screen_div) redraw_mv;
      ()
    end
    | C_file c -> interact_with_program doc st c (screen,screen_div) redraw_mv

let make_dsp_rec doc =
  let dsp_button = Html.createButton doc in
  dsp_button##id <- Js.string "dsp_button";
  let txt = doc##createTextNode (Js.string "edit display options") in
  Dom.appendChild dsp_button txt;
  let dsp_ta = Html.createTextarea doc in
  dsp_ta##id <- Js.string "dsp";
  dsp_ta##rows <- 10;
  dsp_ta##cols <- 30;
  let dsp_ta_button = Html.createButton doc in
  let txt = doc##createTextNode (Js.string "save") in
  Dom.appendChild dsp_ta_button txt;
  let dsp_ta_div = Html.createDiv doc in
  dsp_ta_div##id <- Js.string "dsp_ta_div";
  let dsp_div = Html.createDiv doc in
  dsp_div##id <- Js.string "dsp_div";
  let dsp_rec = {
    dsp_button = dsp_button;
    dsp_ta = dsp_ta;
    dsp_ta_button = dsp_ta_button;
    dsp_ta_div = dsp_ta_div;
    dsp_div = dsp_div;
  } in
  layout_dsp_rec doc dsp_rec;
  dsp_rec

let initialise_layout layout_items instrs =
  List.iter
    (fun item ->
      item##disabled <- Js._false;
      if Js.to_string (item##value) = Pp.pp_layout () instrs.mode.layout
      then item##checked <- Js._true
      else ())
    layout_items

let initialise_tex tex_item instrs =
  tex_item##disabled <- Js._false;
  tex_item##checked <- if instrs.mode.texmode then Js._true else Js._false

(* TODO: jp: make this robust *)
let get_selected_option selector =
  let i = selector##selectedIndex in
  let sels = selector##childNodes in
  let sel_raw_opt = sels##item(i) in
  let sel_raw = Js.Opt.get sel_raw_opt (fun _ -> assert false) in
  let sel_opt : Html.optionElement Js.t Js.opt = Js.some (Js.Unsafe.coerce sel_raw) in
  let sel = Js.Opt.get sel_opt (fun _ -> assert false) in
  sel

let set_options doc s opts default =
  let rec clean () =
    Js.Opt.case (s##firstChild)
      (fun () -> ())
      (fun c -> Dom.removeChild s c; clean ()) in
  clean ();
  let os =
    List.map
      (fun ex ->
        let ex = Js.string ex in
        let o = Html.createOption doc in
        o##value <- ex;
        let txt = doc##createTextNode(ex) in
        Dom.appendChild o txt;
        Dom.appendChild s o;
        o)
      opts in
  let idx =
    List.fold_left
      (fun acc it ->
        match acc with
          | Auxl.Left i ->
            if (Js.to_string (it##value)) = default then Auxl.Right i
            else Auxl.Left (i + 1)
          | Auxl.Right i -> Auxl.Right i) (Auxl.Left 0) os in
  (match idx with
    | Auxl.Right idx -> s##selectedIndex <- idx
    | Auxl.Left _ -> s##selectedIndex <- 0);
  os

let load_example_list doc ta input_type_selector_items =
  let s = Html.createSelect doc in
  s##id <- Js.string "examples_group_selector";
  let s2 = Html.createSelect doc in
  s2##id <- Js.string "example_selector";
  (http_get "examples.json" >>= fun ss ->
   let x = Array.to_list (json##parse (Js.string ss)) in
   let y = List.map (fun a -> List.map Js.to_string (Array.to_list a)) x in
   let examples =
     Auxl.option_map
       (function
         | [] -> None
         | h :: tl -> Some (h, tl))
       y in
   let dirs = List.map (fun (h, a) -> h) examples in
   let default_examples_group =
     let x = "examples/Paper" in
     if List.mem x dirs then x
     else List.nth dirs 0 in
   let s_options = set_options doc s dirs default_examples_group in
   let this_dir = List.assoc default_examples_group examples in
   let default_example_name =
     let x = "sc_atomics.c" in
     if List.mem x this_dir then x
     else List.nth this_dir 0 in
   let s2_options = set_options doc s2 this_dir default_example_name in
   s##onchange <- Html.handler (fun _ ->
     let sel = Js.to_string ((get_selected_option s)##value) in
     let this_dir = (try List.assoc sel examples with Not_found -> []) in
     set_options doc s2 this_dir "t2.c";
     let sel2 = Js.to_string ((get_selected_option s2)##value) in
     load_example (sel ^ "/" ^ sel2) ta input_type_selector_items;
     Js._false);
   load_example (default_examples_group ^ "/" ^ default_example_name) ta input_type_selector_items);
  (s, s2)

let make_reset_button doc =
  let reset = Html.createButton doc in
  reset##id <- Js.string "reset";
  let txt = doc##createTextNode (Js.string "reset") in
  Dom.appendChild reset txt;
  reset

let make_prog_rec doc =
  let (input_type_selector_items,input_type_selector_div) = createInputTypeSelector doc in
  let ta = Html.createTextarea doc in
  ta##id <- Js.string "program";
  ta##value <- Js.string example_c_program;
  let (examples_group_selector, example_selector) = load_example_list doc ta input_type_selector_items in
  let interact_button = Html.createButton doc in
  interact_button##id <- Js.string "interact";
  let txt = doc##createTextNode (Js.string "run") in
  Dom.appendChild interact_button txt;
  let reset_button = make_reset_button doc in
  reset_button##disabled <- Js._true;
  let prog_div = Html.createDiv doc in
  prog_div##id <- Js.string "prog_div";
  let h2 = Html.createH2 doc in
  Dom.appendChild h2 (doc##createTextNode(Js.string "Program"));
  Dom.appendChild prog_div h2;
  Dom.appendChild prog_div examples_group_selector;
  Dom.appendChild prog_div example_selector;
  Dom.appendChild prog_div input_type_selector_div;
  ta##rows <- 17;
  ta##cols <- 70;
  ta##readOnly <- Js._false;
  Dom.appendChild prog_div ta;
  Dom.appendChild prog_div interact_button;
  Dom.appendChild prog_div reset_button;
  Js.Opt.iter (doc##getElementById(Js.string "help"))
    (fun h ->
      h##style##position <- Js.string "static";
      h##style##top <- Js.string "";
      h##style##right <- Js.string "";
      h##style##display <- Js.string "inline";
      Dom.appendChild prog_div h);
  {
    prog_h = h2;
    input_type_selector_items = input_type_selector_items;
    input_type_selector_div = input_type_selector_div;
    examples_group_selector = examples_group_selector;
    example_selector = example_selector;
    ta = ta;
    prog_div = prog_div;
    interact_button = interact_button;
    reset_button = reset_button;
  }

let make_start_screen doc st model_name redraw =
  let title =
    Js.Opt.case (doc##getElementById(Js.string "title"))
      (fun () -> assert false)
      (fun t -> t) in
  let tab_rec = make_tab doc in
  let model_rec = createModelSelector doc st model_name in
  let prog_rec = make_prog_rec doc in
  let msg = Html.createP doc in
  msg##id <- Js.string "log";
  let (relations_items,relations_div) = createRelationsSwitches st.model doc in
  (**)
  let predicates_div = Html.createDiv doc in
  predicates_div##id <- Js.string "predicates_div";
  let execution_info = Html.createH2 doc in
  execution_info##id <- Js.string "execution_info";
  Dom.appendChild execution_info (doc##createTextNode(Js.string "-"));
  set_class execution_info "invisible";
  let program_info = Html.createSpan doc in
  program_info##id <- Js.string "program_info";
  Dom.appendChild program_info (doc##createTextNode(Js.string "-"));
  set_class program_info "invisible";
  let navigation_rec = make_navigation_rec doc in
  let predicates_h3 = Html.createH3 doc in
  Dom.appendChild predicates_h3 (doc##createTextNode(Js.string "Model Predicates"));
  Dom.appendChild predicates_div predicates_h3;
  let predicates_p = Html.createP doc in (* dummy *)
  predicates_p##id <- Js.string "predicates_p";
  Dom.appendChild predicates_div predicates_p;
  let predicates_rec = {
    predicates_div = predicates_div;
    predicates_p = predicates_p;
    predicates_items = [];
  } in
  renew_predicates_switches doc st predicates_rec execution_info program_info;
  (**)
  let (layout_items,layout_div) = createLayoutSelector doc in
  let (tex_item, tex_div) = create_tex_switch doc in
  let dsp_rec = make_dsp_rec doc in
  let screen = {
    title = title;
    execution_info = execution_info;
    program_info = program_info;
    model_rec = model_rec;
    msg = msg;
    relations_div = relations_div;
    relations_items = relations_items;
    predicates_rec = predicates_rec;
    layout_div = layout_div;
    layout_items = layout_items;
    tex_item = tex_item;
    tex_div = tex_div;
    dsp_rec = dsp_rec;
    prog_rec = prog_rec;
    tab_rec = tab_rec;
    navigation_rec = navigation_rec;
    div = Html.createDiv doc; (* dummy *)
  } in
  initialise_relations_items doc st screen redraw;
  initialise_layout layout_items st.instructions;
  initialise_predicates_items doc st screen.predicates_rec screen.execution_info screen.program_info;
  screen

let dot_test_file = "digraph G { 1 -> 2; }\n"

let server_timeout = 40000

let is_good_msg msg =
  let good_start = "<svg" in
  let n = String.length good_start in
  (String.length msg > n)
  && (String.sub msg 0 n = good_start)

let test_graphviz_server doc screen =
  let timeouted = ref false in
  let h = Dom.handler (fun _ -> timeouted := true; Js._false) in
  query_graphviz_server ~timeout:(Some (server_timeout, h)) LO_dot dot_test_file
  >>= fun {XmlHttpRequest.code = cod; content = msg } ->
  if (cod = 0 || cod = 200) && not !timeouted && is_good_msg msg then
    Lwt_mvar.put server_is_alive true >>=
      (fun _ ->
        log_ss screen "graphviz server is OK";
        Lwt.return ())
  else
    (Lwt_mvar.put server_is_alive false >>=
       (fun _ ->
         warning_ss screen "graphviz server not working";
         fst (Lwt.wait ())))

let example_handler doc screen =
  let grp = (get_selected_option screen.prog_rec.examples_group_selector)##value in
  let ex = (get_selected_option screen.prog_rec.example_selector)##value in
  let nm = Js.to_string grp ^ "/" ^ Js.to_string ex in
  load_example nm screen.prog_rec.ta screen.prog_rec.input_type_selector_items

let interact_handler doc st screen redraw_mv =
  let co =
    let input_type = input_type_of screen.prog_rec.input_type_selector_items in
    match input_type with
      | None -> Auxl.Left "no input file type"
      | Some t ->
        let input_text = Js.to_string screen.prog_rec.ta##value in
        (match t with
          | C -> parse_c input_text
          | Exc -> parse_exc (st.model, st.model_name) input_text) in
  match co with
    | Auxl.Right res ->
      set_model_disabled screen.model_rec Js._true;
      set_prog_rec_disabled screen.prog_rec Js._true;
      interact_with doc st res (screen, screen.div) redraw_mv;
      ()
    | Auxl.Left err ->
      error_ss screen ("Can't parse: " ^ err);
      ()

let freeze_predicates screen =
  List.iter (fun (i,_) -> i##disabled <- Js._true) screen.predicates_rec.predicates_items

let open_dsp_handler doc st screen =
  List.iter (fun i -> i##disabled <- Js._true) screen.relations_items;
  List.iter (fun i -> i##disabled <- Js._true) screen.layout_items;
  List.iter (fun i -> i##disabled <- Js._true) screen.model_rec.model_items;
  screen.tex_item##disabled <- Js._true;
  freeze_predicates screen;
  Dom.removeChild screen.dsp_rec.dsp_div screen.dsp_rec.dsp_button;
  (* TODO: jp: empty exod? *)
  screen.dsp_rec.dsp_ta##value <- Js.string (Pp.pp_dsp () (Pp.Dsp_verbose, ppmode_default_web, empty_execution_opsem_data, st.instructions));
  Dom.appendChild screen.dsp_rec.dsp_div screen.dsp_rec.dsp_ta_div;
  ()

let close_dsp_handler doc st screen redraw =
  try
    let dummy_model = snd (List.nth Atomic.the_models 0) in
    let s = Js.to_string screen.dsp_rec.dsp_ta##value in
    let instrs = Execfile.read_instructions_string s (empty_execution_data dummy_model) base_instructions in
    st.instructions <- instrs;
    Dom.removeChild screen.dsp_rec.dsp_div screen.dsp_rec.dsp_ta_div;
    Dom.appendChild screen.dsp_rec.dsp_div screen.dsp_rec.dsp_button;
    initialise_relations_items doc st screen redraw;
    initialise_layout screen.layout_items st.instructions;
    initialise_tex screen.tex_item st.instructions;
    renew_predicates_switches doc st screen.predicates_rec screen.execution_info screen.program_info;
    List.iter (fun i -> i##disabled <- Js._false) screen.model_rec.model_items;
    log_ss screen "Saved instructions";
    redraw ()
  with e ->
    (error_ss screen (Printexc.to_string e);
     Lwt.return ())

let renew_relations_div doc st screen redraw =
  let (relations_items,relations_div) = createRelationsSwitches st.model doc in
  let old = screen.relations_div in
  screen.relations_div <- relations_div;
  screen.relations_items <- relations_items;
  initialise_relations_items doc st screen redraw;
  Js.Opt.iter (old##parentNode)
    (fun p -> Dom.replaceChild p relations_div old)

let change_model_handler doc st screen item redraw =
  if Js.to_bool item##checked then
    (let model_name = Js.to_string item##value in
     let model = List.assoc model_name Atomic.the_models in
     st.model <- model;
     st.model_name <- model_name;
     st.instructions <- { st.instructions with
       ignorechecks = initial_instructions.ignorechecks
     };
     renew_relations_div doc st screen redraw;
     renew_predicates_switches doc st screen.predicates_rec screen.execution_info screen.program_info)
  else ()
  
let change_layout_handler doc st screen item redraw =
  if Js.to_bool item##checked then
    match (try Some (List.assoc (Js.to_string (item##value)) layout_map) with Not_found -> None) with
      | None -> Lwt.return ()
      | Some layout ->
        (st.instructions <- { st.instructions with
          mode = { st.instructions.mode with
            layout = layout;
          }
         };
         redraw ())
  else Lwt.return ()

let change_tex_handler doc st screen item redraw =
  let texmode = Js.to_bool item##checked in
  st.instructions <- { st.instructions with
    mode = { st.instructions.mode with
      texmode = texmode;
    }
  };
  redraw ()

let start () =
  Globals.quietmode := true;
  let model_name = Atomic.default_model_name in
  let st =  {
    model_name = model_name;
    model = List.assoc model_name Atomic.the_models;
    instructions = initial_instructions;
    res = None;
    st_results = [];
    result_index = -1;
  } in
  let doc = Html.document in
  let redraw_mv = Lwt_mvar.create (fun () -> Lwt.return ()) in
  let redraw () = Lwt_mvar.take redraw_mv >>= (fun f -> Lwt_mvar.put redraw_mv f >>= (fun () -> f ())) in
  let screen = make_start_screen doc st model_name redraw in
  let screen_div = layout_start_screen doc screen in
  test_graphviz_server doc screen;
  (**)
  List.iter (fun item -> item##onchange <- Html.handler (fun _ -> change_model_handler doc st screen item redraw; Js._false)) screen.model_rec.model_items;
  List.iter (fun item -> item##onchange <- Html.handler (fun _ -> change_layout_handler doc st screen item redraw; Js._false)) screen.layout_items;
  screen.tex_item##onchange <- Html.handler (fun _ -> change_tex_handler doc st screen screen.tex_item redraw; Js._false);
  screen.prog_rec.example_selector##onchange <- Html.handler (fun _ -> example_handler doc screen; Js._false);
  screen.dsp_rec.dsp_button##onclick <- Html.handler (fun _ -> open_dsp_handler doc st screen; Js._false);
  screen.dsp_rec.dsp_ta_button##onclick <- Html.handler (fun _ -> close_dsp_handler doc st screen redraw; Js._false);
  screen.prog_rec.interact_button##onclick <- Html.handler (fun _ -> interact_handler doc st screen redraw_mv; Js._false);
  ()

let _ =
  Html.window##onload <- Html.handler
    (fun _ ->
      ignore (start ());
      Js._false)
