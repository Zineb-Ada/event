open Phase
open Counter

(* open Cmdliner *)
module Event = Runtime_events

(* let rec hanoi depart milieu arrivee = function 
| 0 -> ()
| n -> hanoi depart arrivee milieu (n - 1); 
        hanoi milieu depart arrivee (n - 1) *)

let bind o f =
  match o with
  | Some x  -> f x
  | None    -> None

let (let*) x f = bind x f

let timeout =
    let* p = Some (Lwt_unix.sleep 10.) in
    Some (Lwt.return None)

let _ =
  Event.start ();
  (* Event.dir="/Users/tarides/Desktop/Tarides-Ocaml/ppxlib"; *)
  (* Event.dir="/Users/tarides/Desktop/Tarides-Ocaml/irmin"; *)
  let cursor = Event.create_cursor None in
  let callbacks = Event.Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter ()
  in
    let _ = String.make 1024 'c' in
    Gc.full_major ();
    (* Event.pause ();
    let _ = timeout in
    (* Lwt_unix.timeout 5.0; *)
    Event.start (); *)
    Gc.full_major ();
    (* faire une boucle qui va aretter le programme un petit moment et le relance *)
    (* hanoi "A" "B" "C" 3; *)
    ignore(Event.read_poll cursor callbacks None);
    Unix.sleep 1;
    if Array.length Sys.argv = 2  then get_phases ~phase_name:(Sys.argv.(1)) ()
    else if Array.length Sys.argv > 2 then get_phases ~phase_name:(Sys.argv.(1)) ~counter_name_input:(Sys.argv.(2)) ()
    else get_phases ()

(* let get_elements phases counters = get_phases (get_counter ())
let count =
  let doc = "get phases $(docv)" in
  Arg.(value & opt string "" & info ["p"; "phases"] ~docv:"PHASES" ~doc)
let msg =
  (* let env = *)
    let doc = "Overrides the default message to print." in
    (* Cmd.Env.info "ELEMENTS_MSG" ~doc
  in
  let doc = "The message to print." in *)
  Arg.(value & opt (some string) None "" & info ["c", "counters"] ~docv:"COUNTERS" ~doc)

let get_elements_t = Term.(const get_elements $ count $ msg)
let cmd =
  let doc = "print elements" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <bugs@example.org>." ]
  in
  let info = Cmd.info "get_elements" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info get_elements_t

let main () = exit (Cmd.eval cmd)
let () = main () *)


(* let chorus count msg = for _ = 1 to count do print_endline msg done
let count =
  let doc = "Repeat the message $(docv) times." in
  Arg.(value & opt int 10 & info ["c"; "count"] ~docv:"COUNT" ~doc)
let msg =
  let env =
    let doc = "Overrides the default message to print." in
    Cmd.Env.info "CHORUS_MSG" ~doc
  in
  let doc = "The message to print." in
  Arg.(value & pos 0 string "Revolt!" & info [] ~env ~docv:"MSG" ~doc)

let chorus_t = Term.(const chorus $ count $ msg)
let cmd =
  let doc = "print a customizable message repeatedly" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <bugs@example.org>." ]
  in
  let info = Cmd.info "chorus" ~version:"%‌%VERSION%%" ~doc ~man in
  Cmd.v info chorus_t

let main () = exit (Cmd.eval cmd)
let () = main () *)



(* command + -phase ou -counter +optionnal phase *)

(* let () = 
  (* match Array.length Sys.argv with
    |2 -> get_phases ~phase_name:(Sys.argv.(1)) ()
    |(>2) -> *)
  if Array.length Sys.argv = 2  then get_phases ~phase_name:(Sys.argv.(1)) ()
  else if Array.length Sys.argv > 2 then get_phases ~phase_name:(Sys.argv.(1)) ~counter_name_input:(Sys.argv.(2)) ()
  else get_phases () *)
