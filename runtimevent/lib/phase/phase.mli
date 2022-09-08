module Event = Runtime_events

val runtime_begin : 'a -> Event.Timestamp.t -> Event.runtime_phase -> unit

val runtime_end : 'a -> Event.Timestamp.t -> Event.runtime_phase -> unit

(* val printing_with_param : unit  *)

val get_phases : ?phase_name:string -> ?counter_name_input:string -> unit -> unit