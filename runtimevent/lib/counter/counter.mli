module Event = Runtime_events

val runtime_counter : 'a -> Event.Timestamp.t -> Event.runtime_counter -> 'b -> unit

val get_counter : ?counter_name_input:string -> unit -> unit