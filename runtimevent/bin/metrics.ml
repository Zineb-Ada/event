let namespace = "runtime"

let subsystem = "main"

module Counters = struct
  open Prometheus

let minor_promoted =
  let help = "Number of words promoted to the shared heap" in
  Counter.v ~help ~namespace ~subsystem "minor_promoted"

let minor_allocated =
  let help = "Number of allocated words on the minor heap" in
  Counter.v ~help ~namespace ~subsystem "minor_allocated"

end