let setup () =
  Gc.set { (Gc.get()) with
    Gc.minor_heap_size = 1048576;  (* 8 MiB, 4x the default *)
    Gc.space_overhead = 80;        (* collect more aggressively than the default 120 *)
  }
