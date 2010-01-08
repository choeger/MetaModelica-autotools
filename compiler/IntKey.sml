(* util/int-key.sml *)

structure IntKey : ORD_KEY =
  struct
    type ord_key = int
    val compare = Int.compare
  end (* structure IntKey *)
