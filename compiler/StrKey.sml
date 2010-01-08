(* util/str-key.sml *)

structure StrKey : ORD_KEY =
  struct
    type ord_key = string
    val compare = String.compare
  end (* structure StrKey *)
