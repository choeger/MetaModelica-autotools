(* reorder/reorder-scc.sig *)

signature REORDER_SCC =
  sig

    val scc: ('a * int list) vector -> 'a list list

  end (* signature REORDER_SCC *)
