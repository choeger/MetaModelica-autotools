(* code/plain.sig *)

signature PLAIN =
  sig

    (* datatype gvar' = GVAR of string *)
    include CODE
    (* sharing type gvar = gvar' *)

  end (* signature PLAIN *)
