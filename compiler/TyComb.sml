(* TyComb.sml *)

structure TyComb =
  TyCombFn(structure Util = Util
           structure Ty = Ty);
