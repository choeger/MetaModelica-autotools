(* TyScheme.sml *)

structure TyScheme =
  TySchemeFn(structure Util = Util
             structure TyComb = TyComb);
