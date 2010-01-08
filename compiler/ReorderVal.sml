(* ReorderVal.sml *)

structure ReorderVal =
  ReorderValFn(structure Util = Util
               structure Absyn = Absyn
               structure ReorderSCC = ReorderSCC
		 );
