(* ReorderTy.sml *)

structure ReorderTy =
  ReorderTyFn(structure Util = Util
              structure Absyn = Absyn
              structure ReorderSCC = ReorderSCC
		);
