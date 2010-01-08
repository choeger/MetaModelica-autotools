(* static/tyfcn.sml *)

functor TyFcnFn(structure Util : UTIL
		structure TyComb : TY_COMB
		  ) : TY_FCN =
  struct

    structure Ty = TyComb.Ty

    datatype tyfcn = THETA of int * (* arity of the type function *)
                              TyComb.tycomb 

    fun lambda(bvars, ty) = THETA(length bvars, TyComb.absBvars(bvars, ty))

    fun arity(THETA(ary,_)) = ary

    fun apply(THETA(ary,comb), args) =
      if ary = length args then TyComb.apply(comb, args)
      else Util.bug "TyFcn.apply: arity error"
      
    fun admitsEq(THETA(ary,comb)) =
      (* XXX: This conses a bit, but it's only called to check eqtype specs.
       * Perhaps there should be a TyComb.admitsEq?
       * The only requirement on mkeqty is that it return types
       * tau for who Ty.admitsEq(tau, true) returns true.
       *)
      let fun mkeqty _ = Ty.VAR(Ty.RIGID "'a")
	  val ty = TyComb.apply(comb, List.tabulate(ary, mkeqty))
      in
	   Ty.admitsEq(ty, true)
      end

  end (* functor TyFcnFn *)
