(* static/tyscheme.sml *)

functor TySchemeFn(structure Util : UTIL
		   structure TyComb : TY_COMB
		     ) : TY_SCHEME =
  struct

    structure Ty = TyComb.Ty

    datatype alphascheme = AS of {eq: bool, name: string option}
    datatype tyscheme = SIGMA of alphascheme list * TyComb.tycomb

    fun genAll ty =
      let fun tyvarName(Ty.RIGID name) = SOME name
	    | tyvarName _ = NONE
	  fun tyvarEq(Ty.RIGID name) = String.sub(name,0) = #"'"
	    | tyvarEq(Ty.FREE{eq,...}) = eq
	  fun genTyvar tyvar = AS{eq=tyvarEq tyvar, name=tyvarName tyvar}
	  val (bvars,comb) = TyComb.absAll ty
      in
		SIGMA(map genTyvar bvars, comb)
      end

    fun genNone ty = SIGMA([], TyComb.absNone ty)

    fun instFree(SIGMA(bvars,comb)) =
      let fun tyvarFree(AS{eq,...}) = Ty.VAR(Ty.makeTyvar eq)
      in
		TyComb.apply(comb, map tyvarFree bvars)
      end

    (* This is only used on (a) type schemes constructed with genNone,
     * and (b) type schemes constructed from explicit types with rigid
     * type variables in specifications. In case (a), there are no
     * type variables to instantiate, and in case (b), we reuse the
     * rigid type variables instead of consing up new ones.
     *)
    fun instRigid(SIGMA(bvars,comb)) =
      let fun tyvarRigid(AS{name=SOME name,...}) = Ty.VAR(Ty.RIGID name)
	    | tyvarRigid _ = Util.bug "TyScheme.instRigid"
      in
		TyComb.apply(comb, map tyvarRigid bvars)
      end
      
  end (* functor TySchemeFn *)
