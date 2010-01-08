(* static/tycomb.sml *)

functor TyCombFn(structure Util : UTIL
		 structure Ty : TY
		   ) : TY_COMB =
  struct

    structure Ty = Ty

    datatype tycomb
      = QUOTE of Ty.ty
      | ARG of int
      | MKTUPLE of tycomb list
      | MKREL of tycomb list * tycomb list
      | MKCONS of tycomb list * Ty.tyname
      | MKNAMED of string * tycomb

    fun absBvars(bvars, ty) =
      let fun isQ(QUOTE _) = true
	    | isQ _ = false
	  fun abstract ty =
	    let val ty = Ty.deref ty
	    in
	      case ty
		of Ty.VAR alpha =>
		    let fun look([], _) = QUOTE ty
			  | look(tyvar::tyvars, i) =
			      if alpha = tyvar then ARG i else look(tyvars, i+1)
		    in
		      look(bvars, 0)
		    end
		 | Ty.TUPLE tys =>
		    let val cs = map abstract tys
		    in
		      if List.all isQ cs then QUOTE ty else MKTUPLE cs
		    end
		 | Ty.REL(tys1,tys2) =>
		    let val cs1 = map abstract tys1
			and cs2 = map abstract tys2
		    in
		      if List.all isQ cs1 andalso List.all isQ cs2 then QUOTE ty
		      else MKREL(cs1, cs2)
		    end
		 | Ty.CONS(tys,t) =>
		    let val cs = map abstract tys
		    in
		      if List.all isQ cs then QUOTE ty else MKCONS(cs, t)
		    end
		 | Ty.NAMED(id_str, ty) => MKNAMED(id_str, abstract ty)
	    end
      in
		abstract ty
      end

    fun absAll ty =
      let fun scan(ty, alphas) =
	    case Ty.deref ty
	      of Ty.VAR alpha => Util.maybeCons(alpha, alphas)
	       | Ty.TUPLE tys => List.foldl scan alphas tys
	       | Ty.REL(tys1,tys2) =>
		  List.foldl scan (List.foldl scan alphas tys1) tys2
	       | Ty.CONS(tys,_) => List.foldl scan alphas tys
	       | Ty.NAMED(id_str, ty) => scan (ty, alphas)
	  val bvars = scan(ty, [])
      in
	    (bvars, absBvars(bvars, ty))
      end

    fun absNone ty = QUOTE(Ty.deref ty)

    fun apply(comb, args) =
      let val args = Vector.fromList args
	  fun eval(QUOTE ty) = ty
	    | eval(ARG i) = Vector.sub(args, i)
	    | eval(MKTUPLE cs) = Ty.TUPLE(map eval cs)
	    | eval(MKREL(cs1,cs2)) = Ty.REL(map eval cs1, map eval cs2)
	    | eval(MKCONS(cs,t)) = Ty.CONS(map eval cs, t)
	    | eval(MKNAMED(id_str, ty)) = Ty.NAMED(id_str, eval ty)
      in
		eval comb
      end

  end (* functor TyCombFn *)
