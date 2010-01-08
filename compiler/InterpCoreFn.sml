(* interp/interpcore.sml *)

functor InterpCoreFn(structure MakeString : MAKESTRING
		     structure LexUtil : LEXUTIL
		     structure Util : UTIL
		     structure Absyn : ABSYN (* where type IdentDict.Key.ord_key = Absyn.ident *)
		     structure IntDict : ORD_DICT where type Key.ord_key = int
		     sharing type Absyn.IdentDict.Key.ord_key = Absyn.ident
		     ) : INTERP_CORE =
  struct

    structure Absyn = Absyn 
    (*
    structure IdentDict = Absyn.IdentDict 
    *)

    fun bug s = Util.bug("InterpCore: "^s)

    (*
     * Simple Objects
     *)
    type loc = int

    datatype code0	= C0_CLOCK
			| C0_LVAR_NEW
			| C0_TICK
    datatype code1	= C1_BOOL_NOT
			| C1_CHAR_INT
			| C1_INT_ABS | C1_INT_CHAR | C1_INT_NEG | C1_INT_REAL
			| C1_INT_STRING
			| C1_LIST_LENGTH | C1_LIST_REVERSE | C1_LIST_STRING
			| C1_LIST_VEC
			| C1_LVAR_GET
			| C1_PRINT
			| C1_REAL_ABS | C1_REAL_ATAN | C1_REAL_COS | C1_REAL_EXP
			| C1_REAL_FLOOR | C1_REAL_INT | C1_REAL_LN | C1_REAL_NEG
			| C1_REAL_SIN | C1_REAL_SQRT | C1_REAL_STRING
			| C1_STRING_INT | C1_STRING_LENGTH | C1_STRING_LIST
			| C1_VEC_LEN | C1_VEC_LIST
    datatype code2	= C2_BOOL_AND | C2_BOOL_OR
			| C2_INT_ADD | C2_INT_DIV | C2_INT_EQ
			| C2_INT_GE | C2_INT_GT | C2_INT_LE | C2_INT_LT
			| C2_INT_MAX | C2_INT_MIN | C2_INT_MOD | C2_INT_MUL
			| C2_INT_NE | C2_INT_SUB
			| C2_LIST_APPEND | C2_LIST_DELETE | C2_LIST_MEMBER
			| C2_LIST_NTH
			| C2_LVAR_SET
			| C2_REAL_ADD | C2_REAL_DIV | C2_REAL_EQ
			| C2_REAL_GE | C2_REAL_GT | C2_REAL_LE | C2_REAL_LT
			| C2_REAL_MAX | C2_REAL_MIN | C2_REAL_MOD | C2_REAL_MUL
			| C2_REAL_NE | C2_REAL_POW | C2_REAL_SUB
			| C2_STRING_APPEND | C2_STRING_NTH
			| C2_VEC_NTH
    datatype prim	= P0 of code0 | P1 of code1 | P2 of code2

    (*
     * Compound Objects
     *)
    datatype value	= LIT of Absyn.lit
			| TUPLE of value list
			| STRUCT of string * value list
			| LOC of loc
			| CLOSURE of {clause: Absyn.clause,
				      ME: menv,
				      VE: venv,
				      VE_rec: venv}
			| PRIM of prim
    withtype venv	= value Absyn.IdentDict.dict
    and menv		= value Absyn.IdentDict.dict Absyn.IdentDict.dict

    datatype state	= STATE of value option IntDict.dict * int
    datatype marker	= MARKER of state

    (*
     * States
     *)
    val state_init = STATE(IntDict.empty,0)
    fun state_new(STATE(map,next)) =
      (next, STATE(IntDict.insert(map,next,NONE), next+1))
    fun state_get(STATE(map,_), loc) =
      case IntDict.find(map, loc)
	of SOME x => x
	 | NONE => bug("state_get: location "^MakeString.icvt loc^" is unbound")
    fun state_set(STATE(map,next), loc, v) =
      STATE(IntDict.insert(map,loc,SOME v), next)

    (*
     * States and Markers
     *)
    fun marker s = MARKER s
    fun restore(MARKER m, s) = m

    (*
     * Equality
     *)
    fun equal(LIT lit1, LIT lit2) = Absyn.litEqual(lit1,lit2)
      | equal(TUPLE v1s, TUPLE v2s) = ListPair.all equal (v1s,v2s)
      | equal(STRUCT(con1,v1s), STRUCT(con2,v2s)) =
	  con1=con2 andalso ListPair.all equal (v1s,v2s)
      | equal(LOC loc1, LOC loc2) = loc1=loc2
      | equal(_, _) = false

    (*
     * Implementation of Primitive Procedures
     *)
    datatype result	= SUCCESS of value list * state
			| FAILURE of state

    exception FAIL of state
    fun NYI(msg, s) = (Util.warn("N.Y.I.: "^msg); raise(FAIL s))

    fun argBool(STRUCT("true",[])) = true
      | argBool(STRUCT("false",[])) = false
      | argBool _ = bug "APPLY: expected BOOL"

    fun argChar(LIT(Absyn.CCONlit(c, _))) = c
      | argChar _ = bug "APPLY: expected CHAR"

    fun argInt(LIT(Absyn.ICONlit(i, _))) = i
      | argInt _ = bug "APPLY: expected INT"

    datatype list_node = NIL | CONS of value * value
    fun argList(STRUCT("nil",[])) = NIL
      | argList(STRUCT("cons",[x,xs])) = CONS(x,xs)
      | argList _ = bug "APPLY: expected NIL or CONS"

    fun argLoc(LOC l) = l
      | argLoc _ = bug "APPLY: expected LVAR"

    fun argReal(LIT(Absyn.RCONlit(r, _))) = r
      | argReal _ = bug "APPLY: expected REAL"

    fun argStr(LIT(Absyn.SCONlit(s, _))) = s
      | argStr _ = bug "APPLY: expected STRING"

    fun argVec(TUPLE xs) = xs
      | argVec _ = bug "APPLY: expected VECTOR"

    val theTRUE = [STRUCT("true",[])]
    val theFALSE = [STRUCT("false",[])]
    fun theBool true = theTRUE
      | theBool false = theFALSE
    val theNONE = [STRUCT("NONE",[])]
    fun theSOME x = [STRUCT("SOME",[x])]
    fun theOption(NONE) = theNONE
      | theOption(SOME x) = theSOME x
    fun theChar c = [LIT(Absyn.CCONlit(c, Absyn.dummyInfo))]
    fun theInt i = [LIT(Absyn.ICONlit (i, Absyn.dummyInfo))]
    fun theLoc l = [LOC l]
    fun theReal r = [LIT(Absyn.RCONlit(r, Absyn.dummyInfo))]
    fun theString s = [LIT(Absyn.SCONlit(s, Absyn.dummyInfo))]
    fun theVec xs = [TUPLE xs]

    val rml_nil = STRUCT("nil",[])
    fun rml_cons(x,xs) = STRUCT("cons",[x,xs])

    fun list2value f =
      let fun cnv([]) = rml_nil
	    | cnv(x::xs) = rml_cons(f x, cnv xs)
      in
		cnv
      end

    fun rml_list_append(xs, ys, s) =
      let fun tackonys xs =
	    case argList xs
	      of NIL => ys
	       | CONS(x,xs) => rml_cons(x, tackonys xs)
      in
		SUCCESS([tackonys xs], s)
      end

    fun rml_list_delete(xs, i, s) =
      let fun delete(xs, i) =
	    case argList xs
	      of CONS(x,xs) => if i = 0 then xs else rml_cons(x, delete(xs, i-1))
	       | NIL => raise(FAIL s)
      in
		SUCCESS([delete(xs, i)], s)
      end

    fun rml_list_length(xs, s) =
      let fun loop(xs, len) =
	    case argList xs
	      of NIL => SUCCESS(theInt len, s)
	       | CONS(_,xs) => loop(xs, len+1)
      in
		loop(xs, 0)
      end

    fun rml_list_member(x, ys, s) =
      let fun loop ys =
	    case argList ys
	      of NIL => SUCCESS(theFALSE, s)
	       | CONS(y,ys) => if equal(x,y) then SUCCESS(theTRUE, s) else loop ys
      in
		loop ys
      end

    fun rml_list_nth(xs, i, s) =
      let fun loop(xs, i) =
	    case argList xs
	      of CONS(x,xs) => if i = 0 then SUCCESS([x], s) else loop(xs, i-1)
	       | NIL => FAILURE s
      in
		loop(xs, i)
      end

    fun rml_list_reverse(xs, s) =
      let fun revonto(xs, ys) =
	    case argList xs
	      of NIL => SUCCESS([ys], s)
	       | CONS(x,xs) => revonto(xs, rml_cons(x,ys))
      in
		revonto(xs, rml_nil)
      end

    fun rml_list_string(xs, s) =
      let fun loop(xs, rev_lst) =
	    case argList xs
	      of NIL => SUCCESS(theString(String.implode(rev rev_lst)), s)
	       | CONS(x,xs) => loop(xs, (argChar x)::rev_lst)
      in
		loop(xs, [])
      end

    fun rml_list_vec(xs, s) =
      let fun loop(xs, rev_lst) =
	    case argList xs
	      of NIL => SUCCESS(theVec(rev rev_lst), s)
	       | CONS(x,xs) => loop(xs, x::rev_lst)
      in
		loop(xs, [])
      end

    fun rml_real_mod(x, y, s) =
      (* core_fmod(x,y) computes fmod(x,y) when x >= 0.0 and y > 0.0 *)
      let fun core_fmod(x, y) =
	    let fun logb x = #exp(Real.toManExp x)
		fun scalb(y, n) = Real.fromManExp{man=y, exp=n}
		val yexp = logb y
		fun reduce x =
		  if x < y then x
		  else
		    let fun find n =
			  let val y_times_2_raised_n = scalb(y, n)
			  in
			    if y_times_2_raised_n > x then find(n - 1)
			    else reduce(x - y_times_2_raised_n)
			  end
		    in
		      find(logb x - yexp)
		    end
	    in
	      reduce x
	    end
      in
	if Real.==(y, 0.0) then FAILURE s
	else
	  let val y = if y < 0.0 then ~y else y
	      val r = if x < 0.0 then ~(core_fmod(~x, y)) else core_fmod(x, y)
	  in
	    SUCCESS(theReal r, s)
	  end
      end

    fun rml_string_list(str, s) =
      SUCCESS(
		[list2value 
			(fn c => 
				LIT(Absyn.CCONlit(c, Absyn.dummyInfo))) (String.explode str)], s)

    fun rml_string_nth(str, i, s) =
      if i < 0 orelse i >= String.size str then FAILURE s
      else SUCCESS(theChar(String.sub(str,i)), s)

    fun rml_vec_list(xs, s) =
      SUCCESS([list2value (fn x => x) xs], s)

    fun rml_vec_nth(xs, i, s) =
      if i < 0 orelse i >= length xs then FAILURE s
      else SUCCESS([List.nth(xs, i)], s)

    fun app_c0(c0, s) =
      case c0
	of C0_CLOCK => SUCCESS(theReal(real(Util.tick())), s)
	 | C0_LVAR_NEW =>
	    let val (loc,s') = state_new s
	    in
	      SUCCESS(theLoc loc, s')
	    end
	 | C0_TICK => SUCCESS(theInt(Util.tick()), s)

    fun app_c1(c1, x, s) =
      case c1
	of C1_BOOL_NOT => SUCCESS(theBool(not(argBool x)), s)
	 | C1_CHAR_INT => SUCCESS(theInt(Char.ord(argChar x)), s)
	 | C1_INT_ABS => SUCCESS(theInt(abs(argInt x)), s)
	 | C1_INT_CHAR =>
	    let val x = argInt x
	    in
	      if x < 0 orelse x > 255 then FAILURE s
	      else SUCCESS(theChar(Char.chr x), s)
	    end
	 | C1_INT_NEG => SUCCESS(theInt(~(argInt x)), s)
	 | C1_INT_REAL => SUCCESS(theReal(real(argInt x)), s)
	 | C1_INT_STRING => SUCCESS(theString(MakeString.icvt(argInt x)), s)
	 | C1_LIST_LENGTH => rml_list_length(x, s)
	 | C1_LIST_REVERSE => rml_list_reverse(x, s)
	 | C1_LIST_STRING => rml_list_string(x, s)
	 | C1_LIST_VEC => rml_list_vec(x, s)
	 | C1_LVAR_GET => SUCCESS(theOption(state_get(s, argLoc x)), s)
	 | C1_PRINT => (print(argStr x); SUCCESS([],s))
	 | C1_REAL_ABS => SUCCESS(theReal(abs(argReal x)), s)
	 | C1_REAL_ATAN => SUCCESS(theReal(Math.atan(argReal x)), s)
	 | C1_REAL_COS => SUCCESS(theReal(Math.cos(argReal x)), s)
	 | C1_REAL_EXP => SUCCESS(theReal(Math.exp(argReal x)), s)
	 | C1_REAL_FLOOR => SUCCESS(theReal(real(floor(argReal x))), s)
	 | C1_REAL_INT => SUCCESS(theInt(floor(argReal x)), s)
	 | C1_REAL_LN =>
	    let val x = argReal x
	    in
	      if x <= 0.0 then FAILURE s else SUCCESS(theReal(Math.ln x), s)
	    end
	 | C1_REAL_NEG => SUCCESS(theReal(~(argReal x)), s)
	 | C1_REAL_SIN => SUCCESS(theReal(Math.sin(argReal x)), s)
	 | C1_REAL_SQRT =>
	    let val x = argReal x
	    in
	      if x < 0.0 then FAILURE s else SUCCESS(theReal(Math.sqrt x), s)
	    end
	 | C1_REAL_STRING => SUCCESS(theString(MakeString.rcvt(argReal x)), s)
	 | C1_STRING_INT =>
	    (case LexUtil.icon(argStr x)
	       of SOME i => SUCCESS(theInt i, s)
		| NONE => FAILURE s)
	 | C1_STRING_LENGTH => SUCCESS(theInt(size(argStr x)), s)
	 | C1_STRING_LIST => rml_string_list(argStr x, s)
	 | C1_VEC_LEN => SUCCESS(theInt(length(argVec x)), s)
	 | C1_VEC_LIST => rml_vec_list(argVec x, s)

    fun app_c2(c2, x, y, s) =
      case c2
	of C2_BOOL_AND => SUCCESS(theBool((argBool x) andalso (argBool y)), s)
	 | C2_BOOL_OR => SUCCESS(theBool((argBool x) orelse (argBool y)), s)
	 | C2_INT_ADD => SUCCESS(theInt((argInt x) + (argInt y)), s)
	 | C2_INT_DIV =>
	    let val x = argInt x and y = argInt y
	    in
	      if y = 0 then FAILURE s else SUCCESS(theInt(x div y), s)
	    end
	 | C2_INT_EQ => SUCCESS(theBool((argInt x) = (argInt y)), s)
	 | C2_INT_GE => SUCCESS(theBool((argInt x) >= (argInt y)), s)
	 | C2_INT_GT => SUCCESS(theBool((argInt x) > (argInt y)), s)
	 | C2_INT_LE => SUCCESS(theBool((argInt x) <= (argInt y)), s)
	 | C2_INT_LT => SUCCESS(theBool((argInt x) < (argInt y)), s)
	 | C2_INT_MAX =>
	    let val x = argInt x and y = argInt y
	    in
	      SUCCESS(theInt(if x >= y then x else y), s)
	    end
	 | C2_INT_MIN =>
	    let val x = argInt x and y = argInt y
	    in
	      SUCCESS(theInt(if x <= y then x else y), s)
	    end
	 | C2_INT_MOD =>
	    let val x = argInt x and y = argInt y
	    in
	      if y = 0 then FAILURE s else SUCCESS(theInt(x mod y), s)
	    end
	 | C2_INT_MUL => SUCCESS(theInt((argInt x) * (argInt y)), s)
	 | C2_INT_NE => SUCCESS(theBool((argInt x) <> (argInt y)), s)
	 | C2_INT_SUB => SUCCESS(theInt((argInt x) - (argInt y)), s)
	 | C2_LIST_APPEND => rml_list_append(x, y, s)
	 | C2_LIST_DELETE => rml_list_delete(x, argInt y, s)
	 | C2_LIST_MEMBER => rml_list_member(x, y, s)
	 | C2_LIST_NTH => rml_list_nth(x, argInt y, s)
	 | C2_LVAR_SET =>
	    let val loc = argLoc x
	    in
	      case state_get(s, loc)
		of NONE => SUCCESS([], state_set(s, loc, y))
		 | SOME _ => FAILURE s
	    end
	 | C2_REAL_ADD => SUCCESS(theReal((argReal x) + (argReal y)), s)
	 | C2_REAL_DIV =>
	    let val x = argReal x and y = argReal y
	    in
	      if Real.==(y, 0.0) then FAILURE s else SUCCESS(theReal(x / y), s)
	    end
	 | C2_REAL_EQ => SUCCESS(theBool(Real.==(argReal x, argReal y)), s)
	 | C2_REAL_GE => SUCCESS(theBool((argReal x) >= (argReal y)), s)
	 | C2_REAL_GT => SUCCESS(theBool((argReal x) > (argReal y)), s)
	 | C2_REAL_LE => SUCCESS(theBool((argReal x) <= (argReal y)), s)
	 | C2_REAL_LT => SUCCESS(theBool((argReal x) < (argReal y)), s)
	 | C2_REAL_MAX =>
	    let val x = argReal x and y = argReal y
	    in
	      SUCCESS(theReal(if x >= y then x else y), s)
	    end
	 | C2_REAL_MIN =>
	    let val x = argReal x and y = argReal y
	    in
	      SUCCESS(theReal(if x <= y then x else y), s)
	    end
	 | C2_REAL_MOD => rml_real_mod(argReal x, argReal y, s)
	 | C2_REAL_MUL => SUCCESS(theReal((argReal x) * (argReal y)), s)
	 | C2_REAL_NE => SUCCESS(theBool(Real.!=(argReal x, argReal y)), s)
	 | C2_REAL_POW => NYI("real_pow", s)	(*XXX*)
	 | C2_REAL_SUB => SUCCESS(theReal((argReal x) - (argReal y)), s)
	 | C2_STRING_APPEND => SUCCESS(theString((argStr x)^(argStr y)), s)
	 | C2_STRING_NTH => rml_string_nth(argStr x, argInt y, s)
	 | C2_VEC_NTH => rml_vec_nth(argVec x, argInt y, s)

    fun APPLY(prim, v_star, s) =
      (case prim
	 of P0 c0	=>
	    (case v_star
	       of []	=> app_c0(c0,s)
		| _	=> bug "APPLY: expected zero arguments")
	  | P1 c1	=>
	    (case v_star
	       of [x]	=> app_c1(c1,x,s)
		| _	=> bug "APPLY: expected one argument")
	  | P2 c2	=>
	    (case v_star
	       of [x,y]	=> app_c2(c2,x,y,s)
		| _	=> bug "APPLY: expected two arguments")
      ) handle FAIL s => FAILURE s

    (*
     * Initial Dynamic Objects
     *)
    val s_init = state_init

    val VE_init =
      let fun bind(VE, var, value) =
	    let val var = Absyn.rmlIdent var
	    in
	      Absyn.IdentDict.insert(VE, var, value)
	    end
	  val VE = Absyn.IdentDict.empty
	  val VE = bind(VE, "bool_and", PRIM(P2 C2_BOOL_AND))
	  val VE = bind(VE, "bool_not", PRIM(P1 C1_BOOL_NOT))
	  val VE = bind(VE, "bool_or", PRIM(P2 C2_BOOL_OR))
	  val VE = bind(VE, "char_int", PRIM(P1 C1_CHAR_INT))
	  val VE = bind(VE, "int_abs", PRIM(P1 C1_INT_ABS))
	  val VE = bind(VE, "int_add", PRIM(P2 C2_INT_ADD))
	  val VE = bind(VE, "int_char", PRIM(P1 C1_INT_CHAR))
	  val VE = bind(VE, "int_div", PRIM(P2 C2_INT_DIV))
	  val VE = bind(VE, "int_eq", PRIM(P2 C2_INT_EQ))
	  val VE = bind(VE, "int_ge", PRIM(P2 C2_INT_GE))
	  val VE = bind(VE, "int_gt", PRIM(P2 C2_INT_GT))
	  val VE = bind(VE, "int_le", PRIM(P2 C2_INT_LE))
	  val VE = bind(VE, "int_lt", PRIM(P2 C2_INT_LT))
	  val VE = bind(VE, "int_max", PRIM(P2 C2_INT_MAX))
	  val VE = bind(VE, "int_min", PRIM(P2 C2_INT_MIN))
	  val VE = bind(VE, "int_mod", PRIM(P2 C2_INT_MOD))
	  val VE = bind(VE, "int_mul", PRIM(P2 C2_INT_MUL))
	  val VE = bind(VE, "int_ne", PRIM(P2 C2_INT_NE))
	  val VE = bind(VE, "int_neg", PRIM(P1 C1_INT_NEG))
	  val VE = bind(VE, "int_real", PRIM(P1 C1_INT_REAL))
	  val VE = bind(VE, "int_string", PRIM(P1 C1_INT_STRING))
	  val VE = bind(VE, "int_sub", PRIM(P2 C2_INT_SUB))
	  val VE = bind(VE, "list_append", PRIM(P2 C2_LIST_APPEND))
	  val VE = bind(VE, "list_delete", PRIM(P2 C2_LIST_DELETE))
	  val VE = bind(VE, "list_length", PRIM(P1 C1_LIST_LENGTH))
	  val VE = bind(VE, "list_member", PRIM(P2 C2_LIST_MEMBER))
	  val VE = bind(VE, "list_nth", PRIM(P2 C2_LIST_NTH))
	  val VE = bind(VE, "list_reverse", PRIM(P1 C1_LIST_REVERSE))
	  val VE = bind(VE, "list_string", PRIM(P1 C1_LIST_STRING))
	  val VE = bind(VE, "list_vector", PRIM(P1 C1_LIST_VEC))
	  val VE = bind(VE, "lvar_get", PRIM(P1 C1_LVAR_GET))
	  val VE = bind(VE, "lvar_new", PRIM(P0 C0_LVAR_NEW))
	  val VE = bind(VE, "lvar_set", PRIM(P2 C2_LVAR_SET))
	  val VE = bind(VE, "real_abs", PRIM(P1 C1_REAL_ABS))
	  val VE = bind(VE, "real_add", PRIM(P2 C2_REAL_ADD))
	  val VE = bind(VE, "real_atan", PRIM(P1 C1_REAL_ATAN))
	  val VE = bind(VE, "real_cos", PRIM(P1 C1_REAL_COS))
	  val VE = bind(VE, "real_div", PRIM(P2 C2_REAL_DIV))
	  val VE = bind(VE, "real_eq", PRIM(P2 C2_REAL_EQ))
	  val VE = bind(VE, "real_exp", PRIM(P1 C1_REAL_EXP))
	  val VE = bind(VE, "real_floor", PRIM(P1 C1_REAL_FLOOR))
	  val VE = bind(VE, "real_ge", PRIM(P2 C2_REAL_GE))
	  val VE = bind(VE, "real_gt", PRIM(P2 C2_REAL_GT))
	  val VE = bind(VE, "real_int", PRIM(P1 C1_REAL_INT))
	  val VE = bind(VE, "real_le", PRIM(P2 C2_REAL_LE))
	  val VE = bind(VE, "real_ln", PRIM(P1 C1_REAL_LN))
	  val VE = bind(VE, "real_lt", PRIM(P2 C2_REAL_LT))
	  val VE = bind(VE, "real_max", PRIM(P2 C2_REAL_MAX))
	  val VE = bind(VE, "real_min", PRIM(P2 C2_REAL_MIN))
	  val VE = bind(VE, "real_mod", PRIM(P2 C2_REAL_MOD))
	  val VE = bind(VE, "real_mul", PRIM(P2 C2_REAL_MUL))
	  val VE = bind(VE, "real_ne", PRIM(P2 C2_REAL_NE))
	  val VE = bind(VE, "real_neg", PRIM(P1 C1_REAL_NEG))
	  val VE = bind(VE, "real_pow", PRIM(P2 C2_REAL_POW))
	  val VE = bind(VE, "real_sin", PRIM(P1 C1_REAL_SIN))
	  val VE = bind(VE, "real_sqrt", PRIM(P1 C1_REAL_SQRT))
	  val VE = bind(VE, "real_string", PRIM(P1 C1_REAL_STRING))
	  val VE = bind(VE, "real_sub", PRIM(P2 C2_REAL_SUB))
	  val VE = bind(VE, "string_append", PRIM(P2 C2_STRING_APPEND))
	  val VE = bind(VE, "string_int", PRIM(P1 C1_STRING_INT))
	  val VE = bind(VE, "string_length", PRIM(P1 C1_STRING_LENGTH))
	  val VE = bind(VE, "string_list", PRIM(P1 C1_STRING_LIST))
	  val VE = bind(VE, "string_nth", PRIM(P2 C2_STRING_NTH))
	  val VE = bind(VE, "vector_length", PRIM(P1 C1_VEC_LEN))
	  val VE = bind(VE, "vector_list", PRIM(P1 C1_VEC_LIST))
	  val VE = bind(VE, "vector_nth", PRIM(P2 C2_VEC_NTH))
	  val VE = bind(VE, "clock", PRIM(P0 C0_CLOCK))
	  val VE = bind(VE, "print", PRIM(P1 C1_PRINT))
	  val VE = bind(VE, "tick", PRIM(P0 C0_TICK))
      in
		VE
      end

    val ME_init =
      Absyn.IdentDict.insert(Absyn.IdentDict.empty, Absyn.rmlIdent "RML", VE_init)

  end (* functor InterpCoreFn *)
