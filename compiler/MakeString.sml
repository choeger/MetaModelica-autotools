(* util/makestring.sml *)

structure MakeString : MAKESTRING =
  struct

    (*XXX all of this assumes ASCII or ISO-8859-1 *)

    fun mkdig i = Char.chr(48 + i)

    (* formatting fixnums *)

    fun iexplode i =
      let fun uexplode(i, tail) = (* PRE: i >= 0 *)
	    if i < 10 then (mkdig i)::tail
	    else
	      let val j = Int.quot(i, 10)
	      in
		uexplode(j, (mkdig(i-10*j))::tail)
	      end
      in
	if i < 0 then
	  let val j = Int.quot(i, 10)
	      val dig = mkdig(~(i-10*j))
	  in
	    if j = 0 then #"-"::dig::[]
	    else #"-" :: uexplode(~j, dig::[])
	  end
	else uexplode(i, [])
      end

    fun icvt i = String.implode(iexplode i)

    (* formatting flonums *)

    fun rexplode r =
      let fun nonempty([], tail) = #"0"::tail
	    | nonempty(digits, tail) = digits@tail

	  fun zeroes(k, tail) =
	    if k > 0 then zeroes(k-1, #"0"::tail)
	    else tail

	  fun decimal(digit, digits, e, tail) =
	    let fun recur(0, digit, digits) = digit :: #"." :: nonempty(digits, tail)
		  | recur(e, digit, digits) =
		      let val tail = case digits
				       of [] => zeroes(e, #"." :: #"0" :: tail)
					| (digit::digits) => recur(e-1,digit,digits)
		      in
			digit::tail
		      end
	    in
	      recur(e, digit, digits)
	    end

	  fun format(digit, digits, e) =
	    if e < ~4 orelse e > 14 then
	      digit :: #"." :: nonempty(digits, #"e" :: iexplode e)
	    else if e < 0 then
	      #"0" :: #"." :: zeroes(~(e+1), digit::digits)
	    else
	      decimal(digit, digits, e, [])

	  fun cons0([]) = []
	    | cons0(tail) = #"0"::tail

	  fun nocarry(revdigs, tail, e) =
	    let fun loop([], []) = format(#"0", [], e)
		  | loop([], dig::tail) = format(dig, tail, e)
		  | loop(0::revdigs, tail) = loop(revdigs, cons0 tail)
		  | loop(dig::revdigs, tail) = loop(revdigs, (mkdig dig)::tail)
	    in
	      loop(revdigs, tail)
	    end

	  fun carry(revdigs, e) =
	    let fun loop([], tail) = format(#"1", tail, e+1)
		  | loop(9::revdigs, tail) = loop(revdigs, cons0 tail)
		  | loop(dig::revdigs,tail) = nocarry(revdigs,(mkdig(dig+1))::tail,e)
	    in
	      loop(revdigs, [])
	    end

	  fun fexplode(f,e) =
	    let fun loop(precision, f, revdigits) =
	      if precision <= 0 then
		if f >= 5.0 then carry(revdigits, e)
		else nocarry(revdigits, [], e)
	      else
		let val digit = floor f
		in
		  loop(precision-1, 10.0*(f - real digit), digit::revdigits)
		end
	    in
	      loop(15, f, [])
	    end

	  fun normalize r =	(* PRE: r > 0 *)
	    if r < 1.0 then
	      let fun ge1(f,e) = if f < 1.0 then ge1(10.0*f,e-1) else fexplode(f,e)
	      in
		ge1(10.0*r,~1)
	      end
	    else
	      let fun lt10(f,e) = if f >= 10.0 then lt10(0.1*f,e+1) else fexplode(f,e)
	      in
		lt10(r,0)
	      end
      in
	if r < 0.0 then #"-" :: normalize(~r)
	else if r > 0.0 then normalize r
	else [#"0", #".", #"0"]
      end

    fun rcvt r = String.implode(rexplode r)

    (* formatting strings *)

    fun cexplode(#"\\", tail) = #"\\" :: #"\\" :: tail
      | cexplode(#"\"", tail) = #"\\" :: #"\"" :: tail
      | cexplode(#"\n", tail) = #"\\" :: #"n"  :: tail
      | cexplode(c,     tail) =          c     :: tail

    fun sexplode s =
      let fun loop(i, tail) =
	    if i < 0 then #"\"" :: tail
	    else loop(i-1, cexplode(String.sub(s,i), tail))
      in
	loop((size s)-1, [#"\""])
      end

    fun scvt s = String.implode(sexplode s)

    (* formatting characters *)

    fun ccvt c = String.implode(#"#" :: #"\"" :: cexplode(c,[#"\""]))

  end (* structure MakeString *)
