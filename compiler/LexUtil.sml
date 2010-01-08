(* lexer/lexutil.sml *)

structure LexUtil : LEXUTIL =
  struct

    fun mkint(s, indx) = (* [-~]?{digit}+ *)
      let val size = String.size s
	  val (isneg,indx) = case String.sub(s, indx)
			       of #"-"	=> (true,indx+1)
				| #"~"	=> (true,indx+1)
				| _	=> (false,indx)
	  fun scan(i, indx) =
	    if indx < size then
	      let val c = Char.ord(String.sub(s, indx)) - Char.ord #"0"
	      in
		scan(i * 10 - c, indx + 1)
	      end
	    else if isneg then i else ~i
      in
		scan(0, indx)
      end

    fun decint s = mkint(s, 0)

    fun hexint s =	(* [-~]?"0x"{xdigit}+ *)
      let val size = String.size s
	  val indx = 0
	  val (isneg,indx) = case String.sub(s, indx)
			       of #"-"	=> (true,indx+1)
				| #"~"	=> (true,indx+1)
				| _	=> (false,indx)
	  val indx = indx+2	(* skip "0x" *)
	  fun digit c =
	    if c >= #"a" then Char.ord c - (Char.ord #"a" - 10)
	    else if c >= #"A" then Char.ord c - (Char.ord #"A" - 10)
	    else Char.ord c - Char.ord #"0"
	  fun scan(i, indx) =
	    if indx < size then
	      let val c = digit(String.sub(s, indx))
	      in
		scan(i * 16 - c, indx + 1)
	      end
	    else if isneg then i else ~i
      in
		scan(0, indx)
      end

    fun icon s =
      let val size = String.size s
	  fun hex_dig indx =
	    if indx >= size then NONE
	    else
	      let val c = String.sub(s, indx)
	      in
		if (c >= #"0" andalso c <= #"9") orelse
		   (c >= #"A" andalso c <= #"F") orelse
		   (c >= #"a" andalso c <= #"f")
		  then SOME(hexint s)
		else NONE
	      end
	  fun hex_pfx indx =
	    if indx >= size then SOME(0)
	    else
	      let val c = String.sub(s, indx)
	      in
		if c = #"x" then hex_dig(indx+1)
		else if c >= #"0" andalso c <= #"9" then SOME(decint s)
		else NONE
	      end
	  fun after_sign indx =
	    if indx >= size then NONE
	    else
	      let val c = String.sub(s, indx)
	      in
		if c = #"0" then hex_pfx(indx+1)
		else if c >= #"1" andalso c <= #"9" then SOME(decint s)
		else NONE
	      end
	  fun sign indx =
	    if indx >= size then NONE
	    else
	      case String.sub(s, indx)
		of #"-"	=> after_sign(indx+1)
		 | #"~"	=> after_sign(indx+1)
		 | _	=> after_sign indx
      in
		sign 0
      end

    fun mkreal(s, indx) =
      let val size = String.size s
	  val (sign,indx) = case String.sub(s,indx)
			      of #"-"	=> (~1.0,indx+1)
			       | #"~"	=> (~1.0,indx+1)
			       | _	=> (1.0,indx)
	  fun scale(f, _, 0) = f*sign
	    | scale(f, x, e) = scale(f*x, x, e-1)
	  fun exp(f, e, indx) =
	    let val e = mkint(s, indx)-e
	    in
	      scale(f, if e < 0 then 0.10 else 10.0, abs e)
	    end
	  fun frac(f, e, indx) =
	    if indx < size then
	      let val c = String.sub(s, indx)
	      in
		if c >= #"0" andalso c <= #"9" then
		  frac(f*10.0 + real(Char.ord c - Char.ord #"0"), e+1, indx+1)
		else if c = #"e" orelse c = #"E" then exp(f, e, indx+1)
		else scale(f, 0.10, e)
	      end
	    else scale(f, 0.10, e)
	  fun start(f, indx) =
	    if indx < size then
	      let val c = String.sub(s, indx)
	      in
		if c >= #"0" andalso c <= #"9" then
		  start(f*10.0 + real(Char.ord c - Char.ord #"0"), indx+1)
		else if c = #"." then frac(f, 0, indx+1)
		else if c = #"e" orelse c = #"E" then exp(f, 0, indx+1)
		else f*sign
	      end
	    else f*sign
      in
		start(0.0, indx)
      end

    fun rcon s = mkreal(s, 0)

    fun escseq(s, i) =
      let fun echar #"n" = #"\n"
	    | echar #"r" = #"\r"
	    | echar #"t" = #"\t"
	    | echar #"f" = #"\f"
	    | echar #"a" = #"\a"
	    | echar #"b" = #"\b"
	    | echar #"v" = #"\v"
	    | echar c = c	(* #"\\" and #"\"" *)
	  fun cntrl c = Char.chr((Char.ord c - 64) mod 128)
	  val c = String.sub(s,i)
      in
	if c >= #"0" andalso c <= #"9" then
	  let val d1 = Char.ord c		   - Char.ord #"0"
	      and d2 = Char.ord(String.sub(s,i+1)) - Char.ord #"0"
	      and d3 = Char.ord(String.sub(s,i+2)) - Char.ord #"0"
	  in
	    (Char.chr(d1 * 100 + d2 * 10 + d3), i+3)
	  end
	else if c = #"^" then (cntrl(String.sub(s,i+1)), i+2)
	else (echar c, i+1)
      end

    fun ccon s =	(* #\"{cdesc}\" -> char *)
      case String.sub(s,2)
	of #"\\" => let val (c,_) = escseq(s,3) in c end
	 | c => c

    fun scon s =	(* \"{cdesc}*\" -> string *)
      let fun sitem(i, rev_cs) =
	    case String.sub(s,i)
	      of #"\""	=> String.implode(List.rev rev_cs)
	       | #"\\"	=>
		  let val (c,i) = escseq(s, i+1)
		  in
		    sitem(i, c::rev_cs)
		  end
	       | c	=> sitem(i+1, c::rev_cs)
      in
		sitem(1, [])
      end

  end (* structure LexUtil *)
