(* lexer/rml.lex -- ML-Lex lexical specification for RML *)

(* parameters for ML-Yacc *)
type arg = LexArg.lexarg
type pos = Cache.poz
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

fun mkPos (pos, lexarg) = 
	(pos, 
	 !(LexArg.currLine lexarg), 
	  (LexArg.fixColumn (lexarg,pos); !(LexArg.currColumn lexarg)))

fun eof(lexarg) =
  let val pos        = LexArg.readPos lexarg
	  val newPos     = mkPos(pos, lexarg)
  in
    if !(LexArg.eofFlag lexarg) 
    then 
		Tokens.EOF_HARD(newPos, newPos)
    else
      let val pos1 = !(LexArg.leftPos lexarg)
	  val _ = LexArg.eofFlag lexarg := true
      in
		if !(LexArg.comLev lexarg) > 0 then
		LexArg.error2 lexarg ("unterminated comment at EOF", pos1, newPos)
		else if !(LexArg.strList lexarg) <> [] then
		LexArg.error2 lexarg ("unterminated string at EOF", pos1, newPos)
		else ();
		Tokens.EOF_SOFT(newPos, newPos)
      end
  end
  
  
fun makeDECINT(pos1, str, lexarg) =
  let val pos2 = pos1 + String.size str - 1
  in
    Tokens.ICON(LexUtil.decint str, mkPos(pos1, lexarg), mkPos(pos2, lexarg))
  end

fun makeHEXINT(pos1, str, lexarg) =
  let val pos2 = pos1 + String.size str - 1
  in
    Tokens.ICON(LexUtil.hexint str, mkPos(pos1, lexarg), mkPos(pos2, lexarg))
  end

fun makeRCON(pos1, str, lexarg) =
  let val pos2 = pos1 + String.size str - 1
  in
    Tokens.RCON(LexUtil.rcon str, mkPos(pos1, lexarg), mkPos(pos2, lexarg))
  end

fun makeCCON(pos1, str, lexarg) =
  let val pos2 = pos1 + String.size str - 1
  in
    Tokens.CCON(LexUtil.ccon str, mkPos(pos1, lexarg), mkPos(pos2, lexarg))
  end

(* The regular expression for a string literal used to be:
 *	white=[\ \t\n\013\012];
 *	gap=\\{white}+\\;
 *	sitem={cdesc}|{gap};
 *	scon=\"{sitem}*\";
 * but due to error handling and position reporting reasons,
 * we now scan strings piecemeal using several lex start states.
 *)
fun addString(lexarg, yytext) =
  let val strList = LexArg.strList lexarg
  in
    strList := yytext :: !strList
  end

fun errBadStr(lexarg, pos, text) =
  let val msg = "illegal character in string " ^ MakeString.scvt text
  in
    LexArg.error2 lexarg (msg, mkPos(pos, lexarg), mkPos(pos, lexarg))
  end

fun makeSCON(lexarg, pos2) =
  let val strList = LexArg.strList lexarg
      val str = String.concat(List.rev("\"" :: !strList))
      val _ = strList := []
  in
    Tokens.SCON(LexUtil.scon str, !(LexArg.leftPos lexarg), mkPos(pos2, lexarg))
  end

fun makeTYVAR(pos1, str, lexarg) =
  let val len_1 = String.size str - 1
  in
    Tokens.TYVAR(substring(str, 1, len_1), mkPos(pos1 + 1, lexarg), mkPos(pos1 + len_1, lexarg))
  end

val kwds = [
	    ("and",	Tokens.AND),
	    ("as",	Tokens.AS),
	    ("axiom",	Tokens.AXIOM),
	    ("datatype",Tokens.DATATYPE),
	    ("default",	Tokens.DEFAULT),
	    ("end",	Tokens.END),
	    ("eqtype",	Tokens.EQTYPE),
	    ("fail",	Tokens.FAIL),
	    ("let",	Tokens.LET),
	    ("module",	Tokens.MODULE),
	    ("not",	Tokens.NOT),
	    ("of",	Tokens.OF),
	    ("relation",Tokens.RELATION),
	    ("rule",	Tokens.RULE),
	    ("type",	Tokens.TYPE),
	    ("val",	Tokens.VAL),
	    ("with",	Tokens.WITH),
	    ("withtype",Tokens.WITHTYPE)
	    ]

fun makeIDENT(pos1, str, lexarg) =
  let val pos2 = pos1 + String.size str - 1
      fun loop [] = Tokens.IDENT(str, mkPos(pos1, lexarg), mkPos(pos2, lexarg))
	| loop((str',mktok)::rest) =
	    if str=str' then mktok(mkPos(pos1, lexarg),mkPos(pos2, lexarg)) else loop rest
  in
	if (str = "end")    
	then LexArg.currVisibility(lexarg) := Cache.PROTECTED
	else ();	
    loop kwds
  end

fun inc(ri) = ri := !ri + 1
fun dec(ri) = let val j = !ri - 1 in ri := j; j end

%%

%header (functor RMLLexFn(structure Tokens : RML_TOKENS
			  structure Cache: CACHE
			  structure LexArg : LEXARG where type poz = Cache.poz
			  structure MakeString : MAKESTRING
			  structure LexUtil : LEXUTIL
			  sharing type Cache.visibility = LexArg.visibility
			  ) : ARG_LEXER);
			  
%S STRING GAP COMMENT COMMENTLINE;
%arg (lexarg);
%full

white=[\ \013\012];
eol="\010";
tab=\t;

ddigit=[0-9];
decint=[-~]?{ddigit}+;
xdigit=[0-9a-fA-F];
hexint=[-~]?"0x"{xdigit}+;

fraction="."{ddigit}+;
exponent=[eE]{decint};
rcon={decint}({fraction}{exponent}?|{exponent});

echar=[\\"nrtfabv];
cntrl=[?-_];
escseq={ddigit}{3}|"^"{cntrl}|{echar};
pchar=[\ -!#-[^-~\128-\255]|"]";
cdesc={pchar}|\\{escseq};
ccon="#\""{cdesc}\";

alpha=[A-Za-z];
alnum={alpha}|[_'0-9];
id={alpha}{alnum}*;

tyvar="'"+{alpha}{alnum}*;

%%

<INITIAL>{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos, lexarg));
	 continue());
<INITIAL>{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos, lexarg));
	 continue());
<INITIAL>{white}	=>
	(continue());
<INITIAL>\"		=>
	(LexArg.strList lexarg := [yytext];
	 LexArg.fixColumn(lexarg, yypos);
	 LexArg.leftPos lexarg := mkPos(yypos, lexarg);
	 YYBEGIN STRING;
	 continue());
<INITIAL>"&"		=>
	(Tokens.AMPERSAND(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"(*"|"/*"		=>
	(LexArg.comLev lexarg := 1;
	 LexArg.leftPos lexarg := mkPos(yypos, lexarg);
	 YYBEGIN COMMENT;
	 continue());
<INITIAL>"//"		=>
	(LexArg.comLev lexarg := 1;
	 LexArg.leftPos lexarg := mkPos(yypos, lexarg);
	 YYBEGIN COMMENTLINE;
	 continue());
<INITIAL>"("		=>
	(Tokens.LPAREN(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>")"		=>
	(Tokens.RPAREN(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"*)"|"*/"		=>
	(LexArg.error2 lexarg ("unmatched close comment",
	 mkPos(yypos, lexarg),
	 mkPos(yypos+1, lexarg)); continue());
<INITIAL>"*"		=>
	(Tokens.STAR(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>","		=>
	(Tokens.COMMA(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"-""-"+	=>
	(Tokens.DASHES(mkPos(yypos, lexarg), mkPos(yypos + String.size yytext - 1, lexarg)));
<INITIAL>"."		=>
	(Tokens.DOT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"::"		=>
	(Tokens.COLONCOLON(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>":"		=>
	(Tokens.COLON(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"="		=>
	(Tokens.EQ(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"=>"		=>
	(Tokens.FATARROW(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"["		=>
	(Tokens.LBRACK(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"]"		=>
	(Tokens.RBRACK(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"_"		=>
	(Tokens.WILD(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"|"		=>
	(Tokens.BAR(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>{decint}	=>
	(makeDECINT(yypos, yytext, lexarg));
<INITIAL>{hexint}	=>
	(makeHEXINT(yypos, yytext, lexarg));
<INITIAL>{rcon}		=>
	(makeRCON(yypos, yytext, lexarg));
<INITIAL>{ccon}		=>
	(makeCCON(yypos, yytext, lexarg));
<INITIAL>{id}		=>
	(makeIDENT(yypos, yytext, lexarg));
<INITIAL>{tyvar}	=>
	(makeTYVAR(yypos, yytext, lexarg));
		
<INITIAL>"+"	=> 
	(Tokens.ADD_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"-"	=>
	(Tokens.SUB_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"~"	=>
	(Tokens.NEG_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));	
<INITIAL>"/"	=>
	(Tokens.DIV_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"%"	=>
	(Tokens.MOD_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"=="	=>
	(Tokens.EQEQ_INT(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>">="	=>
	(Tokens.GE_INT(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>">"	=>
	(Tokens.GT_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"<="	=>
	(Tokens.LE_INT(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"<"	=>
	(Tokens.LT_INT(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"!="	=>
	(Tokens.NE_INT(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"<>"	=>
	(Tokens.NE_INT(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));

<INITIAL>"+."	=>
	(Tokens.ADD_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"-."	=>
	(Tokens.SUB_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"~."	=>
	(Tokens.NEG_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));	
<INITIAL>"*."	=>
	(Tokens.MUL_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"/."	=>
	(Tokens.DIV_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg))); 
<INITIAL>"%."	=>
	(Tokens.MOD_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"^."	=>
	(Tokens.POW_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));		
<INITIAL>"==."	=>
	(Tokens.EQ_REAL(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));
<INITIAL>">=."	=>
	(Tokens.GE_REAL(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));
<INITIAL>">."	=>
	(Tokens.GT_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"<=."	=>
	(Tokens.LE_REAL(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));
<INITIAL>"<."	=>
	(Tokens.LT_REAL(mkPos(yypos, lexarg), mkPos(yypos+1, lexarg)));
<INITIAL>"!=."	=>
	(Tokens.NE_REAL(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));
<INITIAL>"<>."	=>
	(Tokens.NE_REAL(mkPos(yypos, lexarg), mkPos(yypos+2, lexarg)));

<INITIAL>"+&"	=>
	(Tokens.ADD_STRING(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
<INITIAL>"==&"	=>
	(Tokens.EQEQ_STRING(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
	
<INITIAL>"@"	=>
	(Tokens.ADD_LIST(mkPos(yypos, lexarg), mkPos(yypos, lexarg)));
		
<INITIAL>.		=>
	(let val msg = "illegal character " ^ MakeString.scvt yytext
	 in
	   LexArg.error2 lexarg (msg,mkPos(yypos, lexarg),mkPos(yypos, lexarg));
	   continue()
	 end);

<STRING>\"		=>
	(YYBEGIN INITIAL; makeSCON(lexarg, yypos));
<STRING>\\{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos+1, lexarg));
	 YYBEGIN GAP; continue());
<STRING>\\{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos+1, lexarg));
	 YYBEGIN GAP; continue());
<STRING>\\{white}	=>
	(YYBEGIN GAP; continue());
<STRING>{cdesc}		=>
	(addString(lexarg, yytext); continue());
<STRING>{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos, lexarg));
	 addString(lexarg, yytext); continue());
<STRING>{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos, lexarg));
	 addString(lexarg, yytext); continue());
<STRING>.		=>
	(errBadStr(lexarg, yypos, yytext); continue());

<GAP>\\			=>
	(YYBEGIN STRING; continue());
<GAP>{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos, lexarg));
	 continue());
<GAP>{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos, lexarg));
	 continue());
<GAP>{white}		=>
	(continue());
<GAP>.			=>
	(let val msg = "illegal character in string gap " ^ MakeString.scvt yytext
	 in
	   LexArg.error2 lexarg (msg,mkPos(yypos, lexarg),mkPos(yypos, lexarg));
	   continue()
	 end);

<COMMENT>"(*"|"/*"		=>
	(inc(LexArg.comLev lexarg); continue());
<COMMENT>"*)"|"*/"		=>
	(if dec(LexArg.comLev lexarg) > 0 then () else YYBEGIN INITIAL; continue());
<COMMENT>{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos, lexarg));
	 continue());
<COMMENT>{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos, lexarg));
	 continue());
<COMMENT>.		=>
	(continue());

<COMMENTLINE>{eol}		=>
	(LexArg.newLine(lexarg, mkPos(yypos, lexarg));
	 if dec(LexArg.comLev lexarg) > 0 then () else YYBEGIN INITIAL; continue());
<COMMENTLINE>{tab}		=>
	(LexArg.newTab(lexarg, yygone, mkPos(yypos, lexarg));
	 continue());
<COMMENTLINE>.		=>
	(continue());
