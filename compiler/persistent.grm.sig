signature PERSISTENT_TOKENS =
sig
type ('a,'b) token
type svalue
val START_MODULE:  'a * 'a -> (svalue,'a) token
val START_INTERFACE:  'a * 'a -> (svalue,'a) token
val ATTR:  'a * 'a -> (svalue,'a) token
val LV:  'a * 'a -> (svalue,'a) token
val START_SERIALIZATION_INFO:  'a * 'a -> (svalue,'a) token
val SERIALIZATION_INFO_END:  'a * 'a -> (svalue,'a) token
val SERIALIZATION_INFO:  'a * 'a -> (svalue,'a) token
val DUMMY_INTERFACE:  'a * 'a -> (svalue,'a) token
val DUMMY_SOURCE:  'a * 'a -> (svalue,'a) token
val DECLARATIONS:  'a * 'a -> (svalue,'a) token
val INTERFACE_END:  'a * 'a -> (svalue,'a) token
val MODULE:  'a * 'a -> (svalue,'a) token
val SOURCE:  'a * 'a -> (svalue,'a) token
val INTERFACE:  'a * 'a -> (svalue,'a) token
val RELdec:  'a * 'a -> (svalue,'a) token
val VALdec:  'a * 'a -> (svalue,'a) token
val DATAdec:  'a * 'a -> (svalue,'a) token
val TYPEdec:  'a * 'a -> (svalue,'a) token
val WITHdec:  'a * 'a -> (svalue,'a) token
val RELspec:  'a * 'a -> (svalue,'a) token
val VALspec:  'a * 'a -> (svalue,'a) token
val DATAspec:  'a * 'a -> (svalue,'a) token
val TYPEspec:  'a * 'a -> (svalue,'a) token
val ABSTYPEspec:  'a * 'a -> (svalue,'a) token
val REF:  'a * 'a -> (svalue,'a) token
val WITHspec:  'a * 'a -> (svalue,'a) token
val RELBIND:  'a * 'a -> (svalue,'a) token
val TYPBIND:  'a * 'a -> (svalue,'a) token
val DATBIND:  'a * 'a -> (svalue,'a) token
val CTORcb:  'a * 'a -> (svalue,'a) token
val CONcb:  'a * 'a -> (svalue,'a) token
val CLAUSE2:  'a * 'a -> (svalue,'a) token
val CLAUSE1:  'a * 'a -> (svalue,'a) token
val FAIL:  'a * 'a -> (svalue,'a) token
val RETURN:  'a * 'a -> (svalue,'a) token
val CONDgoal:  'a * 'a -> (svalue,'a) token
val ANDgoal:  'a * 'a -> (svalue,'a) token
val NOTgoal:  'a * 'a -> (svalue,'a) token
val LETgoal:  'a * 'a -> (svalue,'a) token
val EQUALgoal:  'a * 'a -> (svalue,'a) token
val CALLgoal:  'a * 'a -> (svalue,'a) token
val IDENTexp:  'a * 'a -> (svalue,'a) token
val STRUCTexp:  'a * 'a -> (svalue,'a) token
val VARexp:  'a * 'a -> (svalue,'a) token
val CONexp:  'a * 'a -> (svalue,'a) token
val LITexp:  'a * 'a -> (svalue,'a) token
val NAMEDpat:  'a * 'a -> (svalue,'a) token
val IDENTpat:  'a * 'a -> (svalue,'a) token
val BINDpat:  'a * 'a -> (svalue,'a) token
val STRUCTpat:  'a * 'a -> (svalue,'a) token
val CONpat:  'a * 'a -> (svalue,'a) token
val LITpat:  'a * 'a -> (svalue,'a) token
val WILDpat:  'a * 'a -> (svalue,'a) token
val NON:  'a * 'a -> (svalue,'a) token
val SOM:  'a * 'a -> (svalue,'a) token
val TUPLEty:  'a * 'a -> (svalue,'a) token
val NAMEDty:  'a * 'a -> (svalue,'a) token
val RELty:  'a * 'a -> (svalue,'a) token
val CONSty:  'a * 'a -> (svalue,'a) token
val VARty:  'a * 'a -> (svalue,'a) token
val LONGID:  'a * 'a -> (svalue,'a) token
val TYVAR: (string) *  'a * 'a -> (svalue,'a) token
val SCON: (string) *  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val RCON: (real) *  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val IDENT: (string) *  'a * 'a -> (svalue,'a) token
val ICON: (int) *  'a * 'a -> (svalue,'a) token
val EOF_SOFT:  'a * 'a -> (svalue,'a) token
val EOF_HARD:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val CCON: (char) *  'a * 'a -> (svalue,'a) token
end
signature PERSISTENT_LRVALS=
sig
structure Tokens : PERSISTENT_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
