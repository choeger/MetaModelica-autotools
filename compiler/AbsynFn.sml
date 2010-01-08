(* absyn/absyn.sml *)

functor AbsynFn(structure MakeString : MAKESTRING
		structure Source : SOURCE
		  ) : ABSYN =
  struct

    structure Source	= Source

    (* --- start RML AST --- *)
    (* pos start, pos end *)
	datatype info = INFO of int * int

    datatype ident	= IDENT of string * info
    type var		= ident
    type tyvar		= ident

    datatype longid	= LONGID of ident option * ident * info

    datatype lit = CCONlit of char * info
				 | ICONlit of int * info
				 | RCONlit of real * info
				 | SCONlit of string * info

	(* adrpo added 2005-10-27 the NAMEDty for named arguments in patterns *) 
    datatype ty	= VARty   of tyvar * info
				| CONSty  of ty list * longid * info
				| TUPLEty of ty list * info
				| RELty   of ty list * ty list * info
				| NAMEDty of ident * ty * info

	(* adrpo added 2005-10-27 the NAMEDpat for named arguments in patterns *)
	(* adrpo added 2005-11-07 the pat list ref component in the STRUCTpat *) 
    datatype pat = WILDpat of info
				 | LITpat of lit * info
				 | CONpat of longid * info
				 | STRUCTpat of longid option * pat list * pat list ref * info
				 | BINDpat of var * pat * info
				 | IDENTpat of ident * pat ref * info
				 | NAMEDpat of ident * pat * info 

    datatype exp = LITexp of lit * info
				 | CONexp of longid * info
				 | VARexp of longid * info
				 | STRUCTexp of longid option * exp list * info
				 | IDENTexp of longid * exp ref * info

	(* adrpo added 2005-11-08 the ref component in CALLgoal *) 
    datatype goal = CALLgoal of longid * exp list * pat list * pat list ref * info
				  | EQUALgoal of var * exp * info
				  | LETgoal of pat * exp * info
				  | NOTgoal of goal * info
				  | ANDgoal of goal * goal * info
				  | CONDgoal of goal * goal * goal * info

    datatype result	= RETURN of exp list * info
					| FAIL of info
					
	datatype attr = ATTRIBUTES of 
					{ 
						public: bool ref,
						final : bool ref,
						var   : bool ref,
						param : bool ref,
						const : bool ref,
						input : bool ref,
						output: bool ref,
						bidir : bool ref
					}
	(* 
	adrpo added 2005-11-08 the pat list ref component in the CLAUSE1 
	adrpo added 2006-03-18 the list of variables with types 
	*) 					
    datatype clause	= CLAUSE1 of goal option 
                               * ident 
                               * pat list 
                               * result 
                               * pat list ref 
                               * (ident * ty option * exp option * attr) list (* list of variables and their type *)
                               * info
					| CLAUSE2 of clause * clause * info 

    datatype conbind = CONcb of ident * info
					 | CTORcb of ident * ty list * info

    datatype datbind = DATBIND of tyvar list * ident * conbind list * info

    datatype typbind = TYPBIND of tyvar list * ident * ty * info

	(* 
	adrpo added 2006-03-18 the list of variables with types 
	*) 					
    datatype relbind = RELBIND of ident 
								* ty option 
								* clause
								* (ident * ty option * exp option * attr) list (* inputs * outputs * normal vars *)
								* (exp * info * pat * info) option (* match exp and match return exp *)								
								* info

    datatype spec = WITHspec of string * interface ref * info
				  | ABSTYPEspec of bool * tyvar list * ident * info
				  | TYPEspec of typbind list * info
				  | DATAspec of datbind list * typbind list * info
				  | VALspec of ident * ty * info
				  | RELspec of ident * ty * info

    and interface = INTERFACE of {modid: ident,
					specs: spec list,
					source: Source.source} * info

    datatype dec = WITHdec of string * interface ref * info
			| TYPEdec of typbind list * info
			| DATAdec of datbind list * typbind list * info
			| VALdec of ident * exp * info
			| RELdec of relbind list * info

	datatype module	= MODULE of interface * dec list * info
    (* --- end RML AST --- *)


  (* adrpo added *)
  (* -- start Modelica+ AST *)
  datatype Path = QUALIFIED of ident * Path * info
	      	| PATHIDENT of ident * info
	      	(*
	      	| TYPEVARIABLE of ident * info	      	
	      	*)
	      	
  (** A component reference is the fully or partially qualified name of *)
  (** a component.  It is represented as a list of *)
  (** identifier--subscript pairs.  The type `Path', on the other hand, *)
  (** is used to store references to class names, or names inside *)
  (** class definitions. *)

  (** Within statements *)
  datatype Within = WITHIN of Path | TOP

  (** - Each attribute *)
  (** The each keyword can be present in both MODIFICATION's and REDECLARATION's. *)
  datatype Each = EACH | NON_EACH

  (** - Restrictions *)
  datatype Restriction = 
  		         R_RECORD of info
		       | R_TYPE of info
		       | R_PACKAGE of info
		       | R_FUNCTION of info
		       | R_FUNCTION_TYPE of info		       
		       | R_ENUMERATION of info
		       | R_UNIONTYPE of info
		       | R_TYPEVARIABLE of info
		       
  (** These constructors each correspond to a different kind of class *)
  (** declaration in Modelica, except the last four, which are used *)
  (** for the predefined types.  The parser assigns each class *)
  (** declaration one of the restrictions, and the actual class *)
  (** definition is checked for conformance during translation.  The *)
  (** predefined types are created in the `Builtin' module and are *)
  (** assigned special restrictions. *)

  (** - Expressions *)
  datatype Operator = 
             (* integer operators *)
			  ADD of info  
			| SUB of info
			| MUL of info     
			| DIV of info
		    | UPLUS of info 
		    | UMINUS of info
		    | LESS of info  
		    | LESSEQ of info 
		    | GREATER of info 
		    | GREATEREQ of info 
		    | EQUAL of info 
		    | NEQUAL of info  
		    (* real operators *)
			| ADD_REAL of info  
			| SUB_REAL of info
			| MUL_REAL of info     
			| DIV_REAL of info
			| POW_REAL of info
		    | UPLUS_REAL of info 
		    | UMINUS_REAL of info
		    | LESS_REAL of info  
		    | LESSEQ_REAL of info 
		    | GREATER_REAL of info 
		    | GREATEREQ_REAL of info 
		    | EQUAL_REAL of info 
		    | NEQUAL_REAL of info  
		    (* boolean operators *)
		    | AND of info   
		    | OR of info
		    | NOT of info
		    (* string operators *)
		    | ADD_STRING of info   
		    | EQEQ_STRING of info
		    (* list operators *)
		    | ADD_LIST of info

  datatype Exp = 
			 INTEGER of int * info
		   (*
		   | CHAR of char * info 
		   *)
	       | REAL of real * info
	       | CREF of ComponentRef * info
	       | STRING of string * info
	       | BOOL of bool * info
	       | BINARY of Exp * Operator * Exp  * info (* Binary operations, e.g. a*b *)
	       | UNARY of Operator * Exp  * info (* Unary operations, e.g. -(x) *)
	       | LBINARY of Exp * Operator * Exp  * info (* Logical binary operations: and, or *)
	       | LUNARY of Operator * Exp  * info (* Logical unary operations: not *)
	       | RELATION of Exp * Operator * Exp  * info (* Relations, e.g. a >= 0 *)
	       | IFEXP of Exp * Exp * Exp * (Exp * Exp) list * info (* If expressions *)
	       | CALL of ComponentRef * FunctionArgs  * info (* Function calls *)
	       | ARRAY of FunctionArgs * info
	       | MATRIX of Exp list list * info
	       | RANGE of Exp * Exp option * Exp  * info(* Range expressions, e.g. 1:10 or 1:0.5:10 *)
	       | TUPLE of FunctionArgs  * info (*PR.*) (* Tuples used in function calls returning several values *)
	       | END of info (* array access operator for last element, e.g. a[end]:=1; *)
	       | MSTRUCTexp of Path option * FunctionArgs * info (* returned from match exps *)
	       | MWILDexp of info
	       | MATCHexp of MatchType * (* match or matchcontinue *)
						 Exp * (* match expression of *)
						 ElementItem list * (* local decls *)
						 Case list * (* case list + else in the end *)
						 string option * (* match expr comment_optional *)
						 info
		   | MBINDexp of ident * Exp * info (* is used for x as Exp *)


  and MatchType = MATCH | MATCHCONTINUE (* type of matching *)	  
  
  and FunctionArgs =  FUNCTIONARGS of NamedArg list * info
			| FOR_ITER_FARG of Exp list * ForIndices list * info
  (** The `FunctionArgs' datatype consists of a list of positional arguments *)
  (** followed by a list of named arguments (Modelica v2.0) *)
							  
  and NamedArg = NAMEDARG of ident option * Exp * info
  
  (** The `NamedArg' datatype consist of an identifier for the argument and an expression *)
  (** giving the value of the argument *)
  
  (** - Subscripts *)
  and Subscript = NOSUB of info
		     | SUBSCRIPT of Exp * info
  (** The `Subscript' datatype is used both in array declarations and *)
  (** component references.  This might seem strange, but it is *)
  (** inherited from the grammar.  The `NOSUB' constructor means that *)
  (** the dimension size is undefined when used in a declaration, and *)
  (** when it is used in a component reference it means a slice of the *)
  (** whole dimension. *)

  (** - Component references and paths *)
  and ComponentRef = CREF_QUAL of ident * Subscript list * ComponentRef * info
		    	| CREF_IDENT of ident * Subscript list * info
		    	
  (** - Modifications *)
  (** Modifications are described by the `Modification' type.  There *)
  (** are two forms of modifications: redeclarations and component *)
  (** modifications. *)
  and Modification = CLASSMOD of ElementArg list * Exp option * info

  (* Wrapper for things that modify elements, modifications and redeclarations *)
  and ElementArg = MODIFICATION of 
					bool * 
					Each * 
					ComponentRef * 
					Modification option * 
					string option * 
					info
		      | REDECLARATION of 
					bool * 
					Each * 
					ElementSpec * 
					ElementSpec option * 
					info

  (* ExternalDecl *)
  and ExternalDecl = EXTERNALDECL of 
				  ident option  * (* The name of the external function *)
				  string option * (* Lanugage of the external function *)
				  ComponentRef option * (* ouput parameter as return value*)
				  Exp list * (* only positional arguments, i.e. expression list*)
				  info      
    
  (** - Programs, the top level construct *)
  (** A program is simply a list of class definitions declared at top
   ** level in the source file, combined with a within statement that
   ** indicates the hieractical position of the program. 
   **)
  and modelica = PROGRAM of Class list (* List of classes *)
				* Within (* Within statement *)
				* interface 
				* info

  (** - Classes *)
  (** A class definition consists of a name, a flag to indicate if this *)
  (** class is declared as `partial', the declared class restriction, *)
  (** and the body of the declaration. *)
  and Class = CLASS of ident	 (* Name *)
			  * bool		     (* Partial *)
			  * bool		     (* Final *)
			  * bool		     (* Encapsulated *)
			  * Restriction	     (* Restricion *)
			  * ClassDef         (* Body *) (* not needed for type Type_a; declarations so: PARTS([],NONE,_) *)
			  * info
 
  (** The `ClassDef' type contains the definition part of a class *)
  (** declaration.  The definition is either explicit, with a list of *)
  (** parts (`public', `protected', `equationc' and `algorithm'), or it *)
  (** is a definition derived from another class or an enumeration type. *)
  (** For a derived type, the  type contains the name of the derived class and an optional *)
  (** array dimension and a list of modifications. An enumeration type contains a list of *)
  and ClassDef = PARTS of ClassPart list
			       * string option	       (* string comment *)
			       * info
		    | DERIVED of TypeSpec          (* Type *) 
		           * ElementAttributes     (* ElementAttributes *)
			       * ElementArg list
			       * Comment option	       (* comment *)
			       * info
		    | ENUMERATION of EnumLiteral list   
				* Comment option           (* comment*)		
				* info
  (* 
		type X = option<Type>            -> option type
		type O = list<Type>              -> list type
		type Y = record<Type,Type,Type>  -> tuple type
		type Z = Tree<Type>              -> uniontype with replaceable type variables
		type K = TypeSpec[:] -> Array<TypeSpec>
  *)   
  and TypeSpec = TPATH of Path * info 
               | TCOMPLEX of Path * TypeSpec list * info 		    

  (** EnumLiteral, which is a name in an enumeration and an optional
   Comment.*)
  and EnumLiteral = ENUMLITERAL of ident (* Literal *)
			 * Comment option	     (* comment *)
			 * info

  (** A class definition contains several parts.  There are public and *)
  (** protected component declarations, type definitions and `extends' *)
  (** clauses, collectively called elements.  There are also equation *)
  (** sections and algorithm sections. The EXTERNAL part is used only by functions *)
  (** which can be declared as external C or FORTRAN functions. *)
  and ClassPart = PUBLIC of ElementItem list * info
		     | PROTECTED of ElementItem list * info
		     | EQUATIONS of EquationItem list * info
		     | ALGORITHMS of AlgorithmItem list * info
		     | EXTERNAL of ExternalDecl * Annotation option * Annotation option * info

  (** An element item is either an element or an annotation *)
  and ElementItem = ELEMENTITEM of Element * info
		       | ANNOTATIONITEM of Annotation * info

  (** - Elements *)
  (* The basic element type in Modelica *)
  and Element = ELEMENT of bool		      (* final *)
			      * bool		      (* replaceable *)
			      * InnerOuter	      (* inner/outer *)	     
			      * ident                 (* Element name *)
			      * ElementSpec           (* Actual element specification*)
			      * info              (* file/location *)
			      * ElementSpec option (* only valid for classdef and component*)

  (** An element is something that occurs in a public or protected
   ** section in a class definition.  There is one constructor in the
   ** `ElementSpec' type for each possible element type.  There are
   ** class definitions (`CLASSDEF'), `extends' clauses (`EXTENDS')
   ** and component declarations (`COMPONENTS').
   ** 
   ** As an example, if the element `extends TwoPin;' appears
   ** in the source, it is represented in the AST as
   ** `EXTENDS(IDENT("TwoPin"),[])'.
   **)
  and ElementSpec = CLASSDEF of bool	     (* replaceable *)
				   * Class * info
		       | EXTENDS of Path * ElementArg list * info
		       | IMPORT of Import * Comment option * info
		       | COMPONENTS of ElementAttributes     (* ElementAttributes *)
		             * TypeSpec (* Type *)
				     * ComponentItem list
				     * info
				     
  (** One of the keyword inner and outer CAN be given to reference an inner or
      outer component. Thus there are three disjoint possibilities. **)		     
  and InnerOuter = INNER of info
				| OUTER of info
				| INNEROUTER of info
				| UNSPECIFIED

  (* Import statements, different kinds  *) 				     
  and Import = NAMED_IMPORT of ident * Path * info
		  | QUAL_IMPORT of Path * info
		  | UNQUAL_IMPORT of Path * info

  (* Collection of component and an optional comment *)
  and ComponentItem = COMPONENTITEM of Component
					  * Comment option
					  * info

  (* Some kind of Modelica entity (object or variable) *)
  and Component = COMPONENT of ident 	     (* component name *)
				  * Modification option (* Optional modification *)
				  * info 

  (** Several component declarations can be grouped together in one *)
  (** `ElementSpec' by writing them on the same line in the source. *)
  (** This type contains the information specific to one component. *)
  and EquationItem = EQUATIONITEM of Equation * Comment option * info
			| EQUATIONITEMANN of Annotation * info 

  (** Info specific for an algorithm item. *)
  and AlgorithmItem = ALGORITHMITEM  of Algorithm * Comment option * info
			 |  ALGORITHMITEMANN of Annotation * info

  and ForIndices = FORINDICE of ident * Exp option * info
  (* Information on one (kind) of equation, different constructors for different
     kinds of equations *)
  and Equation = EQ_IF of Exp                     (* Conditional expression *)
			     * EquationItem list	     (* true branch *)  
			     * (Exp * EquationItem list) list (* elseif branches *)
			     * EquationItem list	     (* else branch *)
			     * info
		    | EQ_EQUALS of Exp * Exp * info (* Standard 2-side eqn*)
		    | EQ_CONNECT of ComponentRef * ComponentRef * info (* Connect stmt *)
		    | EQ_FOR of ForIndices list * EquationItem list * info (* For-loops *)
		    | EQ_WHEN_E of Exp (* Condition *)
				* EquationItem list (* Loop body *)
				* (Exp * EquationItem list) list (* else when *)
				* info
		    | EQ_NORETCALL of ComponentRef * FunctionArgs * info(* fcalls without return value *)
		    | EQ_FAILURE of Equation * info (* not goals *)
		    | EQ_EQUALITY of Equation * info(* unification goals *)
		    
  (** The `Algorithm' type describes one algorithm statement in an *)
  (** algorithm section.  It does not describe a whole algorithm.  The *)
  (** reason this type is named like this is that the name of the *)
  (** grammar rule for algorithm statements is `algorithm'. *)
  and Algorithm = ALG_ASSIGN of Exp * Exp * info
		     | ALG_TUPLE_ASSIGN of Exp 		(*tuple*)  
					* Exp 		(* value*) 	
					* info
		     | ALG_IF of Exp
			       * AlgorithmItem list	     (* true branch *)  
			       * (Exp * AlgorithmItem list) list (* elseif *)
			       * AlgorithmItem list	     (* else branch *)
			       * info
		     | ALG_FOR of ForIndices list * AlgorithmItem list * info
		     | ALG_WHILE of Exp * AlgorithmItem list * info
		     | ALG_WHEN_A of Exp 
					* AlgorithmItem list 
					* (Exp * AlgorithmItem list) list (* elsewhen *)
					* info
		     | ALG_NORETCALL of ComponentRef * FunctionArgs * info(* general fcalls without return value *)
		     | ALG_FAILURE of Algorithm * info  (* not goals *)
		     | ALG_EQUALITY of Algorithm * info (* unification goals *)

  and Case = CASE of Pattern list * (* patterns to be matched *) 
					 ElementItem list * (* local decls *)
					 ClassPart  * (* equations [] for no equations: axioms *)
					 Exp * (* result: 
					          when nothing is returned: MSTRUCTexp(NONE,   [], _)
					          when fail is returned:    MSTRUCTexp(SOME("FAIL"), [], _) *) 
					 string option * (* comment after case like: case pattern string_comment *) 
					 info
           | ELSE of ElementItem list * (* local decls *)
					 ClassPart  * (* equations [] for no equations: axioms *)
					 Exp * (* result: 
					          when nothing is returned: MSTRUCTexp(NONE,   [], _)
					          when fail is returned:    MSTRUCTexp(SOME("FAIL"), [], _) *)
					 string option * (* comment after case like: case pattern string_comment *)
					 info
					 
  (* Variability *) 
  and Variability = VAR
				| PARAM of info
				| CONST of info
				
  and Direction = INPUT of info
				| OUTPUT of info
				| BIDIR of info
  
  (** - Component attributes *)
  and ElementAttributes = ATTR of 
			     	       Variability (* parameter, constant etc. *)
			     	     * Direction
						 * info
  (** Component attributes are *)
  (** properties of components which are applied by type prefixes. *)
  (** As an example, declaring a component as `input Real x;' will *)
  (** give the attributes `ATTR([],false,VAR,INPUT)'. *)
  
  (** Annotation *)
  and Annotation = ANNOTATION of ElementArg list * info
  (** An Annotation is a class_modification. *)

  (** Comment *)	
  and Comment = COMMENT of Annotation option 
				* string option 
				* info
				
  (** Modelica+ Patterns **)
  and Pattern = MWILDpat of info
			| MLITpat of Exp * info
			| MCONpat of Path * info
			| MSTRUCTpat of Path option * Pattern list * info
			| MBINDpat of var * Pattern * info             (* x as pat *)
			| MIDENTpat of ident * Pattern ref * info
			| MNAMEDARGpat of var * Pattern * info (* name = pat *)

  (* --- end Modelica+ AST --- *)

  datatype serializationInfo = SERIALIZE of { 
                                              file:    string, (* which filename was serialized *)
                                              date:    string, (* date as string when it was serialized *)
                                              version: int    (* version of the serialized file *) 
                                            }

  datatype serialized = SERIALIZED of serializationInfo
                                    * module option (* the parsed module *)

  datatype program = RML_FILE of string (* file *)
                               * module
                   | MOD_FILE of string (* file *)
                               * modelica 
                   | SRZ_FILE of string (* file *)
                               * serialized

  structure IdentDict =
      AADictFn(
      struct
		 type ord_key = ident
		 fun compare(IDENT(s1,_), IDENT(s2,_)) = String.compare(s1, s2)
	  end)

  val dummyInfo = INFO(~1, ~1)

  fun makeIdent(name, info) = IDENT(name, info)
  fun rmlIdent name = IDENT(name, dummyInfo)
  fun identName(IDENT(name,_)) = name
  fun identCtxInfo(IDENT(_, info)) = info
  fun lidentName(LONGID(SOME(IDENT(name1,_)),IDENT(name2,_),_)) = name1^"."^name2
  |	lidentName(LONGID(NONE,IDENT(name,_),_)) = name
  fun lidentCtxInfo(LONGID(_, _, info)) = info    
  fun identEqual(IDENT(s1,_), IDENT(s2,_)) = (s1 = s2)

  fun litEqual(CCONlit(c1, _), CCONlit(c2, _)) = (c1 = c2)
    | litEqual(ICONlit(i1, _), ICONlit(i2, _)) = (i1 = i2)
    | litEqual(RCONlit(r1, _), RCONlit(r2, _)) = Real.==(r1, r2)
    | litEqual(SCONlit(s1, _), SCONlit(s2, _)) = (s1 = s2)
    | litEqual(_, _) = false

  fun litString(CCONlit(c, _)) = MakeString.ccvt c
    | litString(ICONlit(i, _)) = MakeString.icvt i
    | litString(RCONlit(r, _)) = MakeString.rcvt r
    | litString(SCONlit(s, _)) = MakeString.scvt s
    
  val dummyInterface =
      INTERFACE({modid = rmlIdent "", specs = [], source = Source.dummy}, dummyInfo)
            
  fun getLastPathAsString(QUALIFIED(_, x, _)) = getLastPathAsString(x)
  |   getLastPathAsString(PATHIDENT(IDENT(id,_), _)) = id
  (*
  |   getLastPathAsString(TYPEVARIABLE(IDENT(id,_), _)) = id
  *)

  fun getCrefAsString(CREF_QUAL(IDENT(s,_), _, cr, _)) = s^"."^getCrefAsString(cr)
  |   getCrefAsString(CREF_IDENT(IDENT(s,_), _, _)) = s

  fun getPathAsString(QUALIFIED(IDENT(s,_), p, _)) = s^"."^getPathAsString(p)
  |   getPathAsString(PATHIDENT(IDENT(s,_), _)) = s
  (*
  |   getPathAsString(TYPEVARIABLE(IDENT(s,_), _)) = s
  *)

  fun getPozFromInfo(INFO(sp,ep)) = ((sp,~1,~1),(ep,~1,~1))
		  
  fun filterImports([]) = []
  |	  filterImports(ELEMENTITEM(ELEMENT(_,_,_,ident, IMPORT(import,_, info), _, _),_)::rest) =
			(
			case import of
				QUAL_IMPORT(path, importInfo) => 
				((getLastPathAsString path)^".mo", getPozFromInfo(importInfo))::filterImports(rest)
			|	UNQUAL_IMPORT(path, importInfo) => 
				((getLastPathAsString path)^".mo", getPozFromInfo(importInfo))::filterImports(rest)
			|	_ => filterImports(rest)
			)
	|   filterImports(_::rest) = filterImports(rest)

  fun filterFunctionsAndRecords([]) = []
  |   filterFunctionsAndRecords(
		ELEMENTITEM(
			ELEMENT(_,_,_,ident, CLASSDEF(_, class, _), _, _),_)::rest) =
			(
			case class of
				CLASS(id, _, _, _, r as R_RECORD(_), _, info) =>
				(identName id, r)::filterFunctionsAndRecords(rest)
			|	CLASS(id, _, _, _, r as R_FUNCTION(_), _, info) => 
				(identName id, r)::filterFunctionsAndRecords(rest)
			|	_ => filterFunctionsAndRecords(rest)
			)
  |   filterFunctionsAndRecords(_::rest) = filterFunctionsAndRecords(rest)
  
end (* functor AbsynFn *)