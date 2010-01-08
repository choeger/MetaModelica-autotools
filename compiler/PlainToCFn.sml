(* code/plaintoc.sml *)
functor PlainToCFn(structure Util : UTIL
		   structure Code : PLAIN
		   structure CodeToC : CODETOC
                   structure Control : CONTROL
		   sharing Code = CodeToC.Code
		     ) : PLAINTOC =
  struct

    structure Code = Code
    structure ConRep = Code.ConRep

    fun emitModule(os, ((prefix, ext), module as Code.MODULE{modname, xmods, xlabs, xvals, values, litdefs, labdefs, source, ...})) =
    let 
	val _ = CodeToC.setCurrentSource(source)
	
    val output = TextIO.output
    val output1 = TextIO.output1

    fun prLabDec os (Code.LABDEF{globalP, label, ...}) =
      (if globalP then () else output(os, "static ");
       output(os, "RML_FORWARD_LABEL(");
       CodeToC.prLabel os label;
       output(os, ");\n"))

    fun emitLabDecs(_, []) = ()
      | emitLabDecs(os, labdefs) =
	  (output1(os, #"\n"); List.app (prLabDec os) labdefs)

    fun prLabBody os (Code.LABDEF{globalP,label,varHP,nalloc,nargs,code,pos,...}) =
     (output1(os, #"\n");
      if globalP 
      then () 
      else output(os, "static ");
	 	   output(os, "RML_BEGIN_LABEL(");
		   CodeToC.prLabel os label;
		   output(os, ")\n{");
     if nalloc > 0 
     then
	 (
		output(os, "\tvoid *");
		CodeToC.prLVar os varHP;
		output(os, ";");
		output(os, "\n\tRML_ALLOC(");
		CodeToC.prLVar os varHP;
		output1(os, #",");
		CodeToC.prInt os nalloc;
		output1(os, #",");
		CodeToC.prInt os nargs;
		output1(os, #",");
		CodeToC.prLabel os label;	(* for `new' runtime *)
		output(os, ");");
		(*
		if !Control.doDebug
		then CodeToC.instrumentCont os ("success", Code.POSITION(pos), CodeToC.getLabelName label, "goal")
		else ();
		*)
		if !Control.doTrace
		then
		(
			output(os, "\n\tif (rml_trace_enabled)\n\t{fprintf(stderr, \"");
			CodeToC.prLabelUnmangled os label; output(os, "\\n\"); fflush(stderr); \n\t}\n")
		)
		else
			()
	  )
      else 
      (
		(*
		if !Control.doDebug
		then CodeToC.instrumentCont os ("success", Code.POSITION(pos), CodeToC.getLabelName label, "goal")
		else ();
		*)
		if !Control.doTrace
		then
		(      
			output(os, "if (rml_trace_enabled)\n\t{\n\tfprintf(stderr, \"");
			CodeToC.prLabelUnmangled os label; output(os, "\\n\"); fflush(stderr);\n\t}\n")
		)
		else
			()
      );
            
      output(os, "\n\t{");
      CodeToC.prCode os code;
      output(os, "}");      
      output(os, "\n}\nRML_END_LABEL\n"))

    fun emitLabDefs(os, labdefs) = List.app (prLabBody os) labdefs

  val (Code.Mangle.NAME modname) = Code.Mangle.encode modname
  in
	output(os, "/* module "); output(os, modname); output(os, " */\n");
	output(os, "#include \"rml.h\"\n");
	output(os, "#include <stdlib.h>\n");
	output(os, "#include <stdio.h>\n");
	if !Control.fixJavaNames 
	then output(os, "#include \"ExternalRMLDefines.h\"\n")	
	else ();
	(* adrpo added 2004-10-19: include module name in file *)
	output(os, "RML_DEFINE_MODULE(\""); output(os, modname); output(os, "\")\n");
	List.app (CodeToC.prImpLab os) xlabs;
	List.app (CodeToC.prValDec os) xvals;
	emitLabDecs(os, labdefs);
	CodeToC.emitLitdefs os litdefs;
	CodeToC.emitValues os values;
	CodeToC.prInitProc (os,prefix) (fn () => ()) module;
	emitLabDefs(os, labdefs)
  end

    fun emitInterface(os, module) = CodeToC.prInterface os module

  end (* functor PlainToCFn *)
