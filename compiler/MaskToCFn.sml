(* code/masktoc.sml *)

functor MaskToCFn(structure Code : SWITCH 
		  structure SwitchIntras : SWITCH_INTRAS
		  structure CodeToC : CODETOC
		  sharing type SwitchIntras.Code.gvar_name = Code.gvar_name
		  sharing Code = SwitchIntras.Code = CodeToC.Code
		    ) : SWITCHTOC =
  struct

    structure Code = Code

    fun emitModule(os, ((prefix, ext), module as Code.MODULE{modname, xmods, xlabs, xvals, values, litdefs, labdefs, source, ...})) =
	let
	val _ = CodeToC.setCurrentSource(source)
    val output = TextIO.output

    fun prLabStruct os modname (Code.LABDEF{globalP,label,...}, tagno) =
      (if globalP then () else output(os, "static ");
       output(os, "const rml_label_t ");
       CodeToC.prLabel os label;
       output(os, " = { module__");
       output(os, modname);
       output(os, ", TAG(");
       CodeToC.prInt os tagno;
       output(os, "), &mask__");
       output(os, modname);
       output(os, " };\n");
       tagno+1)

    fun emit_labdecs(_, [], _) = ()
      | emit_labdecs(os, labdefs, modname) =
	  (output(os, "\n");
	   output(os, "#ifdef RML_GCCGOTO_NOSHIFT\n");
	   output(os, "#define TAG(k) ((k)<<RML_LOG2_SIZE_INT)\n");
	   output(os, "#else\n");
	   output(os, "#define TAG(k) (k)\n");
	   output(os, "#endif\n");
	   output(os, "RML_FORWARD_MODULE(module__");
	   output(os, modname);
	   output(os, ");\n");
	   output(os, "static unsigned mask__");
	   output(os, modname);
	   output(os, ";\n");
	   List.foldl (prLabStruct os modname) 1 labdefs;
	   ())

    fun prLabBody os (Code.LABDEF{label,varHP,nalloc,nargs,code,...}, tagno) =
      (output(os, "CASE(TAG(");
       CodeToC.prInt os tagno;
       output(os, "))\n");
       output(os, "label__");
       CodeToC.prLabel os label;
       output(os, ": {");
       if nalloc > 0 
       then
		 (output(os, "\n\tvoid *");
		  CodeToC.prLVar os varHP;
		  output(os, ";\n\tRML_ALLOC(");
		  CodeToC.prLVar os varHP;
		  output(os, ",");
		  CodeToC.prInt os nalloc;
		  output(os, ",");
		  CodeToC.prInt os nargs;
		  output(os, ",TAG(");
		  CodeToC.prInt os tagno;
		  output(os, "));");
		  output(os, "\n\t{\n\t"))
       else ();
       CodeToC.prCode os code;
       if nalloc > 0 
       then output(os, "\n\t}\n")
       else ();       
       output(os, "}\n");
       tagno+1)

    fun label_uses_alloc(Code.LABDEF{nalloc,...}) = nalloc > 0

    fun prGCCLabPtr os (Code.LABDEF{label,...}) =
      (output(os, "\t\t&&label__");
       CodeToC.prLabel os label;
       output(os, ",\n"))

    fun prDeclIntraVar os name =
      (output(os, "\tvoid *");
       CodeToC.prGVar os (Code.GVAR{scope=Code.INTRAgvs,name=name});
       output(os, ";\n"))

    fun prLoadIntraVar os name =
      (output(os, "\t");
       CodeToC.prGVar os (Code.GVAR{scope=Code.INTRAgvs,name=name});
       output(os, " = ");
       CodeToC.prGVar os (Code.GVAR{scope=Code.INTERgvs,name=name});
       output(os, ";\n"))

    fun prFlushIntraVar os name =
      (output(os, "\t");
       CodeToC.prGVar os (Code.GVAR{scope=Code.INTERgvs,name=name});
       output(os, " = ");
       CodeToC.prGVar os (Code.GVAR{scope=Code.INTRAgvs,name=name});
       output(os, ";\n"))

    fun emit_labdefs(_, [], _) = ()
      | emit_labdefs(os, labdefs, modname) =
	  let val need_gc = List.exists label_uses_alloc labdefs
	      and intras = List.foldl SwitchIntras.labdefIntras [] labdefs
	  in
	    output(os, "\nRML_BEGIN_MODULE(module__");
	    output(os, modname);
	    output(os, ")\n{\n");
	    if need_gc then output(os, "\tunsigned gc_nwords, gc_nargs;\n") else ();
	    output(os, "\tconst rml_label_t *theLabel;\n");
	    List.app (prDeclIntraVar os) intras;
	    output(os, "#ifdef RML_GCCGOTO\n");
	    output(os, "\tstatic void ** const labels_tab[] = {\n");
	    output(os, "\t\t&&labelTrapHandler,\n");
	    List.app (prGCCLabPtr os) labdefs;
	    output(os, "\t};\n");
	    output(os, "#ifdef RML_GCCGOTO_NOSHIFT\n");
	    output(os, "\tconst char *labels_ptr = (const char*)labels_tab;\n");
	    output(os, "#endif\n");
	    output(os, "#define CASE(TAG)\t/*empty*/\n");
	    output(os, "#else\n");
	    output(os, "#define CASE(TAG)\tcase TAG:\n");
	    output(os, "#endif\n");
	    output(os, "\tmask__");
	    output(os, modname);
	    output(os, " = (unsigned)~0;\n");
	    if need_gc then output(os, "retry_after_gc:\n") else ();
	    List.app (prLoadIntraVar os) intras;
	    output(os, "\tgoto dispatch;\n");
	    output(os, "mask_dispatch:\n");
	    output(os, "\tswitch_tag = theLabel->tag & *(theLabel->mask);\n");
	    output(os, "dispatch:\n");
	    output(os, "#ifdef RML_GCCGOTO\n");
	    output(os, "#ifdef RML_GCCGOTO_NOSHIFT\n");
	    output(os, "\tgoto **(const void*const*const*)(labels_ptr + switch_tag);\n");
	    output(os, "#else\n");
	    output(os, "\tgoto *labels_tab[switch_tag];\n");
	    output(os, "#endif\n");
	    output(os, "#else\n");
	    output(os, "\tswitch( switch_tag ) {\n");
	    output(os, "#endif\n");
	    List.foldl (prLabBody os) 1 labdefs;
	    output(os, "CASE(TAG(0))\n");
	    output(os, "labelTrapHandler:\n");
	    List.app (prFlushIntraVar os) intras;
	    output(os, "\tgoto epilogue;\n");
	    output(os, "#ifndef RML_GCCGOTO\n");
	    output(os, "\t}\n");
	    output(os, "#endif\n");
	    if need_gc then
	      (output(os, "collect:\n");
	       List.app (prFlushIntraVar os) intras;
	       output(os, "\trml_prim_gc(gc_nwords, gc_nargs);\n");
	       output(os, "\tgoto retry_after_gc;\n"))
	    else ();
	    output(os, "epilogue:\n");
	    output(os, "\tmask__");
	    output(os, modname);
	    output(os, " = 0;\n");
	    output(os, "\treturn theLabel;\n");
	    output(os, "}\nRML_END_MODULE\n")
	  end

  val (Code.Mangle.NAME modname) = Code.Mangle.encode modname
  in
	output(os, "/* module "); output(os, modname); output(os, " */\n");
	output(os, "#include \"rml.h\"\n");
	List.app (CodeToC.prImpLab os) xlabs;
	List.app (CodeToC.prValDec os) xvals;
	emit_labdecs(os, labdefs, modname);
	CodeToC.emitLitdefs os litdefs;
	CodeToC.emitValues os values;
	CodeToC.prInitProc (os,prefix) (fn () => ()) module;
	emit_labdefs(os, labdefs, modname)
  end

    fun emitInterface(os, module) = CodeToC.prInterface os module

  end (* functor MaskToCFn *)
