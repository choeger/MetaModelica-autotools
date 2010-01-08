(* code/difftoc.sml *)

functor DiffToCFn(structure Code : SWITCH
		  structure SwitchIntras : SWITCH_INTRAS
		  structure CodeToC : CODETOC
		  sharing type SwitchIntras.Code.gvar_name = Code.gvar_name
		  sharing Code = SwitchIntras.Code = CodeToC.Code
		    ) : SWITCHTOC =
  struct

    structure Code = Code

    val output = TextIO.output

    fun emitModule(os, ((prefix, ext), module as Code.MODULE{modname, xmods, xlabs, xvals, values, litdefs, labdefs, source, ...})) =
	let 
	val _ = CodeToC.setCurrentSource(source)
	
    fun prLabTabEntry os modfun (Code.LABDEF{label,...}) =
      (output(os, "RML_LABTAB_ENTRY(");
       CodeToC.prLabel os label;
       output(os, ",");
       output(os, modfun);
       output(os, ")\n"))

    fun prLabCheck os (Code.LABDEF{label,...}, tagno) =
      (output(os, "\tcheck_label(&");
       CodeToC.prLabel os label;
       output(os, ",TAG(");
       CodeToC.prInt os tagno;
       output(os, "));\n");
       tagno+1)

    fun emit_labdecs(_, [], _) = ()
      | emit_labdecs(os, labdefs as Code.LABDEF{label,...}::_, modname) =
	  (output(os, "\
\\n\
\#ifdef RML_GCCGOTO_NOSHIFT\n\
\#define LABDIFF(LABI,LAB0) ((rml_uint_t)(LABI)-(rml_uint_t)(LAB0))\n\
\#define TAG(k) ((k)<<RML_LOG2_SIZE_INT)\n\
\#else\n\
\#define LABDIFF(LABI,LAB0) ((((rml_uint_t)(LABI)-(rml_uint_t)(LAB0)))/(sizeof(rml_label_t)))\n\
\#define TAG(k) (k)\n\
\#endif\n");
	   output(os, "RML_LABTAB_SETUP(module__");
	   output(os, modname);
	   output(os, ")\n");
	   List.app (prLabTabEntry os ("module__" ^ modname)) labdefs;
	   output(os, "\
\#include <stdio.h>\n\
\#include <stdlib.h>\n\
\static void check_label(rml_labptr_t l, long tag)\n\
\{\n\
\\tlong off = (long)LABDIFF(l,&");
	   CodeToC.prLabel os label;
	   output(os, "\
\);\n\
\\tif( off != tag ) {\n\
\\t\tfprintf(stderr, \"");
	   output(os, modname);
	   output(os, ": label %ld has offset %ld -- aborting..\\n\", tag, off);\n\
\\t\tabort();\n\
\\t}\n\
\}\n\
\static void check_all_labels(void)\n\
\{\n");
	   List.foldl (prLabCheck os) 0 labdefs;
	   output(os, "\
\}\n"))

    fun prLabBody os (Code.LABDEF{label,varHP,nalloc,nargs,code,...}, tagno) =
      (output(os, "CASE(TAG(");
       CodeToC.prInt os tagno;
       output(os, "))\n");
       output(os, "label__");
       CodeToC.prLabel os label;
       output(os, ": {");
       if nalloc > 0 then
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
	  output(os, "));"))
       else ();
       CodeToC.prCode os code;
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
      | emit_labdefs(os, labdefs as Code.LABDEF{label=label0,...}::_, modname) =
	  let val need_gc = List.exists label_uses_alloc labdefs
	      and intras = List.foldl SwitchIntras.labdefIntras [] labdefs
	  in
	    output(os, "\nRML_BEGIN_MODULE(module__");
	    output(os, modname);
	    output(os, ")\n{\n");
	    if need_gc then output(os, "\tunsigned gc_nwords, gc_nargs;\n") else ();
	    output(os, "\trml_uint_t switch_tag;\n");
	    output(os, "#ifdef RML_DIFF_LABEL0_INREG\n");
	    output(os, "\trml_labptr_t myLabel0 = &");
	    CodeToC.prLabel os label0;
	    output(os, ";\n");
	    output(os, "#else\n");
	    output(os, "#define myLabel0 (&");
	    CodeToC.prLabel os label0;
	    output(os, ")\n");
	    output(os, "#endif\n");
	    List.app (prDeclIntraVar os) intras;
	    output(os, "#ifdef RML_GCCGOTO\n");
	    output(os, "\tstatic void ** const labels_tab[] = {\n");
	    List.app (prGCCLabPtr os) labdefs;
	    output(os, "\t};\n");
	    output(os, "#ifdef RML_GCCGOTO_NOSHIFT\n");
	    output(os, "\tconst char *labels_ptr = (const char*)labels_tab;\n");
	    output(os, "#endif\n");
	    output(os, "#define CASE(TAG)\t/*empty*/\n");
	    output(os, "#define DEFAULT\tcaseDefault\n");
	    output(os, "#else\n");
	    output(os, "#define CASE(TAG)\tcase TAG:\n");
	    output(os, "#define DEFAULT\tdefault\n");
	    output(os, "#endif\n");
	    List.app (prLoadIntraVar os) intras;
	    output(os, "mask_dispatch:\n");
	    output(os, "\tswitch_tag = LABDIFF(theLabel,myLabel0);\n");
	    output(os, "#ifdef RML_GCCGOTO\n");
	    output(os, "\tif( switch_tag > TAG(");
	    output(os, Int.toString(List.length labdefs - 1));
	    output(os, ") ) goto caseDefault;\n");
	    output(os, "#endif\n");
	    output(os, "tag_dispatch:\n");
	    output(os, "#ifdef RML_GCCGOTO\n");
	    output(os, "#ifdef RML_GCCGOTO_NOSHIFT\n");
	    output(os, "\tgoto **(const void*const*const*)(labels_ptr + switch_tag);\n");
	    output(os, "#else\n");
	    output(os, "\tgoto *labels_tab[switch_tag];\n");
	    output(os, "#endif\n");
	    output(os, "#else\n");
	    output(os, "\tswitch( switch_tag )\n");
	    output(os, "#endif\n");
	    output(os, "\t{\n");
	    List.foldl (prLabBody os) 0 labdefs;
	    output(os, "DEFAULT:\n");
	    List.app (prFlushIntraVar os) intras;
	    output(os, "epilogue:\n");
	    output(os, "\treturn theLabel;\n");
	    output(os, "\t}\n");
	    if need_gc then
	      (output(os, "collect:\n");
	       List.app (prFlushIntraVar os) intras;
	       output(os, "\trml_prim_gc(gc_nwords, gc_nargs);\n");
	       List.app (prLoadIntraVar os) intras;
	       output(os, "\tgoto tag_dispatch;\n"))
	    else ();
	    output(os, "}\nRML_END_MODULE\n")
	  end

  val (Code.Mangle.NAME modname) = Code.Mangle.encode modname
  fun emit_labchecks() =
    case labdefs
      of [] => ()
       | _ => output(os, "\tcheck_all_labels();\n")
  in
	output(os, "/* module "); output(os, modname); output(os, " */\n");
	output(os, "#include \"rml.h\"\n");
	List.app (CodeToC.prImpLab os) xlabs;
	List.app (CodeToC.prValDec os) xvals;
	emit_labdecs(os, labdefs, modname);
	CodeToC.emitLitdefs os litdefs;
	CodeToC.emitValues os values;
	CodeToC.prInitProc (os, prefix) emit_labchecks module;
	emit_labdefs(os, labdefs, modname)
  end

    fun emitInterface(os, module) = CodeToC.prInterface os module

  end (* functor DiffToCFn *)
