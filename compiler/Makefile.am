GENERATED=persistent.grm.sig persistent.grm.desc persistent.grm.sml \
mod.grm.sig mod.grm.desc mod.grm.sml \
rml.grm.sig rml.grm.desc rml.grm.sml \
persistent.lex.sml mod.lex.sml rml.lex.sml

SOURCES=AADictFn.sml CodeToCFn.sml CPSToSML.sml FOL_UNIFY.sig LexArg.sml MOToRML.sml PMC.sml StartFn.sml SysDep.sml\
AbsynFn.sml CODETOC.sig CPSToSwitch.sml FOLUnify.sml LEXUTIL.sig ORD_DICT.sig PP.sig START.sig TransEnvFn.sml\
ABSYN_INSTRUMENTED.sig CONREP.sig CPSUsagesFn.sml FOLUsagesFn.sml LexUtil.sml ORD_KEY.sig PP.sml Start.sml TRANSENV.sig\
AbsynPersistFn.sml Conrep.sml CPS_USAGES.sig FOL_USAGES.sig MainFn.sml PARSE.sig ReorderFn.sml StatElabFn.sml TransEnv.sml\
ABSYN_PERSIST.sig CONTROL.sig CPSUsages.sml FOLUsages.sml MainMLton.sml persistent.grm.sig REORDER_SCC.sig STAT_ELAB.sig TyCombFn.sml\
AbsynPersist.sml Control.sml CPSUsefulFn.sml FrontEndFn.sml MAIN.sig persistent.grm.sml ReorderSCC.sml StatElab.sml TY_COMB.sig\
AbsynPrintFn.sml CPSFn.sml CPS_USEFUL.sig FRONTEND.sig Main.sml PERSISTENTLex.sml REORDER.sig StatObjFn.sml TyComb.sml\
ABSYN_PRINT.sig CPSFVarsFn.sml CPSUseful.sml FrontEnd.sml make-mlton.sml PERSISTENTLrVals.sml Reorder.sml STAT_OBJ.sig TyFcnFn.sml\
AbsynPrint.sml CPS_FVARS.sig DiffToCFn.sml InstrumentFn.sml MAKESTRING.sig PERSISTENTParseFn.sml ReorderTyFn.sml StatObj.sml TY_FCN.sig\
ABSYN.sig CPSFVars.sml DiffToC.sml Instrument.sml MakeString.sml PERSISTENTParser.sml REORDER_TY.sig STR_DICT.sig TyFcn.sml\
Absyn.sml CPSOptimFn.sml FOLFn.sml IntDict.sml MangleFn.sml PERSISTENT_PARSE.sig ReorderTy.sml StrDict.sml TySchemeFn.sml\
AbsynToFOLFn.sml CPS_OPTIM.sig FOLOptimFn.sml InterpCoreFn.sml MANGLE.sig PERSISTENTParse.sml ReorderValFn.sml StrKey.sml TY_SCHEME.sig\
ABSYNTOFOL.sig CPSOptim.sml FOL_OPTIM.sig INTERP_CORE.sig Mangle.sml PlainCodeToC.sml REORDER_VAL.sig SwitchCodeToC.sml TyScheme.sml\
AbsynToFOL.sml CPSPrintFn.sml FOLOptim.sml InterpCore.sml MaskToCFn.sml PlainFn.sml ReorderVal.sml SwitchFn.sml TY.sig\
CacheFn.sml CPS_PRINT.sig FOLPrintFn.sml InterpFn.sml MaskToC.sml PlainFVars.sml RMLLex.sml SwitchFVars.sml Ty.sml\
CACHE.sig CPSPrint.sml FOL_PRINT.sig INTERP.sig MODLex.sml PlainOptim.sml RMLLrVals.sml SwitchIntrasFn.sml UTIL.sig\
Cache.sml CPS.sig FOLPrint.sml Interp.sml MODLrVals.sml PLAIN.sig RMLParseFn.sml SWITCH_INTRAS.sig Util.sml\
call-main.sml CPS.sml FOL.sig IntKey.sml MODParseFn.sml Plain.sml RMLParser.sml SwitchIntras.sml VersionFn.sml\
CodeFVarsFn.sml CPSToCodeFn.sml FOL.sml LexArgFn.sml MODParser.sml PlainToCFn.sml RML_PARSE.sig SwitchOptim.sml VERSION.sig\
CODE_FVARS.sig CPSTOCODE.sig FOLToCPSFn.sml LEXARG.sig MOD_PARSE.sig PLAINTOC.sig RMLParse.sml SWITCH.sig Version.sml\
CodeOptimFn.sml CPSToPlain.sml FOLTOCPS.sig LexArgSimpleFn.sml MODParse.sml PlainToC.sml RML.sml Switch.sml\
CODE_OPTIM.sig CPSToSMLFn.sml FOLToCPS.sml LEXARG_SIMPLE.sig MOToRMLFn.sml PMCFn.sml SOURCE.sig SWITCHTOC.sig\
CODE.sig CPSTOSML.sig FOLUnifyFn.sml LexArgSimple.sml MOTORML.sig PMC.sig Source.sml SYSDEP.sig \
mod.grm persistent.grm rml.grm mod.lex persistent.lex rml.lex

bin_SCRIPTS = rml$(EXEEXT)

CLEANFILES=$(GENERATED)

rml$(EXEEXT): $(GENERATED) $(SOURCES)
	$(MLTON) rml.mlb

%.grm.sig %.grm.sml: %.grm
	$(MLYACC) $< 

%.lex.sig %.lex.sml: %.lex
	$(MLLEX) $< 

