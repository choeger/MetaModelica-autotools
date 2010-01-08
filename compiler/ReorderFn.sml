(* reorder/reorder.sml *)

functor ReorderFn(
	structure ReorderTy : REORDER_TY
	structure ReorderVal : REORDER_VAL
	structure Control : CONTROL		  
	structure Util : UTIL
	sharing ReorderTy.Absyn = ReorderVal.Absyn		  
	) : REORDER =
  struct

    structure Absyn = ReorderTy.Absyn
    
	val debugFlag = false
	fun debug s = if (debugFlag) then Util.outStdErr ("ReorderFn."^s) else ()    

    fun addTyp(typbnd, ts) = ReorderTy.TYPBND(typbnd)::ts
    fun addDat(datbnd, ts) = ReorderTy.DATBND(datbnd)::ts

    (* ADI!!!*) 
    fun typSpec(ReorderTy.TYPDEC typbnd) = Absyn.TYPEspec([typbnd], Absyn.dummyInfo)
      | typSpec(ReorderTy.DATDEC(datbnds,typbnds)) = Absyn.DATAspec(datbnds,typbnds, Absyn.dummyInfo)

    (* ADI!!!*) 
    fun typDec(ReorderTy.TYPDEC typbnd) = Absyn.TYPEdec([typbnd], Absyn.dummyInfo)
      | typDec(ReorderTy.DATDEC(datbnds,typbnds)) = Absyn.DATAdec(datbnds,typbnds, Absyn.dummyInfo)

    fun reorderSpecs(source, specs) =
      let fun split([], wsas, ts, vs) = wsas @ map typSpec (ReorderTy.reorderTyBnds(source, ts)) @ vs
	    | split(spec::specs, wsas, ts, vs) =
		case spec
		  of Absyn.WITHspec _ => split(specs, spec::wsas, ts, vs)
		   | Absyn.ABSTYPEspec _ => split(specs, spec::wsas, ts, vs)
		   | Absyn.TYPEspec(typbnds, _) => split(specs, wsas, List.foldl addTyp ts typbnds, vs)
		   | Absyn.DATAspec(datbnds, typbnds, _) =>
		      let val ts = List.foldl addDat ts datbnds
			  val ts = List.foldl addTyp ts typbnds
		      in
				split(specs, wsas, ts, vs)
		      end
		   | Absyn.VALspec _ => split(specs, wsas, ts, spec::vs)
		   | Absyn.RELspec _ => split(specs, wsas, ts, spec::vs)
      in
		split(specs, [], [], [])
      end

    fun addVal(var, exp, vs) = ReorderVal.VALBND(var,exp) :: vs
    fun addRel(relbnd, vs) = ReorderVal.RELBND(relbnd) :: vs

    fun valDec(ReorderVal.VALDEC(var,exp)) = Absyn.VALdec(var, exp, Absyn.dummyInfo)
      | valDec(ReorderVal.RELDEC relbnds) = Absyn.RELdec (relbnds, Absyn.dummyInfo)

    fun reorderDecs(source, decs) =
      let fun split([], ws, ts, vs) =
		let val typDecs = map typDec (ReorderTy.reorderTyBnds(source, ts))
		    val valDecs = map valDec (ReorderVal.reorderValBnds(source, vs))
		in
		  ws @ typDecs @ valDecs
		end
	    | split(dec::decs, ws, ts, vs) =
		(case dec
		   of Absyn.WITHdec _ => split(decs, dec::ws, ts, vs)
		    | Absyn.TYPEdec(typbnds, _) =>
				split(decs, ws, List.foldl addTyp ts typbnds, vs)
		    | Absyn.DATAdec(datbnds, typbnds, _) =>
			let val ts = List.foldl addDat ts datbnds
			    val ts = List.foldl addTyp ts typbnds
			in
			  split(decs, ws, ts, vs)
			end
		    | Absyn.VALdec(var, exp, _) =>
				split(decs, ws, ts, addVal(var, exp, vs))
		    | Absyn.RELdec(relbnds, _) =>
				split(decs, ws, ts, List.foldl addRel vs relbnds))
      in
		split(decs, [], [], [])
      end

    fun reorderInterface(interface as Absyn.INTERFACE({modid, specs, source}, infoI)) =
	  if !Control.doReorder  
      then 
      let val specs = reorderSpecs(source, specs)
      in
		debug ("reorderInterface:"^(Absyn.identName modid)^"\n");
		Absyn.INTERFACE({modid=modid, specs=specs, source=source}, infoI)
      end
      else interface

    fun reorderModule(module as Absyn.MODULE(interface, decs, infoM)) =
	  if !Control.doReorder 
      then 
      let val Absyn.INTERFACE({source,modid,...}, infoI) = interface
      in
		debug ("reorderModule:"^(Absyn.identName modid)^"\n");
		Absyn.MODULE(reorderInterface interface, reorderDecs(source, decs), infoM)
      end
      else module

  end (* functor ReorderFn *)
