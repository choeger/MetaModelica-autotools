(* FrontEnd.sml *)

structure FrontEnd = FrontEndFn(
	structure Cache        = Cache
	structure Control      = Control
	structure Absyn        = Absyn
	structure RMLParse     = RMLParse 
	structure MODParse     = MODParse 
	structure StatElab     = StatElab
	structure AbsynPrint   = AbsynPrint
	structure AbsynPersist = AbsynPersist
	structure Util         = Util
	structure Reorder      = Reorder
	structure MOToRML      = MOToRML
	structure Instrument   = Instrument
);

(*


		   let val cachedModule = lookupCache(file, cache)
		   in
			case cachedModule of
				NONE => (* no module in cache, parse the file and add it to the cache *)
				let val m =
						(
						case Control.fileType file
						of Control.RML_FILE => RMLParse.parseInterface file
						| Control.MO_FILE  =>  
						(     
						if Control.isSerializedFileValid(file)
						then 
						(
						debug("annotate_with: reading serialization of : "^file^"\n");
						(* RML_INTERFACE_FILE *)
						(*
						RMLParse.parseInterface (Control.getFileName(file, Control.RML_MODULE_FILE))
						*)
						let val Absyn.PERSISTENT(_, SOME(module)) = 
								AbsynPersist.parseInterface(Control.getFileName(file, Control.SERIALIZATION_FILE))
						in
						  module
						end
						
						)
						else 
						(
						debug("annotate_with: parsing: "^file^"\n");  
						let val parsedMOModule = MODParse.parseModule file
							val interfaceFile = Control.getFileName(file, Control.INTERFACE_FILE)
							val serializationFile = Control.getFileName(file, Control.SERIALIZATION_FILE)
							in 
								debug("annotate_with: serializing: "^interfaceFile^"\n"); 
								withOutput AbsynPrint.printInterface parsedMOModule interfaceFile;								
								debug("StatElabFn.annotate_with: serializing: "^serializationFile^"\n"); 
								withOutput AbsynPersist.serializeModule parsedMOModule serializationFile;
								parsedMOModule
						    end
						)
						)
						| _ => uerror("annotate_with: unknown file type: "^file)
						)
				in				
					debug("annotate_with: caching :"^file^"\n");
					cache := CACHE(file, m)::(!cache);
					m
				end
			|   SOME(m) => 
				(
				debug("annotate_with: using cached: "^file^"\n");
				m
				)
		   end			

*)