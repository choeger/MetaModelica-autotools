(* StatElab.sml *)


structure StatElab = StatElabFn(
	structure Util = Util
    structure StatObj = StatObj
	structure Control = Control
	structure Instrument = Instrument
	structure AbsynPrint = AbsynPrint
	structure AbsynPersist = AbsynPersist
	structure Cache = Cache
);

