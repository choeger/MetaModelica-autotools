(* AbsynToFOL.sml *)

structure AbsynToFOL =
  AbsynToFOLFn(structure StrDict = StrDict
	       structure Util = Util      
	       structure Absyn = Absyn
	       structure FOL = FOL
	       structure Instrument = Instrument);
