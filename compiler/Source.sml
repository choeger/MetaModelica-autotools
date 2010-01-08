(* util/source.sml *)

structure Source : SOURCE =
  struct

	structure ArraySourceMap = ArraySourceMap
	
    datatype source
      = SOURCE of ArraySourceMap.sourcemap ref (* _descending_ order *) 
      
    fun getCurrentDate() = Date.toString(Date.fromTimeLocal(Time.now()))
    
    val dummy = SOURCE(ref(ArraySourceMap.new("",getCurrentDate())))

	val debugFlag = false
	fun debug s = if (debugFlag) then Util.outStdErr ("Source."^s) else ()	

    (* The pos of an imaginary newline before a file's very
     * first character. This is necessary to adjust for the
     * weird notion of ML-Lex that the first character has
     * position 2. Not 0 or 1, but 2.
     * XXX: THIS WILL BREAK IF ML-LEX IS FIXED
     *)
    val startPos:int = 1

    fun lookup(sourcemap, pos) = 
    let val {line,column} = ArraySourceMap.decode(sourcemap, pos)
    in
      {line=line+1,column=column}
    end
    	
    fun sayErr s = TextIO.output(TextIO.stdErr, s)
    fun sayErr1 c = TextIO.output1(TextIO.stdErr, c)

    fun sayFile file = (sayErr file; sayErr1 #":")

    fun sayPos(sourcemap, pos) =
      let val {line,column} = lookup(sourcemap, pos)
      in
		sayErr(Int.toString line);
		sayErr1 #".";
		sayErr(Int.toString column)
      end
      
    fun sayMsg (SOURCE(ref(sourcemap))) (msg,leftPos,rightPos) =
      (sayFile (ArraySourceMap.getFileName(sourcemap));
       sayPos(sourcemap, leftPos);
       sayErr1 #"-";
       sayPos(sourcemap, rightPos);
       sayErr1 #" ";
       sayErr msg; sayErr1 #"\n")
       
    fun getLC({line, column}) = (line, column)
       
    fun getLoc((SOURCE(ref(sourcemap))), spos, epos) =
	let val (sline, scolumn) = getLC(lookup(sourcemap, spos))
		val (eline, ecolumn) = getLC(lookup(sourcemap, epos))
	in
	{
	fileName = ArraySourceMap.getFileName(sourcemap),
	sline = sline, 
	scolumn = scolumn, 
	eline = eline, 
	ecolumn = ecolumn
	}
	end
	
    fun getFileName (SOURCE(ref(sourcemap))) = 
			ArraySourceMap.getFileName(sourcemap)		    
    fun getSerializationDate (SOURCE(ref(sourcemap))) = 
			ArraySourceMap.getSerializationDate(sourcemap)		    
    fun getLines (SOURCE(ref(sourcemap))) = 
			ArraySourceMap.getLines(sourcemap)
			
    fun getCurrentLine (SOURCE(ref(sourcemap))) = 
			ArraySourceMap.getCurrentLine(sourcemap)
			
    fun getSource (filename, date_str, int_list, curLine) = 
		SOURCE(ref(ArraySourceMap.getSourceMap(filename, date_str, int_list, curLine)))

  end (* structure Source *)
