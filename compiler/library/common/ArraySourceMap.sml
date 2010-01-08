(* SourceMap implements the map from character positions in a file to 
   pairs (line no,column no).
   
   Unlike the corresponding SML/NJ structure it is fairly free of features
   (but hopefully cheaper).
   *)
structure ArraySourceMap:>ARRAY_SOURCE_MAP=
struct
   structure DArray=DynIntArray

   val line_offset=0 
   val column_offset=1
   (* Internally we think of lines and columns as being numbered from 0 
      (because that's how ML does arrays), but add these numbers to get the 
      output of decode. 

      HACK.  The toplevel function (in Parse.sml) adds a fictitious
      newline to the start of each file.  This is because at the moment,
      ml-lex starts yypos at 2 (but may later be changed to start it at
      some sensible number).  So we have an imaginary line 0.
      *)

   type sourcemap=int array ref (* the pos of \n as char on line [i] *)
                 *int ref       (* the current line *)
                 *string        (* the file *)
                 *string        (* the serialization date as string *)
   (* The int is the number of the current line.  arr[i] is the number of the
      newline character at the end of line i.  Here lines and characters are
      numbered from 0.
      *)

   fun new (filename, date_str) = (DArray.array 1000,
                                   ref 0,
                                   filename,
						           date_str)

   fun newline((arr,refl as ref l,_,_),pos)=
   let
      val ()=DArray.update(arr,l,pos)
      val ()= refl:= l+1
   in
      ()
   end
   
   fun decode((arr,refl as ref l,_,_),pos)=
   let
      (* find the least j in [0,l-1] such that arr[j]>=pos, or return l if all
         elements of arr[0:l-1] are <pos.  Then the internal line number is j
         and the internal column number is pos-arr[j-1]-1.
         *)
      fun bchop(lower,upper)=
      (* Define arr[l]>=pos.    
         Then this function is called when arr[lower]<pos<=arr[upper],0<=lower<upper and we
         want to find the least j with arr[j]>=pos and arr[j-1]. *)
      let
         val middle=Int.quot(lower+upper,2)
         val try=DArray.sub(arr,middle)
      in
         if try<pos
         then
            if middle=lower
            then
               (upper,try)
            else
               bchop(middle,upper)
         else
            bchop(lower,middle)
      end

      val (line_no,last_nl)=
         if l=0 orelse DArray.sub(arr,0)>=pos
         then
            (0,~1)
         else
            bchop(0,l)
   in
     {line=line_no+line_offset, column=(pos-last_nl)+(column_offset-1)}
   end

   val eof_pos=
     (case Int.maxInt of SOME x => x)

   fun eof(arr,refl as ref l,_,_)=
   let
      val ()=DArray.update(arr,l,eof_pos-1)
      val ()=refl:=(l+1)
   in
      eof_pos
   end
   

   fun getFileName ((arr,l,filename,date_str)) = filename
   fun getLines((arr,l,filename,date_str)) = DArray.toList(arr)
   fun getCurrentLine((arr,l,filename,date_str)) = !l
   fun getSerializationDate((arr,l,filename,date_str)) = date_str   
   fun getSourceMap(filename,date_str,intLst,curLine) = 
	   (DArray.fromList(intLst), ref curLine, filename, date_str)
	   
end


