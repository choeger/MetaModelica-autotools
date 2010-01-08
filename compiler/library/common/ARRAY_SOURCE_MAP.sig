(* SourceMap implements the map from character positions in a file to 
   pairs (line no,column no).
   
   Unlike the corresponding SML/NJ structure it is fairly free of features
   (but hopefully cheaper).
   *)
signature ARRAY_SOURCE_MAP =
sig
   type sourcemap

   val new: string*string->sourcemap 
   (* new(filename,date_str) creates a sourcemap for filename with date *)


   val newline:sourcemap*int->unit
   (* newline(s,i) indicates a new line character at character i 
      (characters are numbered from 0) of the file.  i should be non-negative
      and greater than all i's on previous calls with this s. 

      There is a hack added to work around a bug in ml-lex, which assumes
      the file has an additional newline added at the very start of the
      file.
      *)
   val decode:sourcemap*int->
     {line:int,column:int}
   (* decode returns the line and column number for this character position.
      Line and columns are numbered from 1. 
      The convention is that the EOF character occurs on line (last line + 1),
      column 1. *)
   val eof:sourcemap->int
   (* Finish off for file, returning the EOF position *)

   val getFileName          : sourcemap -> string
   val getSerializationDate : sourcemap -> string
   val getLines             : sourcemap -> int list
   val getCurrentLine       : sourcemap -> int   
   (* recreating a map from a file is done by:
      - saving the getFileName
      - saving the getLines 
      - saving the getSerializationTime
      - saying new (setLines(file, date, int_list))*)
   val getSourceMap:  string * string * int list * int -> sourcemap
end
