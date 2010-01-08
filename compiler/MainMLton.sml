structure MainMLton = 
struct
    fun main  () = 
    let
	val argz = CommandLine.arguments()
	val name = CommandLine.name()
    in  
	Start.start Main.main (name,argz)
    end
end
