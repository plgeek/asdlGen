(* A Pretty Printer, based on Philip Wadler's 'A prettier printer'. 
   http://cm.bell-labs.com/cm/cs/who/wadler/topics/recent.html
   by Ken Larsen <kla@it.dtu.dk>

   This code is released under GNU LGPL version 2 or any later after
   your choice, the licence can be obtained at
   http://www.gnu.org/copyleft/lgpl.html

   Modified by Daniel Wang <danwang@cs.princeton.edu> to support
   hard breaks as well as using TextIO.outstream as output.
*)
signature  WPP =
  sig
    type doc
    val empty : doc
    val line  : doc
    val nl    : doc  (* force a new line *)
   
    val group : doc -> doc
    val nest  : int -> doc -> doc
    val text  : string -> doc
    val ^^ : doc * doc -> doc

    val pretty    : int -> doc -> TextIO.outstream -> unit
    val prettyStr : int -> doc -> string 
  end

structure Wpp :> WPP =
struct
  
    infixr 6 :<>
    infixr 6 ^^

    datatype doc = 
	NIL
      | :<>   of doc * doc
      | NEST  of int * doc
      | TEXT  of string
      | BREAK 
      | GROUP of doc

    open SMLofNJ.Susp

	
    datatype Doc = 
      Nil
    | Text of string * Doc susp
    | Line of int * Doc susp

    fun copy i c =  Byte.bytesToString
      (Word8Vector.tabulate(i,(fn _ => Byte.charToByte c)))
      
    val empty    = NIL
    val op^^     = op:<>
    fun nest i x = NEST(i,x)
    val text     = TEXT 
    val nl       = TEXT "\n"
    val line     = BREAK 
    fun group x	 = GROUP x

      
    local
      (* preallocate some text *)
      val pad_2 = copy 2 #" "
      val pad_4 = copy 4 #" "
      val pad_8 = copy 8 #" "
      val pad_16 = copy 16 #" "
      val pad_32 = copy 32 #" "
      val pad_64 = copy 64 #" "
    in
      fun layout Nil outs	  = ()
	| layout (Text(s,x)) outs = (outs s;layout (force x) outs)
	| layout (Line(i,x)) outs =
	let
	  fun loop 0 = ()
	    | loop n =
	    let fun out s = (outs s;loop (n-(String.size s)))
	    in if n < 0 then ()
	       else (case (n mod 64) of
		     0 =>  out pad_64
		   | 2 => out pad_2
		   | 4 => out pad_4
		   | 8 => out pad_8
		   | 16 => out pad_16
		   | 32 => out pad_32
		   | r => out (copy r #" "))
	    end
      in
	outs "\n";loop i;
	layout (force x) outs
      end
    end

    fun newline i = StringCvt.padRight #" " (i+1) "\n"
   
    fun fits w x =
        if w < 0 then false
        else (case x of
                  Text(s,x) => fits (w - size s) (force x)
		| _         => true)
                
    local
	datatype mode = F | B

	fun be w k []                      = Nil
	  | be w k ((i,_,NIL)::z)          = be w k z
	  | be w k ((i,mode,x :<> y)::z)   = be w k ((i,mode,x)::(i,mode,y)::z)
	  | be w k ((i,mode,NEST(j,x))::z) = be w k ((i+j,mode,x)::z)
	  | be w k ((i,_,TEXT "\n")::z)    = 
	    Line (i, delay (fn()=> be w i z))
	  | be w k ((i,_,TEXT s)::z)       =
	    Text (s, delay(fn()=> be w (k + size s) z))
	  | be w k ((i,F,BREAK )::z)        = 
	    Text (" ", delay(fn()=> be w (k + 1) z))
	  | be w k ((i,B,BREAK)::z)        = 
	    Line (i, delay (fn()=> be w i z))
	  | be w k ((i,F,GROUP x)::z)      = be w k ((i,F,x)::z)
	  | be w k ((i,B,GROUP x)::z)      = 
	    let val flat = be w k ((i,F,x)::z)
	    in  if fits (w-k) flat then flat
		else be w k ((i,B,x)::z)
	    end
    in
	fun best w x = be w 0 [(0,B,x)]
    end

    fun pretty w x outstream =
      let fun outs s = TextIO.output(outstream,s)
      in (layout (best w x) outs; (outs "\n"))
      end
    fun prettyStr w x =
      let val str = ref []
	fun outs s = (str := (s::(!str)))
	val _ = layout (best w x) outs
	val _ = outs "\n"
      in String.concat (List.rev (!str))
      end
end


