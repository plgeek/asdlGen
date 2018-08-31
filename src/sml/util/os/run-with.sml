structure RunWith :> RUN_WITH =
  struct
    datatype ins =
      Tins of TextIO.instream
    | Bins of BinIO.instream 

    datatype outs =
      Touts  of TextIO.outstream
    | Bouts  of BinIO.outstream

    (* should work for Win32 and Unix all bets are off for MacOS *)

    fun get_in (Tins ins) = 
      let
	val tname = OS.FileSys.tmpName()
	val outs = TextIO.openOut tname
	val finalize = (fn () => OS.FileSys.remove tname)
	fun loop "" = TextIO.closeOut outs
	  | loop s = (TextIO.output(outs,s);
		      loop (TextIO.inputN(ins,512)))
      in
	loop (TextIO.inputN(ins,512));
	("<"^tname,finalize)
      end
    | get_in (Bins ins) = 
      let
	val tname = OS.FileSys.tmpName()
	val outs = BinIO.openOut tname
	val finalize = (fn () => OS.FileSys.remove tname)
	fun loop s =
	  if Word8Vector.length s = 0 then  BinIO.closeOut outs
	  else (BinIO.output(outs,s);loop (BinIO.inputN(ins,512)))
      in
	loop (BinIO.inputN(ins,512));
	("<"^tname,finalize)
      end

    fun get_out (Touts outs) = 
      let
	val tname = OS.FileSys.tmpName()
	fun finalize () =
	  let
	    val ins = TextIO.openIn tname
	    fun loop "" = TextIO.closeIn ins
	      | loop s = (TextIO.output(outs,s);
			  loop (TextIO.inputN(ins,512)))
	  in
	    loop (TextIO.inputN(ins,512));
	    OS.FileSys.remove tname
	  end
      in
	(">"^tname,finalize)
      end
    | get_out (Bouts outs) = 
      let
	val tname = OS.FileSys.tmpName()
	fun finalize () =
	  let
	    val ins = BinIO.openIn tname
	    fun loop s =
	      if Word8Vector.length s = 0 then BinIO.closeIn ins
	      else
		(BinIO.output(outs,s);
		 loop (BinIO.inputN(ins,512)))
	  in
	    loop (BinIO.inputN(ins,512));
	    OS.FileSys.remove tname
	  end
      in
	(">"^tname,finalize)
      end

    fun run_with (ins,outs) cmd =
      let
	val ins = Option.map get_in ins
	val outs = Option.map get_out outs
	val (tins,fins) = Option.getOpt(ins,("",(fn () => ())))
	val (touts,fouts) = Option.getOpt(outs,("",(fn () => ())))
	val cmd = String.concat [cmd," ",tins," ",touts]
	val _ = print ("#"^ cmd ^ "\n")
	val ret = OS.Process.system cmd 
      in
	fins();fouts();ret
      end
  end
