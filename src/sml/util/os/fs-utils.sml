structure FSUtils :> FS_UTILS =
  struct
    val tmpName = OS.FileSys.tmpName
    fun cmp(x,y) =
     ((((OS.FileSys.fileSize x) = (OS.FileSys.fileSize y)) andalso
      let val xin = BinIO.openIn x val yin = BinIO.openIn y
	fun loop (sx,sy) =
	  if ((Word8Vector.length sx = 0) orelse
	     (Word8Vector.length sy) = 0) then true
	  else ((sx = sy) andalso
		(loop(BinIO.inputN(xin,4*1024),BinIO.inputN(yin,4*1024))))
	val ans = (loop(BinIO.inputN(xin,4*1024),BinIO.inputN(yin,4*1024))
	   handle _ => false)
      in BinIO.closeIn xin; BinIO.closeIn yin; ans
      end) handle _ => false)
    fun rm s = OS.FileSys.remove s
    fun mv (x,y) =
      (OS.FileSys.rename {old=x,new=y} handle OS.SysErr _ =>
	let val xin = BinIO.openIn x val yout = BinIO.openOut y
	  fun loop (sx) =
	    if Word8Vector.length sx = 0 then ()
	    else (BinIO.output(yout,sx);
		  loop(BinIO.inputN(xin,4*1024)))
	in (loop(BinIO.inputN(xin,4*1024)) handle _ => ());
	  BinIO.closeIn xin; BinIO.closeOut yout; rm x
	end)
  end
