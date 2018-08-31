(* quick and dirty C #include dependeincy analysis *)

structure CCDepends =
  struct
    datatype inc =
      Known   of Paths.file_path
    | Unknown of Paths.file_path
    type depends = Paths.file_path * inc list list
    structure S = TextIO.StreamIO

    fun IsLJunk #"\"" = true
      | IsLJunk #"<" = true
      | IsLJunk x = Char.isSpace x

    fun IsRJunk #"\"" = true
      | IsRJunk #">" = true
      | IsRJunk x = Char.isSpace x

    fun IsPathSep #"/" = true
      | IsPathSep x = false

   fun parseIncs resolve (("",_),acc) = acc
     | parseIncs resolve ((str,s),acc) =
      if (String.isPrefix "#include" str) then
	let
	  val ss = Substring.extract(str,8,NONE)
	  val ss = Substring.dropl IsLJunk ss
	  val ss = Substring.dropr IsRJunk ss
	  val toks = Substring.tokens IsPathSep ss
	  val arcs = List.map Substring.string toks
	  val fp = resolve (Paths.fileFromPath (Paths.pathFromArcs arcs))
	in
	  parseIncs resolve (S.inputLine s,fp::acc)
	end
      else parseIncs resolve (S.inputLine s,acc)

    fun parseFile fp =
      let
	val s = (TextIO.openIn (Paths.fileToNative fp))
	val ret = (fp,parseIncs Known (S.inputLine (TextIO.getInstream s),[]))
      in
	TextIO.closeIn s;
	ret
      end

    fun parseFiles x = List.map parseFile x
    val sayp  = (fn () => print " ") o print o Paths.fileToNative
    fun printDeps (x,xs) =
      (sayp x ;print ":"; List.app (fn Known x => sayp x) xs)
  end