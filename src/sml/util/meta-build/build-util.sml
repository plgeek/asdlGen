signature BUILD_UTIL =
  sig
    structure B : CORE_BUILD
    type 'a M
    val mUnit     : 'a -> 'a M
    val mBind     : 'a M * ('a -> 'b M) -> 'b M
    val mBindList : 'a M list * ('a list -> 'b M) -> 'b M

    val mkRule   : ('a -> ('b * B.rule list)) -> 'a M -> 'b M
    val mkRule'  : (((string -> Paths.file_path) * 'a) -> ('b * B.rule list))
                    -> 'a M -> 'b M
    val getDir   : 'a M * (('a *  Paths.dir_path) -> 'b M) -> 'b M

    val mkInput  : (Paths.file_path -> 'a) -> string -> 'a M
    val getRules : Paths.path -> 'a M -> ('a * B.rule list)
  end
functor BuildUtil(structure B : CORE_BUILD) : BUILD_UTIL =
  struct
    structure B = B
    type 'a M = {root:Paths.path,
		rules:B.rule list ref} -> 'a
	    
    fun mUnit v st = v
    fun mBind (m:'a M,f) st = ((f (m st)) st)
    fun mBindList (ms,f) st =
      let val a = List.map (fn x => x st) ms
      in (f a) st
      end

    fun mkRule f m (st as {root,rules}) =
      let
	val (v,rl) = f (m st)
      in rules := rl@(!rules);v
      end
    fun mkRule' f m (st as {root,rules}) =
      let
	fun mk_abs root arcs =
	  Paths.pathConcat(root,Paths.pathFromArcs arcs)
	fun mk_file x = (Paths.fileFromPath (mk_abs root [x]))
	val (v,rl) = f (mk_file,(m st))
      in rules := rl@(!rules);v
      end
    fun getDir (m:'a M,f) (st as {root,rules}) =
      let val v = m st
      in (f(v,Paths.dirFromPath root)) st
      end
    fun mkInput f s (st as {root,rules}) =
      let
	fun mk_abs root arcs =
	  Paths.pathConcat(root,Paths.pathFromArcs arcs)
	fun mk_file x = f (Paths.fileFromPath (mk_abs root [x]))
      in mk_file s
      end
    fun getRules p m =
      let val rules = ref []
	val v = m ({root=p,rules=rules})
      in (v,!rules)
      end
  end