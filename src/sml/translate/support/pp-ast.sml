functor mkPPAst(structure Ast :
		  sig
		    structure TypeId   : SOURCE_ID
		    structure VarId    : SOURCE_ID
		    structure ModuleId : SOURCE_ID
		  end
		structure IdMap : ID_MAP
		  val cap_mod : bool
		  val cap_typ : bool
		  val sep : string) : PP_AST =
  struct
    open Ast
    type pp = PPUtil.pp

    structure Ht =
      HashTableFn(struct
	type hash_key = string
	val hashVal = HashString.hashString
	val sameKey = (op =): (hash_key * hash_key) -> bool
      end)

    structure TypeId = Ast.TypeId
    structure VarId = Ast.VarId
    structure ModuleId = Ast.ModuleId

    type path = {base:string,qualifier:string list}
    type ('a,'b) cvt = (('a -> path) * (path -> 'b))
    fun mk_tbl n l =
      let val tbl =
	Ht.mkTable(128,Error.error ["Lookup in table ",n])
      in List.app (Ht.insert tbl) l; tbl
      end
    (* more complex than need be *)
    fun fix_path tbl (arg as {base,qualifier}) =
      let
	fun get x =
	  (case (Ht.find tbl x) of
	     NONE => (x,false)
	   | (SOME x) => (x,true))
	     
	val (base,b_sub) = get base
	val (qualifier,q_sub) = List.foldr
	  (fn (x,(xs,ts)) =>
	   let val (x,t) = get x
	   in (x::xs,t orelse ts)
	   end) ([],b_sub) qualifier
      in
	if q_sub then SOME {base=base,qualifier=qualifier}
	else SOME arg
      end

			
    val id_tbl = mk_tbl IdMap.name IdMap.id_map
    val ty_tbl = mk_tbl IdMap.name IdMap.ty_map

    val id_fix = fix_path id_tbl
    val ty_fix = fix_path ty_tbl

    val cap = CvtCase.cvt_string CvtCase.capitalize
    val cap_ty_path =
      case (cap_mod,cap_typ) of
	(false,false) => (fn x => NONE)
      | (false,true) =>
	  (fn {base,qualifier} => 
	   SOME {base=base, qualifier=List.map cap qualifier})
      | (true,false) =>
	  (fn {base,qualifier} => 
	   SOME {base=base, qualifier=List.map cap qualifier})
      | (true,true) =>
	  (fn {base,qualifier} => 
	   SOME {base=cap base, qualifier=List.map cap qualifier})
    val fix_tid = (TypeId.subst cap_ty_path) o (TypeId.subst ty_fix)

    val cap_var_path =
      if cap_mod then (fn {base,qualifier}:VarId.path => 
		       SOME {base=base, qualifier=List.map cap qualifier})
      else (fn x => NONE)
    val fix_vid = (VarId.subst cap_var_path) o (VarId.subst id_fix)

    val cap_mod_path =
      if cap_mod then (fn {base,qualifier} => 
		       SOME {base=cap base, qualifier=List.map cap qualifier})
      else (fn x => NONE)

    val fix_mid = (ModuleId.subst cap_mod_path) o (ModuleId.subst id_fix)


    val tid = PPUtil.wrap ((TypeId.toString' sep) o fix_tid)
    val vid = PPUtil.wrap ((VarId.toString' sep) o fix_vid)
    val mid = PPUtil.wrap ((ModuleId.toString' sep) o fix_mid)

    val base_tid = PPUtil.wrap ((TypeId.getBase) o fix_tid)
    val base_vid = PPUtil.wrap ((VarId.getBase) o fix_vid)

    fun local_tid mid =
      let val q = ModuleId.getBase mid
	fun pp id =
	  if [q] = TypeId.getQualifier id then (base_tid id)
	  else (tid id)
      in pp
      end
    fun local_vid mid =
      let val q = ModuleId.getBase mid
	fun pp id =
	  if [q] = VarId.getQualifier id then (base_vid id)
	  else (vid id)
      in pp
      end
    val num = PPUtil.d
    val str = PPUtil.s
    val kw  = PPUtil.s
    val cat = PPUtil.cat

    val empty = PPUtil.empty
    val ws = PPUtil.ws
    val nl = PPUtil.nl
    fun vsep s = cat [nl,str s]
    fun hsep s = cat [str s,ws]

    fun vb i f b s =
      cat [f,PPUtil.vbox i [nl,b],nl,s]
    fun hb i f b s =
      cat [f,PPUtil.box i [b],s]

    fun seq sep fmt = PPUtil.seq{fmt=fmt,sep=sep}
    fun seq' sep fmt = PPUtil.seq_term{fmt=fmt,sep=sep}

    fun opt n s = PPUtil.opt{some=s,none=n}
    fun lst n f [] = n
      | lst n f x  = (f x)
  end