structure FromOO :>
  sig
    datatype class_hierarchy =
      Abstract of string * field list * class_hierarchy list
    | Concrete of string * field list * class_hierarchy list
    withtype field =
      {name:string,typ:string,tycon:Asdl.tycon option}
    val fromOO : string -> class_hierarchy list -> Asdl.decl list
    val doit :  string -> class_hierarchy list -> unit
  end =
struct
    datatype class_hierarchy =
      Abstract of string * field list * class_hierarchy list
    | Concrete of string * field list * class_hierarchy list
    withtype field =
      {name:string,typ:string,tycon:Asdl.tycon option}

    structure A = Asdl

    val name2id = Identifier.fromString 
    val upper = String.map Char.toUpper
    val lower = String.map Char.toLower
    fun con_name x y = Identifier.fromString (upper (x^y))
    fun typ_name x = Identifier.fromString (lower x)

    fun transOO inherited_fds (Abstract(n,fds,cls),decls) =
      let
	val fds = inherited_fds@(trans_fields fds)
	val cons = (trans_cons n fds cls)
	val decls = List.foldl (transOO fds) decls cls
      in  case cons of
	    [] => (Error.warn ["Ignoring abstract class ",n,
			      " which has no children."];decls)
	  | (c::cs) =>
	      (A.SumType {name=typ_name n,attribs=[],c=c,cs=cs})::decls
      end
      | transOO inherited_fds (Concrete(n,fds,cls),decls) =
      let
	val fds = inherited_fds@(trans_fields fds)
	val c = {name=con_name "" n,fs=fds}
	val cs = (trans_cons n fds cls)
	val decls = List.foldl (transOO fds) decls cls
	val tname = typ_name n
      in (A.SumType {name=tname,attribs=[],c=c,cs=cs})::decls
      end
    and trans_field {name,typ,tycon} = (* TODO handle aggergates like lists *)
      let
	val tname = typ_name typ
	val lbl = name2id name
	val path = {qualifier=NONE,base=tname}
      in {typ=path,label_opt=SOME lbl,tycon_opt=tycon}
      end
    and trans_fields fds = List.map trans_field fds
    and trans_con prefix fds (Concrete(n,_,_)) = 
      {name=con_name prefix n,
       fs=[{typ={qualifier=NONE,base=typ_name n},
	    label_opt=NONE, tycon_opt=NONE}]}
      | trans_con prefix fds (Abstract(n,_,_)) =
       {name=con_name prefix n,
	fs=[{typ={qualifier=NONE,base=typ_name n},
	     label_opt=NONE,tycon_opt=NONE}]}
    and trans_cons prefix fds cls =
      List.map (trans_con prefix fds) cls
    fun fromOO s d =
      let
	val decls = List.foldl (fn (ch,decls) => (transOO [] (ch,decls))) [] d
      in [A.Module{name=name2id s,imports=[],decls=decls}]
      end
    fun doit s decls =
      let val decls = fromOO s decls
	  val pp = PPUtil.seq_term{fmt=PPASDL.pp_decl,sep=PPUtil.nl} decls
      in PPUtil.pp_to_outstream TextIO.stdOut 72 pp
      end
end
structure FromOOTest =
  struct
    open FromOO
    val ch =
      Abstract("obj",[],
	       [Concrete("A",[{name="x",typ="int",tycon=NONE}],
			 [Concrete("B",[{name="y",typ="int",tycon=NONE}],[]),
			  Concrete("C",[{name="z",typ="int",tycon=NONE}],[])]),
		Concrete("D",[{name="w",typ="int",tycon=NONE}],[])])
  end
