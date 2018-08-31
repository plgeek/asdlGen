structure DefaultDecls =
  struct
    structure T = Asdl
    val std_prims = [
     {file="<builtin>",
      decl=T.PrimitiveModule
      {name=Identifier.fromString "StdPrims",
       exports=List.map Identifier.fromString
       ["int","string","identifier","big_int"]}}]

    fun mk_vd (e,p,v) = {entity=List.map Identifier.fromString e,prop=p,value=v}
    val std_views = [{file="<bultin>",decl=T.View
		      {name=Identifier.fromString "Java",
		       decls=[mk_vd (["StdPrims","int"],
				     "source_name","StdPrims.java_int"),
			mk_vd (["StdPrims","big_int"],
			       "source_name",
			       "StdPrims.java_math_BigInteger"),
			mk_vd (["StdPrims","string"],
			       "source_name",
			       "StdPrims.java_lang_String")]}},

		     {file="<bultin>",
		      decl=T.View
		      {name=Identifier.fromString "Icon",
		       decls=
		       [mk_vd (["StdPrims"], "source_name","libasdl")]}},

		     {file="<bultin>",
		      decl=T.View
		      {name=Identifier.fromString "OCaml",
		       decls=
		       [mk_vd (["StdPrims","int"],
				       "source_name","StdPrims.std_int"),
			mk_vd (["StdPrims","string"],
			       "source_name","StdPrims.std_string")]}}]
  end