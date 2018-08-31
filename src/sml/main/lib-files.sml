structure LibFiles =
  struct
    fun mkroot s = Paths.pathConcat(Paths.pathFromNative
				    ConfigPaths.top_srcdir,
				    Paths.pathFromArcs ("src"::s))
    val add_files  =  List.foldl FileSet.addFile FileSet.empty
    val pkl_int_lib =
      FileSet.mkLib {name="pkl-int.h",depends=[],impl=
      MkFileSet.file_set (mkroot ["c"]) FileSet.empty
      [("pkl-int.c",["pkl-int.h"]),
       ("pkl-int.h",[])]}
    local 
      val share_lib =
      FileSet.mkLib {name="share.h",depends=[],impl=
		     MkFileSet.file_set (mkroot ["c"]) FileSet.empty
		     [("share.c",["share.h"]),
		      ("share.h",[])]}

    val std_prims_lib =
      FileSet.mkLib {name="StdPrims.h",depends=["pkl-int.h","share.h"],impl=
      MkFileSet.file_set (mkroot ["c"]) FileSet.empty
		     [("StdPrims.h",["pkl-int.h","StdPkl.h"]),
		      ("StdPkl.h",["share.h"]),
		      ("std_prims.c",["StdPrims.h","StdPkl.h"])]}
    val std_types_lib =
      FileSet.mkLib {name="StdTypes.h",depends=["pkl-int.h"],impl=
      MkFileSet.file_set (mkroot ["c"]) FileSet.empty
		     [("StdTypes.h",[])]}

    val xml_prims_lib =
      FileSet.mkLib {name="XMLPrims.h",depends=["pkl-int.h","share.h"],impl=
      MkFileSet.file_set (mkroot ["c"]) FileSet.empty
		     [("XMLPrims.h",["XMLPkl.h"]),
		      ("XMLPkl.h",["share.h"]),
		      ("xml_prims.c",["XMLPrims.h","XMLPkl.h"])]}
    in
      val c = add_files
	[share_lib,pkl_int_lib,xml_prims_lib,std_types_lib,std_prims_lib]
    end

    local 
      val std_prims_lib =
	FileSet.mkLib {name="StdPrims.hxx",depends=["pkl-int.h"],impl=
		       MkFileSet.file_set (mkroot ["cxx"]) FileSet.empty
		     [("StdPrims.hxx",["StdPkl.hxx"]),
		      ("StdPkl.hxx",["pkl-int.h"]),
		      ("std_prims.cxx",["StdPrims.hxx"])]}
    val std_types_lib =
      FileSet.mkLib {name="StdTypes.hxx",depends=["pkl-int.h"],impl=
      MkFileSet.file_set (mkroot ["cxx"]) FileSet.empty
		     [("StdTypes.hxx",["pkl-int.h"])]}
    in
      val cxx = add_files
	[pkl_int_lib,std_types_lib,std_prims_lib]
    end

    local
      fun join (x,y) = OS.Path.concat("asts",OS.Path.concat(x,y))

      val std_pkl_lib =
	FileSet.mkLib {name=join("StdPkl","g.java"),depends=[],impl=
        MkFileSet.file_set (mkroot ["java"]) FileSet.empty
	  [(join("StdPkl","g.java"),[]),
	   (join("StdPkl","PklJava.java"),[join("StdPkl","g.java")])]}

      val std_prims_lib =
	FileSet.mkLib {name=join("StdPrims","g.java"),
		    depends=[join("StdPkl","g.java")],impl=
        MkFileSet.file_set (mkroot ["java"]) FileSet.empty
	 [(join("StdPrims","g.java"),[join("StdPkl","g.java")]),
	  (join("StdPrims","identifier.java"),[join("StdPrims","g.java")])]}

      val std_types_lib =
	FileSet.mkLib {name=join("StdTypes","g.java"),
		    depends=[join("StdPkl","g.java")],impl=
        MkFileSet.file_set (mkroot ["java"]) FileSet.empty
	 [(join("StdTypes","g.java"),[])]}
    in val java = add_files [std_pkl_lib,std_prims_lib,std_types_lib]
    end
    local
      val sources = ["identifier.sig",
	  "identifier.sml", "pkl.sig", "pkl-int.sml", "std-pkl.sig",
	  "std-pkl.sml", "sexp-pkl.sig", "sexp-pkl.sml",
	  "sexp-lex.sig", "sexp-lex.sml", "std-prims.sig",
	  "std-prims.sml", "std-prims-util.sig", "std-prims-util.sml",
	  "std-types.sml", "share.sml"]

      val asdl_base_lib =
	  FileSet.mkLib {name="asdl-base.cm",depends=[],impl=
		       MkFileSet.file_set (mkroot ["sml","base"]) FileSet.empty
		       (("asdl-base.cm",sources)::
		       (List.map (fn x => (x,[])) sources))}
      val std_prims_lib =
	FileSet.mkLib {name="StdPrims.sml",depends=["asdl-base.cm"],
		      impl=FileSet.empty}
      val std_types_lib =
	FileSet.mkLib {name="StdTypes.sml",depends=["asdl-base.cm"],
		      impl=FileSet.empty}
    in
	val sml = add_files
	  [std_types_lib,std_prims_lib,asdl_base_lib]
    end

    local
      val sources = ["pklInt", "sexpPkl", "stdPkl",
		     "sexpLex", "share", "stdPrims"]

      val std_prims_lib =
       FileSet.mkLib {name="stdPrims.mli",depends=[],impl=
       MkFileSet.file_set (mkroot ["ocaml"]) FileSet.empty
		      (List.foldl (fn (x,xs) =>
				   (x^".ml",[x^".mli"])::(x^".mli",[])::xs)
		       [] sources)}
      val std_prims_util_lib =
	FileSet.mkLib  {name="stdPrimsUtil.mli",depends=["stdPrims.mli"],
	impl=MkFileSet.file_set (mkroot ["ocaml"]) FileSet.empty
        [("stdPrimsUtil.mli",[]),
	 ("stdPrimsUtil.ml",["stdPrimsUtil.mli","stdPrims.mli"])]}

      val std_types_lib =
	 FileSet.mkLib {name="stdTypes.mli",depends=["stdPrims.mli"],
		        impl=FileSet.empty}
      val std_types_util_lib =
	FileSet.mkLib  {name="stdTypesUtil.mli",depends=["stdTypes.mli"],
	impl=FileSet.empty}
    in
      val ocaml = add_files [std_types_lib,std_types_util_lib,
			     std_prims_lib,std_prims_util_lib]
    end
  
    local
      val sources = ["SexpLex.hs","StdPkl.hs","StdPrimsUtil.hs",
		     "SexpPkl.hs","StdPrims.hs","StdTypes.hs"]

      val std_prims_lib =
       FileSet.mkLib {name="StdPrims.hs",depends=[],impl=
       MkFileSet.file_set (mkroot ["haskell"]) FileSet.empty
		      (List.map (fn (x) => (x,[])) sources)}

      val std_types_lib =
	 FileSet.mkLib {name="StdTypes.hs",depends=["StdPrims.hs"],
		        impl=FileSet.empty}
    in
      val hs = add_files [std_types_lib,std_prims_lib]
    end
     local
       val std_prims_lib =
	 FileSet.mkLib {name="libasdl.icn",depends=[],impl=
			MkFileSet.file_set (mkroot ["icon"]) FileSet.empty
			[("libasdl.icn",[])]}
     in val icon = add_files [std_prims_lib]
     end

  end



