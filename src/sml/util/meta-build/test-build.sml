
functor TestBuild(structure CC : CC_BUILD) =
  struct
    val topath = Paths.fileFromPath o Paths.pathFromNative
    val tosrc = CC.mk_src_file o topath
    val lib_srcs = List.map tosrc ["lib/l1.c","lib/l2.c"]
    val main_srcs = [tosrc "test1.c"]
    val out_exe = topath "test"
    val out_lib = topath "testlib"

				    
    val comp_env =
      CC.mk_comp_env {ipath=[],debug=true,opt=1,
		      defines=[("NDEBUG",NONE),
			       ("SIZEOFINT",SOME "4")]}
    val link_env =
      CC.mk_link_env {lpath=[],static=true}

    val (main_objs,compile_main) =
      CC.comp_srcs{cenv=comp_env,srcs=main_srcs}

    val (lib_objs,compile_lib) =
      CC.comp_srcs{cenv=comp_env,srcs=lib_srcs}

    val (lib,make_lib) =
      CC.make_lib{name=out_lib,static=true,objs=lib_objs}

    val (exe,make_exe) =
      CC.link_objs{name=out_exe,lenv=link_env,objs=main_objs,libs=[lib]}
    val build = CC.B.BUILD {name=NONE,
			    rules=(make_lib@
				   compile_main@
				   make_exe@
				   compile_lib@
				   make_lib)}
    val build' = CC.B.BUILD {name=NONE,
			     rules=[CC.B.RULE{valid=CC.B.INVALID,
					      update=build}]}
  end
structure TestUnix = TestBuild(structure CC = UnixCC)
structure TestWin32 = TestBuild(structure CC = Win32CC)
