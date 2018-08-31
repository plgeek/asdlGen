structure Win32BuildParams =
  struct
    structure B = MetaBuild
    val run_cm  =
      B.mkVAR{name=SOME "SML_CM",
	      doc=["SML with compilation manager"],
	      init=B.STR "sml-cm"}
    val heap_suffix = (Compiler.architecture)^"-win32"
    val run_heap  =
      B.mkVAR{name=SOME "RUN_SML_HEAP",
	      doc=["Run ML heap"],
	      init=B.STR ("run."^heap_suffix)}

    fun heap_arg x = "@SMLload="^x

    val comp  =
      B.mkVAR{name=SOME "CC",
	      doc=["Win32 C  compiler"],
	      init=B.STR "cl"}
    val comp_cxx =
      B.mkVAR{name=SOME "CXX",
	      doc=["Win32 C++  compiler"],
	      init=B.STR "cl"}

    val link  =
      B.mkVAR{name=SOME "LD",
	      doc=["Win32 C linker"],
	      init=B.STR "link"}

    val mklib =
      B.mkVAR{name=SOME "MKLIB",
	      doc=["Win32 Library Archiver"],
	      init=B.STR "lib"}

    val comp_args = [B.STR "/nologo",B.STR "/c",B.STR "/Za"]
    val link_args = [B.STR "/nologo"]
    val mklib_args = [B.STR "/nologo"]

    fun debugFlag _ = B.STR ""

    fun optFlag _ = B.STR ("/O2")
    fun incFlag x = B.STR ("/I"^x)
    fun libFlag x = B.STR ("/L"^x)
    fun defFlag (x,NONE) = B.STR ("/D"^x)
      | defFlag (x,SOME y) = B.STR ("/D"^x^"="^y)
    fun linkOutFlag x = B.STR ("/Fe"^x)
    fun compOutFlag x = B.STR ("/Fo"^x)
    fun mklibOutFlag x = B.STR ("/OUT:"^x)
    val lib_suffix = SOME "lib"
    val obj_suffix = SOME "obj"
    val exe_suffix = SOME "exe"

    val copy =
      B.mkVAR{name=SOME "COPY",
	      doc=["Win32 copy command"],
	      init=B.STR "copy"}
    val delete =
      B.mkVAR{name=SOME "DELETE",
	      doc=["Win32 delete command"],
	      init=B.STR "del"}
    val delete_force_arg = "/f"
    val mkDir =
      B.mkVAR{name=SOME "MKDIR",
	      doc=["Win32 make directory"],
	      init=B.STR "mkdir"}
    val existsDir =
      B.mkVAR{name=SOME "TESTDIR",
	      doc=["Win32 test directory exists"],
	      init=B.STR "cd"}
    val attrb_set =
      B.mkVAR{name=SOME "SETATTRB",
	      doc=["Win32 set file attributes"],
	      init=B.STR "attrib"}
    fun attrbs2str {r,w=false,x} = "+R"
      | attrbs2str {r,w=true,x} = "-R"
      

  end

structure Win32CC = CCBuild(structure Params = Win32BuildParams)
structure Win32SML = SMLBuild(structure Params = Win32BuildParams)
structure Win32FileOps = FileOpsBuild(structure Params = Win32BuildParams)