structure UnixBuildParams =
  struct
    structure B = MetaBuild
    val run_cm  =
      B.mkVAR{name=SOME "SML_CM",
	      doc=["SML with compilation manager"],
	      init=B.STR "sml-cm"}
    val run_heap  =
      B.mkVAR{name=SOME "RUN_SML_HEAP",
	      doc=["Run ML heap"],
	      init=B.STR "sml"}
    (* this is unreliable *)
    val os = String.map Char.toLower (SMLofNJ.SysInfo.getOSName ())
    val heap_suffix = (Compiler.architecture)^"-"^os
    fun heap_arg x = "@SMLload="^x
    val comp  =
      B.mkVAR{name=SOME "CC",
	      doc=["Unix C compiler"],
	      init=B.STR "gcc"}
    val comp_cxx  =
      B.mkVAR{name=SOME "CXX",
	      doc=["Unix C compiler"],
	      init=B.STR "g++"}

    val link  =
      B.mkVAR{name=SOME "LD",
	      doc=["Unix C linker"],
	      init=B.STR "gcc"}

    val mklib =
      B.mkVAR{name=SOME "MKLIB",
	      doc=["Unix Library Archiver"],
	      init=B.STR "ar"}

    val comp_args = [B.STR "-c"]
    val link_args = []
    val mklib_args = [B.STR "crs"]

    fun debugFlag true  = B.STR "-g"
      | debugFlag false = B.STR ""

    fun optFlag x = B.STR ("-O"^(Int.toString x))
    fun incFlag x = B.STR ("-I"^x)
    fun libFlag x = B.STR ("-L"^x)
    fun defFlag (x,NONE) = B.STR ("-D"^x)
      | defFlag (x,SOME y) = B.STR ("-D"^x^"="^y)

    fun linkOutFlag x = B.STR ("-o "^x)
    fun compOutFlag x = B.STR ("-o "^x)
    fun mklibOutFlag x = B.STR (x)

    val lib_suffix = SOME "a"
    val obj_suffix = SOME "o"
    val exe_suffix = NONE

    val copy =
      B.mkVAR{name=SOME "COPY",
	      doc=["Unix copy command"],
	      init=B.STR "cp"}
    val delete =
      B.mkVAR{name=SOME "DELETE",
	      doc=["Unix delete command"],
	      init=B.STR "rm"}
    val delete_force_arg = "-f"
    val mkDir =
      B.mkVAR{name=SOME "MKDIR",
	      doc=["Unix make directory"],
	      init=B.STR "mkdir"}
    val existsDir =
      B.mkVAR{name=SOME "TESTDIR",
	      doc=["Unix test directory exists"],
	      init=B.STR "test -d"}
    val attrb_set =
      B.mkVAR{name=SOME "SETATTRB",
	      doc=["Unix set file attributes"],
	      init=B.STR "chmod"}
    fun attrbs2str {r,w,x} =
      let
	fun cif (true,x,xs) = x^xs
	  | cif (false,x,xs) = xs
      in "a="^cif(r,"r",cif(w,"w",cif(x,"x","")))
      end

  end
structure UnixSML = SMLBuild(structure Params = UnixBuildParams)
structure UnixCC = CCBuild(structure Params = UnixBuildParams)
structure UnixFileOps = FileOpsBuild(structure Params = UnixBuildParams)
