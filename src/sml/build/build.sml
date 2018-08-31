signature BUILD_WORLD =
  sig
    val docs  : unit -> bool
    val heaps : unit -> bool
    val c     : unit -> bool
    val cxx   : unit -> bool
    val all   : unit -> bool
    val clean : unit -> bool
    val install : unit -> bool
  end
functor BuildWorld(structure SML      : SML_BUILD
		   structure CC       : CC_BUILD
		   structure FO       : FILE_OPS_BUILD
		   val do_it          : SML.B.rule list -> bool
		   val debug          : bool
		   val src_root       : Paths.path
		   val install_root   : Paths.path
		   sharing SML.B = CC.B = FO.B) :> BUILD_WORLD =
  struct
    fun src_path arcs = Paths.pathConcat(src_root,Paths.pathFromArcs arcs)

    structure BU = BuildUtil(structure B = CC.B)

    structure MkC =
      BuildC(structure CC = CC
	     structure BU = BU
	     val include_dirs =[]
	     val debug = debug)

    structure MkCXX =
      BuildCXX(structure CC = CC
	       structure BU = BU
	       val debug = debug
	       val include_dirs = [Paths.dirFromPath (src_path ["c"])])

    structure MkSML =
      BuildSML(structure SML = SML
	       structure BU = BU
	       val debug = debug)

    structure MkDT =
      BuildDTangle(structure SML = SML
		   structure BU = BU
		   val debug = debug)

    structure MkDoc =
      BuildDoc(structure DT = MkDT
	       val dtangle_src = src_path ["sml","util","dtangle"]
	       val src_root = src_root)

    structure I =
      BuildInstall(structure BuildC = MkC
		   structure BuildCXX = MkCXX
		   structure BuildSML = MkSML
		   structure BuildDoc = MkDoc
		   structure FileOps = FO
		   val install_dir = install_root)
    structure Clean =
      BuildClean(structure BuildC = MkC
		 structure BuildCXX = MkCXX
		 structure BuildSML = MkSML
		 structure BuildDoc = MkDoc
		 structure FileOps = FO)

    fun wrap s r = do_it (#2(BU.getRules (src_path [s]) r))
    fun heaps () = wrap "sml" MkSML.rules
    fun c () = wrap "c" MkC.rules
    fun cxx () = wrap "cxx" MkCXX.rules
    fun docs () = do_it MkDoc.rules
    fun install () = (do_it I.rules)
    fun all x = (heaps x) (* andalso (c x) andalso (cxx x) *)
    fun clean () = do_it Clean.rules
  end

structure Build =
  struct
    fun do_it rules = MetaBuild.run  (MetaBuild.BUILD{name=NONE,rules=rules})
    fun do_it' rules =
      (MetaBuild.export TextIO.stdOut
      (MetaBuild.BUILD{name=NONE,rules=rules});
       true)
      
    structure Win32 =
      BuildWorld(structure SML = Win32SML
		 structure CC = Win32CC
		 structure FO = Win32FileOps
		 val do_it = do_it
		 val debug = true
		 val install_root =  Paths.pathFromNative "\\tmp\\asdlGen"
		 val src_root = Paths.pathFromNative "..")
    structure Win32E =
      BuildWorld(structure SML = Win32SML
		 structure CC = Win32CC
		 structure FO = Win32FileOps
		 val do_it = do_it'
		 val debug = true
		 val install_root =  Paths.pathFromNative "\\tmp\\asdlGen"
		 val src_root = Paths.pathFromNative "..")

    structure Unix =
      BuildWorld(structure SML = UnixSML
		 structure CC = UnixCC
		 structure FO = UnixFileOps
		 val do_it = do_it
		 val debug = true
		 val install_root =  Paths.pathFromNative "/tmp/asdlGen"
		 val src_root = Paths.pathFromNative "..")
    structure UnixE =
      BuildWorld(structure SML = UnixSML
		 structure CC = UnixCC
		 structure FO = UnixFileOps
		 val do_it = do_it'
		 val debug = true
		 val install_root =  Paths.pathFromNative "/tmp/asdlGen"
		 val src_root = Paths.pathFromNative "..")

    structure Config =
      BuildWorld(structure SML = ConfigSML
		 structure CC = ConfigCC
		 structure FO = ConfigFileOps
		 val do_it = do_it
		 val debug = true
		 val install_root =  Paths.pathFromNative ConfigPaths.prefix
		 val src_root = Paths.pathFromNative "..")

    structure ConfigE =
      BuildWorld(structure SML = ConfigSML
		 structure CC = ConfigCC
		 structure FO = ConfigFileOps
		 val do_it = do_it'
		 val debug = true
		 val install_root =  Paths.pathFromNative ConfigPaths.prefix
		 val src_root =
		   Paths.pathConcat
		   (Paths.pathFromNative ConfigPaths.top_srcdir,
		    Paths.pathFromNative "src"))
  end



