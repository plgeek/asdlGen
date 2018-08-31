signature RUNNABLE_BUILD =
  sig
    include CORE_BUILD
    val setVAR : var -> string -> unit 
    val run    : unit cmd -> bool
      end
structure BuildRun :> RUNNABLE_BUILD =
  struct
    datatype 'a ret = OK of 'a | FAIL 
    type 'a cmd = unit -> 'a ret
    type var = (int * {name:string option,
		       doc:string list,r:string cmd ref})
    type valid = (unit cmd * {targets:string list,depends:string list})
    type rule =  (unit cmd * {targets:string list,depends:string list})

    local
      val nextid = ref 0
    in
      fun getid () = (nextid := (!nextid) +1; !nextid)
    end
    fun mkVAR {name,doc,init} = (getid(),{r=ref init,name=name,doc=doc})
    fun dprint x = (print (x^"\n"))
    fun setVAR ((_,{r,...}):var) v = (r := (fn () => (OK v)))
    fun VAR ((_,{r,...}):var) = (fn () => (!r)())
    fun getString f =
      case f () of
	OK x => x
      | FAIL => ""

    fun CONCAT c =
      (fn () => OK(List.foldl (op ^) "" (List.map getString c)))
    fun GUARD c {OK=t,FAIL=f} =
      (fn () =>
       case (c ()) of (OK _) => t()
     | FAIL => f ())

    fun OR cs =
      let
      fun loop (x::xs,res) =
	(case (x()) of
	   FAIL => loop (xs,FAIL)
	 | x => OK ())
	| loop ([],res) = res
      in
	(fn () => loop (cs,FAIL))
      end
    fun AND cs =
      let
      fun loop (x::xs,res) =
	(case (x() ) of
	   (OK x) => loop (xs,OK ())
	 | FAIL => FAIL)
	| loop ([],res) = res
      in
	(fn () => loop (cs,OK ()))
      end
    fun SEQ cs =
      let
	fun loop (x::xs,res) =
	  (case (x()) of
	     OK _ => loop(xs,OK ())
	   | FAIL => loop(xs,FAIL))
	  | loop ([],res) = res
      in
	(fn () => loop (cs,OK ()))
      end

    fun STR x = (fn () => OK x)
    fun INT x = (fn () => OK x)
    fun IGN x = (fn () => (x();OK()))
    fun WRITE x =
      let val s = (getString x)^"\n"
      in (fn () =>
	  OK(TextIO.output(TextIO.stdOut,s)))
      end
    fun EXIT x =
      let val s = (getString x)^"\n"
      in (fn () =>
	  (TextIO.output(TextIO.stdErr,s);
	   raise (Fail s); FAIL))
      end

    fun join s = List.foldr (fn (x,xs) => x^" "^xs) "" s
    fun EXEC(x,xs) =
      (fn () =>
       let
	 val cmd = (join (List.map getString ((VAR x)::xs)))
       in
	 if ((OS.Process.system cmd) = OS.Process.success) then
	   (dprint ("OK:"^cmd);OK ())
	 else (dprint ("FAIL:"^cmd);FAIL)
       end)
    fun EXEC_WITH_INPUT (x,xs,inp) =
      (fn () =>
       let
	 val s = (getString inp)
	 val ins = TextIO.openString  s
	 val cmd = (join (List.map getString ((VAR x)::xs)))
	 val run =  RunWith.run_with (SOME (RunWith.Tins ins),NONE) 
       in
	 dprint ("INP:"^s);
	 if (run cmd = OS.Process.success) then
	   (dprint ("OK:"^cmd) ;OK ())
	 else (dprint ("FAIL:"^cmd);FAIL)
       end)
    val  INVALID  = ((fn () => FAIL),{targets=[],depends=[]})
    fun VALIDATE {targets,depends} =
      let
	val mtime = OS.FileSys.modTime o Paths.fileToNative
	fun maxtime (x,y) =
	  if (Time.<(x,y)) then y else x
	fun mintime (x,y) =
	  if (Time.<(x,y)) then x else y
	fun newer (x,y) = Time.>=(x,y) 
	  	  
	fun do_it () =
	  let
	    val tmin =
	      ((List.foldl
		(fn (x,SOME xs) => SOME(mintime (mtime x,xs))
                  | (x,NONE) => (SOME (mtime x))) NONE targets)
	       handle (OS.SysErr _ )=>  NONE)
	    val tmin = Option.getOpt(tmin,Time.zeroTime)
	    val dmax = List.foldl
	      (fn (x,xs) => (maxtime (mtime x,xs))) Time.zeroTime depends
	  in
	    if (newer(tmin,dmax)) then OK () else FAIL
	  end 
      in
	(do_it,{targets=List.map Paths.fileToNative targets,
		depends=List.map Paths.fileToNative depends})
      end

    fun RULE {valid=(test,deps),update} =
      ((fn () => (case (test ()) of
		   (OK _ ) => OK ()
		 | FAIL =>
		     (case (update()) of
			(OK _) => OK ()
		      | FAIL => (EXIT (STR "rule error"))()))),deps)
    local
      val not_found = Fail "Not found"
      structure H = AtomTable
      structure N =
	struct
	  datatype node =
	    Root
	  | Rule of (unit cmd * Atom.atom list * int)
	  type ord_key = node
	  fun compare (Root,Root) = EQUAL
	    | compare (Root,_) = LESS
	    | compare (_,Root) = GREATER
	    | compare (Rule(_,_,x),Rule(_,_,y)) = Int.compare (x,y)
	  fun rules2graph rules =
	    let
	      val ht = H.mkTable(128,not_found)
	      fun dorule ((c,{targets,depends}),(i,nodes)) =
		let
		  val ts = List.map Atom.atom targets
		  val ds = List.map Atom.atom depends
		  val node = Rule(c,ds,i)

		in
		    (* todo check for uniqueness *)
		    List.app (fn t => H.insert ht (t,node)) ts;
		    (i+1,node::nodes)
		end
	      val (_,nodes) = List.foldl dorule (0,[]) rules
	      (* todo warn about missing targets *)

	      fun follow (Root) = nodes
		| follow (Rule(c,ds,i)) = List.mapPartial (H.find ht) ds
	    in
	      ((H.find ht) o Atom.atom,follow)
	    end
	end
      structure Scc = SCCUtilFun(structure Node = N)
    in
      
    fun BUILD {name,rules} =
      let
	val (gn,follow) = N.rules2graph rules
	val tsorted =  Scc.topOrder{root=N.Root,follow=follow}
	fun noRec (Scc.SIMPLE (N.Rule(c,_,_)),cs) = c::cs
	  | noRec (Scc.SIMPLE _,cs) = cs
	  | noRec (Scc.RECURSIVE _,_) = raise Fail "recursive rules"
	  (* reverse the list and check for recursion *)
	val cmds = List.foldl noRec [] tsorted
	  
      in
	AND cmds
      end
    end
    fun run x = ((x ();true) handle e =>
		 (dprint (exnMessage e); false))

  end

