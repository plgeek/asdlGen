signature EXPORTABLE_BUILD =
  sig
    include CORE_BUILD
    val export     : TextIO.outstream -> unit cmd -> unit
  end
structure BuildExport :> EXPORTABLE_BUILD =
  struct
    datatype cmd' = 
      c_VAR of var
    | c_STR of string
    | c_INT of int
    | c_WRITE of cmd'
    | c_CONCAT of cmd' list
    | c_EXEC of var * cmd' list
    | c_EXEC_WITH_INPUT of var * cmd' list * cmd'
    | c_EXIT of cmd'
    | c_OR of cmd' list
    | c_AND of cmd' list
    | c_SEQ of cmd' list
    | c_IGN of cmd' 
    | c_BUILD of {name:string option,rules:rule list} 
    and valid =
      c_INVALID 
    | c_VALIDATE of {targets:Paths.file_path list,
		     depends:Paths.file_path list} 
    and rule =  c_RULE of {valid:valid,update:cmd'}
    withtype var = (int * {name:string option,doc:string list,
			   init:cmd'})
    type 'a cmd = cmd'

    local
      val nextid = ref 0
    in
      fun getid () = (nextid := (!nextid) +1; !nextid)
    end
    fun mkVAR x = (getid(),x)
    fun VAR v = c_VAR v
    val STR = c_STR
    val INT = c_INT
    val WRITE = c_WRITE
    val CONCAT = c_CONCAT
    val EXEC = c_EXEC
    val EXEC_WITH_INPUT = c_EXEC_WITH_INPUT
    val EXIT = c_EXIT
    val OR = c_OR
    val AND = c_AND
    val SEQ = c_SEQ
    val IGN = c_IGN
    val INVALID = c_INVALID
    val VALIDATE = c_VALIDATE
    val RULE = c_RULE
    val BUILD = c_BUILD

    structure  S = SplaySetFn
      (struct
	type ord_key = var
	fun compare ((x,_),(y,_)) = Int.compare(x,y)
      end)

    fun listVars (c_VAR v,env) = S.add'(v,env)
      | listVars (c_WRITE c,env) = listVars(c,env)
      | listVars (c_CONCAT s,env) = List.foldl listVars env s
      | listVars (c_OR s,env) = List.foldl listVars env s
      | listVars (c_AND s,env) = List.foldl listVars env s
      | listVars (c_SEQ s,env) = List.foldl listVars env s
      | listVars (c_IGN c,env) = listVars (c,env)
      | listVars (c_EXIT c,env) = listVars(c,env)
      | listVars (c_EXEC (v,cs),env) = List.foldl listVars (S.add'(v,env)) cs
      | listVars (c_EXEC_WITH_INPUT (v,cs,i),env) =
      List.foldl listVars (S.add'(v,env)) (i::cs)
      | listVars (c_BUILD{name,rules},env) =
      let
	fun doRule (c_RULE{update,...},e) = listVars(update,e)
      in
	List.foldl doRule env rules
      end
      | listVars (_,env) = env

    datatype token =
        Str of string
      | Lit of token 
      | Var of string
      | Cat of token list
      | Seq of token list
      | Tabbed of token 
      | Line of token 
    fun seqTokens sep cs =
      let
	fun do_it [c] = [toTokens c]
	  | do_it (c::cs) = (toTokens c)::sep::(do_it cs)
	  | do_it [] = [Str ""]
      in
	Seq (do_it cs)
      end
    and toTokens (c_VAR (i,{name,...})) =
      (case name of
	 NONE => Var ("v"^(Int.toString i))
       | (SOME s) => Var (s^(Int.toString i)))
      | toTokens (c_STR s) = Str s
      | toTokens (c_INT i) = Str (Int.toString i)
      | toTokens (c_WRITE c) = Seq[Str "echo",Lit (toTokens c)]
      | toTokens (c_CONCAT cs) = Cat (List.map toTokens cs)
      | toTokens (c_EXEC (v,cs)) =
	 Seq (List.foldr (fn (c,ts) => (toTokens c)::ts) [] ((c_VAR v)::cs))
      | toTokens (c_EXEC_WITH_INPUT (v,cs,i)) =
	 Seq[Str "echo",Lit (toTokens i),Str "|",toTokens (c_EXEC(v,cs))]
      | toTokens (c_EXIT c) =
	 Seq[Str "echo",Lit (toTokens c),Str "; exit 1"]

      | toTokens (c_OR cs) = seqTokens (Str "||") cs
      | toTokens (c_AND cs) = seqTokens (Str "&&") cs
      | toTokens (c_SEQ cs) =  seqTokens (Str ";") cs
      | toTokens (c_IGN c) = Tabbed (Cat[Str "-",toTokens c])
      | toTokens (c_BUILD {name,rules}) = Seq (List.map ruleToken rules)
    and ruleToken (c_RULE{valid,update}) =
     Seq [validToken valid,Tabbed (toTokens update)]
    and validToken c_INVALID = Line (Str "_bogus_::")
      | validToken (c_VALIDATE{targets,depends}) =
      let
	val ts = List.map (Str o Paths.fileToNative) targets
	val ds = List.map (Str o Paths.fileToNative) depends
      in
	Line (Cat [Seq ts,Str ": ",Seq ds])
      end

    fun printTokens say t =
      let
	fun p (Str s) = say s
	  | p (Lit t) = (say "'";p t;say "'")
	  | p (Var s) = say ("$("^s^")")
	  | p (Tabbed t) = (say ("\n\t");p t)
	  | p (Line t) = (say ("\n"); p t)
	  | p (Cat ts) = List.app p ts
	  | p (Seq []) = ()
	  | p (Seq [t]) = p t
	  | p (Seq (t::ts)) = (p t;say " ";p (Seq ts))
      in
	p t
      end
    fun normalize cmd =
      let
	fun map_exps f g (xs,acc) = 
	  let
	    fun doelt (x,(xs,acc)) =
	      let val (x,acc) = f (x,acc)
	      in (x::xs,acc)
	      end
	    val (xs,acc) = List.foldr doelt ([],acc) xs
	  in (g xs,acc)
	  end

	fun map_exp f g (x,acc) =
	  let val (x,acc) = f(x,acc)
	  in (g x,acc)
	  end
	val mvar = mkVAR{name=SOME "SUBMAKE",doc=[],init=c_STR "make"}
	fun make_cmd (SOME s) =  c_EXEC (mvar,[c_STR s])
	  | make_cmd (NONE)   =  c_EXEC (mvar,[c_STR "unamed"])

	fun flatten (c_OR cmds,  acc) = map_exps flatten c_OR  (cmds,acc)
	  | flatten (c_AND cmds, acc) = map_exps flatten c_AND (cmds,acc)
	  | flatten (c_SEQ cmds, acc) = map_exps flatten c_SEQ (cmds,acc)
	  | flatten (c_IGN cmd, acc)  = map_exp  flatten c_IGN (cmd,acc)
	  | flatten (c_BUILD {name, rules}, acc) = let
	      val (rules,acc) =
		map_exps flatten_rule (fn x => x) (rules,acc)
	    in (make_cmd name,(c_BUILD {name=name,rules=rules})::acc)
	    end
	  | flatten (x, acc) = (x, acc)
	and flatten_rule (c_RULE{valid,update},acc) =
	  map_exp flatten (fn v => c_RULE{valid=valid,update=v}) (update,acc)
      in
	flatten (cmd,[])
      end
    
    fun defVars r =
      let
	val vars = listVars (r,S.empty)
	fun varDefTok d n i =
	  Seq [Line (Seq [Str "#",Seq (List.map Str d)]),
	       Line (Cat[Str n,Str "=",toTokens i])]
	fun pv ((i,{name=NONE,init,doc,...}):var) =
	  varDefTok doc ("v"^(Int.toString i)) init
	  | pv (i,{name=SOME s,init,doc,...}) =
	  varDefTok doc (s^(Int.toString i)) init
      in
	Seq (S.foldl (fn (x,xs) => (pv x)::xs) [] vars)
      end
    fun export s cmd =
      let
	fun say x = TextIO.output(s,x)
	fun printCMD c =
	  let
	    val tokens = toTokens c
	    val tokens =
	      Seq [Line (Str "# machine generated edit at your own risk"),
		   defVars c,tokens]
	  in
	    (case c of
	       (c_BUILD{name=SOME n,...}) => say ("# submake "^n)
	     | _ => ());
	    printTokens say tokens
	  end
	val (cmd,subcmds) = normalize cmd
      in
	List.app printCMD (cmd::subcmds)
      end

  end
