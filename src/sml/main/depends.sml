(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


functor mkDependGen(structure S : SEMANT) : TRANSLATE  =
    struct

	structure IdOrdKey =
	    struct
	      type ord_key = Identifier.identifier
	      val compare = Identifier.compare
	    end
	
	structure Env = SplayMapFn(IdOrdKey)
	structure S = S
	type input = S.menv_info
	type output = unit

	val (cfg,scc) =
	    Params.declareBool Params.empty
	    {name="scc",flag=NONE,default=false}

	structure Env =
	    SplayMapFn(struct
			   type ord_key = SourceId.sid
			   val compare = SourceId.compare
		       end)

	structure Scc =
	    SCCUtilFun(structure Node =
			   struct
			       type ord_key = SourceId.sid option
			       fun compare (SOME x,SOME y) =
				 SourceId.compare(x,y)
				 | compare (SOME _,NONE) = LESS
				 | compare (NONE,SOME _) = GREATER
				 | compare (NONE,NONE) = EQUAL
			   end)

	fun translate p menv =
	    let
		fun mkenv (m,env) =
		    List.foldl (fn (x,env) => (Env.insert(env,x,m)))
		    env (S.Module.types m) 

		val env = List.foldl mkenv
		    Env.empty (S.MEnv.modules menv) 

		fun follow (SOME id) =
		    (case (Env.find(env,id)) of
			NONE => []
		      | (SOME m) =>
			    let
				val ids = S.Type.uses (S.Module.type_info m id)
				val tinfos =
				    List.map (S.Module.type_info m) ids
				val tinfos =
				    List.filter (not o S.Type.is_prim) tinfos
			    in
				List.map (SOME o S.Type.name) tinfos
			    end)
		  | follow NONE =
		    List.map (SOME o #1) (Env.listItemsi env)

		val torder = Scc.topOrder {root=NONE,follow=follow}

		fun print_id (SOME n) =
		    (if List.null (follow (SOME n)) then
			 ()
		    else
			();
			 print (SourceId.toString n))
		  | print_id NONE = print "-- root"

		fun depends (Scc.SIMPLE n) =
		    (print_id n;print "\n")
		  | depends (Scc.RECURSIVE recs) =
		    (print "\n-- begin scc\n";
		     List.app (fn x => (print_id x;print "\n")) recs;
		     print "-- end scc\n\n")
		    
	    in
		if scc p then
		    List.app depends torder
		else
		    ()
	    end
	
    end





