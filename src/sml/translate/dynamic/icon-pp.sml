(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure IconPP : DYNAMIC_PP =
    struct 
      structure Ast  = DynamicAst
      structure PP = mkPPAst(structure Ast = Ast
			     structure IdMap = IdMaps.Icon
			     val cap_mod = false
			     val cap_typ = false
			     val sep = "_")
      open PP
      type code =  (Ast.module * Semant.Module.P.props)
      val opts = CommandOptions.empty
      fun mkComment l =
	vb 0 (str "####") (seq nl (fn x => str ("# "^x)) l) (str "####") 
      val mkDeps = PPDepends.makefile
      open Ast
      val pp_tid = PP.tid
      val pp_id = PP.vid
	fun pp_ty (TyId tid) = pp_tid tid
	  | pp_ty (TyCon (tid,ts)) =
	  hb 2 (str "<") (seq (hsep ",") pp_ty ts) (str ">")
	  | pp_ty (TyFunction(tes,ty)) =
	  hb 2 (str "proc (") (seq (hsep ",") pp_ty tes) 
	  (cat [str ") rets",pp_ty ty])
	   
	(* code to fix up scope and locals in Icon *)
	structure OrdKey =
	  struct
	    type ord_key = VarId.id
	    val compare = VarId.compare
	  end
	structure Env = SplayMapFn(OrdKey)
	structure Set = SplaySetFn(OrdKey)

	fun fix_id (env,s) id =
	  (case (Env.find(env,id)) of
	     NONE => id
	   | (SOME id') => id')

	fun decl_id (env,s) (id,xs) =
	  (case (Env.find(env,id)) of
	     NONE => (id,(Env.insert(env,id,id),s),Set.add'(id,xs))
	   | (SOME id) =>
	       let val id' = VarId.suffixBase (Int.toString s) id
	       in (id',(Env.insert(env,id,id'),s),Set.add'(id',xs))
	       end)
	fun new_scope (env,s) = (env,s+1)

	fun fix_bind env (Call(e,el),xs) =
	     let val (e',xs) = fix_bind env (e,xs)
	         val (el',xs) = fix_binds env (el,xs)
	     in (Call(e',el'),xs)
	     end
	  | fix_bind env (Case{test,clauses,default},xs) =
	  let
	    fun do_clause ({const,body},(cs,xs)) =
	      let val (body',xs) = fix_bind env (body,xs)
	      in ({const=const,body=body'}::cs,xs)
	      end
	    val (test',xs) = fix_bind env (test,xs)
	    val (clauses',xs) = List.foldr do_clause ([],xs) clauses
	    val (default',xs) = fix_bind env (default,xs)
	  in (Case{test=test',clauses=clauses',default=default'},xs)
	  end
	  | fix_bind env (Bind{binds,body},xs) =
	  let
	    fun do_bind ({name,v=Id id},((env,s),bs,xs)) =
	      let
		val id' = fix_id (env,s) id
		val env = Env.insert(env,name,id')
	      in ((env,s),bs,xs)
	      end
	      | do_bind ({name,v},(env,bs,xs)) =
	      let val (v',xs) = fix_bind env (v,xs)
		  val (name',env,xs) = decl_id env (name,xs)
	      in (env,{name=name',v=v'}::bs,xs)
	      end
	    val (env,binds',xs) = List.foldr do_bind (env,[],xs) binds
	    val (body',xs) = fix_bind (new_scope env) (body,xs)
	  in (Bind{binds=binds',body=body'},xs)
	  end
	  | fix_bind env (Seq el,xs) =
	    let val (el',xs) = fix_binds env (el,xs)
	    in (Seq el',xs)
	    end
	  | fix_bind env (MakeStruct(name,el,fd),xs) =
	    let val (el',xs) = fix_binds env (el,xs)
	    in (MakeStruct(name,el',fd),xs)
	    end
	  | fix_bind env (GetField(e,fd),xs) = 
	    let val (e',xs) = fix_bind env (e,xs)
	    in (GetField(e',fd),xs)
	    end
	  | fix_bind env (Id id,xs) = (Id (fix_id env id),xs)
	  | fix_bind env (GetStructType e,xs) =
	    let val (e',xs) = fix_bind env (e,xs)
	    in (GetStructType(e'),xs)
	    end
	  | fix_bind env (x,xs) = (x,xs)
	and fix_binds env (el,xs) =
	     let
	       fun do_e (e,(es,xs)) =
		 let val (e,xs) = fix_bind env (e,xs)
		 in (e::es,xs)
		 end
	     in List.foldr do_e ([],xs) el
	     end
	fun pp_const (Int i) = num i
	  | pp_const (TypeName t) = cat [str "\"",pp_tid t,str "\""]
	  | pp_const (String s) = cat [str "\"",str s,str "\""]
	  | pp_const Nil = str "&null"
	and pp_exp (Const c) = pp_const c
	  | pp_exp (Id i) = pp_id i
	  | pp_exp (Call (e,el)) =
	  hb 2 (cat [pp_exp e,str "("]) (seq (hsep ",") pp_exp el) (str ")")
	  | pp_exp (Case{test,clauses,default}) =
	  let fun pp_clause {const,body} =
	    cat [pp_const const, str " : ", pp_exp body]
	  in vb 2 (cat [str "case ",pp_exp test, str " of {"])
	    (cat [(seq' nl pp_clause clauses),
		  str "default :", pp_exp default])
	    (str "}")
	  end
	  | pp_exp (Bind {binds=[],body}) = pp_exp body
	  | pp_exp (Bind {binds,body}) =
	  let fun pp_bind {name,v} =cat [pp_id name, str " := ",pp_exp v]
	  in vb 2 (str "{")
	    (cat [seq' nl pp_bind binds, pp_exp body])
	    (str "}")
	  end
	  | pp_exp (Seq []) = pp_const (Nil)
	  | pp_exp (Seq [e]) = pp_exp e
	  | pp_exp (Seq el) =
	  vb 2 (str "{") (seq (cat [str ";",nl]) pp_exp el) (str "}")
	  | pp_exp (MakeStruct(name,el,fd)) =
	  hb 2 (cat [pp_tid name,str "("]) (seq (hsep ",") pp_exp el) (str ")")
	  | pp_exp (GetStructType e) = cat [str "type(",pp_exp e,str ")"]
	  | pp_exp (GetField(e,{name,ty})) = cat [pp_exp e,str".",pp_id name]
	  | pp_exp (Error s) =
	  cat[str "stop(\"Error:\",&file,\":\",&line,\": \",\"",
			       str s,str "\")"]
	and pp_decl (DeclStruct(tid,fds,ty)) =
	     (cat [str  "record ",pp_tid tid,pp_fds fds])
	  | pp_decl (DeclFun(id,fds,exp,ty)) =
	  let
	    val (exp,vars) = fix_bind (Env.empty,0) (exp,Set.empty)
	    val local_pp =
	      lst empty
	      (fn vars => vb 2 (str "local ") (seq (hsep ",")
					       pp_id vars) empty) 
	  in
	    vb 2 (cat [str "procedure ",pp_id id,pp_fds fds])
	      (cat [local_pp (Set.listItems vars),
		    str "return {",nl,
		    pp_exp exp,nl,str "}"])
	      (str "end")
	  end
	and  pp_decl_ty (DeclStruct(tid,[],ty)) =
	      cat [str "record ",pp_tid tid,str " isa ",pp_ty ty]
	  | pp_decl_ty  (DeclStruct(tid,fds,ty)) =
	      vb 2 (cat [str "record ",pp_tid tid])
	      (seq nl pp_fd_ty fds)
	      (cat [str "isa ",pp_ty ty])
	  | pp_decl_ty (DeclFun(id,fds,exp,ty)) =
	      vb 2 (cat [str "procedure ",pp_id id])
	      (seq nl pp_fd_ty fds)
	      (cat [str "returns ",pp_ty ty])
	and pp_fd_ty {name,ty} = cat [pp_id name,str " : ",pp_ty ty]
	and pp_decls d =
	  let val ty_pp = seq nl pp_decl_ty d
	    val cm =
	      String.fields (fn x => x = #"\n") (PPUtil.pp_to_string 60 ty_pp)
	  in cat [mkComment cm,PP.nl, seq' nl pp_decl d]
	  end
	and pp_fd {name,ty} = pp_id name
	and pp_fds fds =
	  hb 2 (str "(") (seq (hsep ",") pp_fd fds) (str ")")
	  
	and pp_module (Module{name,imports,decls}) =
	  cat [str "# module ",PP.mid name,nl,
	       if (not(List.null imports)) then
		 cat [str "link ",
		      seq (hsep ",") PP.mid imports,nl]
	       else PP.empty,pp_decls decls]
	fun pp_code p (m as Module{name,imports,...},props) =
	  let
	    fun file_name x =
	      let val x =
		case (ModuleId.toString x) of 
		  "" => "Ast" | x => x
	      in OS.Path.joinBaseExt{base=x,ext=SOME "icn"}
	      end
	  in [FileSet.mkFile{name=file_name name,
			     depends=List.map file_name imports,
			     body=pp_module m}]
	  end

    end







