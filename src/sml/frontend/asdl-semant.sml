structure AsdlSemant :> ASDL_SEMANT =
  struct
    (** new alternative interface **)
    datatype 'mod  menv =
      MEnv of {modules:'mod list,
	       prim_modules:Semant.module_info list,
	       prim_types:Semant.type_info list}
      
    datatype ('typ,'tycon) mod =
      Module of {module:Semant.module_info,
		 imports:Semant.module_info list,
		 props:Semant.Module.P.props,
	          typs:'typ list,
	     type_cons:'tycon list}
		
    datatype field =
      Fd of {finfo:Semant.field_info,
	     kind:Semant.kind option,
	     name:Semant.Field.Id.id,
	     tname:Semant.Type.Id.id,
	     is_local:bool, 
	     props:Semant.Field.P.props,
	     tinfo:Semant.type_info,
	     tprops:Semant.Type.P.props}

    datatype 'fd con =
      Con of {cinfo:Semant.con_info,
	      cprops:Semant.Con.P.props,
	      tprops:Semant.Type.P.props,
	      tinfo:Semant.type_info,
	      name:Semant.Con.Id.id,
	      attrbs:'fd list,
	      fields:'fd list}
      
    datatype tycon =
      TyCon of {tinfo:Semant.type_info,	     
		 name:Semant.Type.Id.id,
		 kinds:Semant.kind list,
		 props:Semant.Type.P.props}
       
    datatype ('fd,'con) typ =
      Sum of {tinfo:Semant.type_info,
	      props:Semant.Type.P.props,
	      name:Semant.Type.Id.id,
  	      cons:'con list,
	      fields:'fd list}
      
    | Product of {tinfo:Semant.type_info,
		  props:Semant.Type.P.props,
		  name:Semant.Type.Id.id,
		  fields:'fd list}
    type ('a,'b) cvt = 'a -> 'b
    structure S = Semant
    fun fold {menv,module,field,con,typ,tycon} me =
      let
	fun do_module m =
	  let
	    fun do_typ id =
	      let
		val tinfo = S.Module.type_info m id
		val props = S.Type.props tinfo 
		val name = S.Type.src_name tinfo
		val cons = List.map do_con (S.Type.cons tinfo)
		val fields = List.map (do_field tinfo) (S.Type.fields tinfo)
	      in case cons of
		[] => typ(Product{tinfo=tinfo,props=props,
				    name=name,fields=fields})
	       | cons => typ(Sum{tinfo=tinfo,props=props,
				    name=name,cons=cons,fields=fields})
	      end
	    and do_con cinfo =
	      let
		val cinfo = cinfo
		val cprops = S.Con.props cinfo
		val tinfo = S.Module.con_type m cinfo
		val tprops = S.Type.props tinfo 
		val name = S.Con.src_name cinfo
		val attrbs = List.map (do_field tinfo) (S.Type.fields tinfo)
		val fields = List.map (do_field tinfo)  (S.Con.fields cinfo)
	      in con(Con{cinfo=cinfo,tinfo=tinfo,name=name,
			   tprops=tprops,cprops=cprops,
			   attrbs=attrbs,fields=fields})
	      end
	    and do_field srct finfo =
	      let
		val finfo = finfo
		val kind = S.Field.kind finfo
		val tinfo = S.Module.field_type m finfo
		val tprops = S.Type.props tinfo 
		val props = S.Field.props finfo
		val is_local = S.Module.is_defined m tinfo
		val name = S.Field.src_name finfo
		val tname = S.Type.src_name tinfo
	      in field(Fd{finfo=finfo,kind=kind,
			    is_local=is_local,props=props,
			    tprops=tprops,
			    tinfo=tinfo,name=name,tname=tname})
	      end
	    and do_type_con (id,kinds) =
	      let
		val tinfo = S.Module.type_info m id
		val props = S.Type.props tinfo 
		val name = S.Type.src_name tinfo
	      in tycon(TyCon{tinfo=tinfo,name=name,kinds=kinds,props=props})
	      end
	    val props = S.Module.props m
	    val typs =  List.map do_typ (S.Module.types m)
	    val type_cons = List.map do_type_con (S.MEnv.qualified me m)
	    val imports = S.Module.imports m
	  in module (Module{module=m,
			   imports=imports,
			   props=props,
			   typs=typs,
			   type_cons=type_cons})
	  end
	val minfos = (S.MEnv.modules me)
	val prim_modules = List.filter S.Module.prim_module  minfos
	val modules = List.filter (not o S.Module.prim_module)  minfos
	val modules = List.map do_module  modules
	val prim_types = S.MEnv.prim_types me
      in menv(MEnv{modules=modules,
		     prim_modules=prim_modules,
		     prim_types=prim_types})
      end
      
  end







