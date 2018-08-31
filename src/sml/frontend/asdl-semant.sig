signature ASDL_SEMANT =
  sig
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
	     tinfo:Semant.type_info,
	     props:Semant.Field.P.props,
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
	       cons:'con list,
	       name:Semant.Type.Id.id,
	     fields:'fd list}
      
    | Product of {tinfo:Semant.type_info,
	          props:Semant.Type.P.props,
	           name:Semant.Type.Id.id,
 	         fields:'fd list}
    type ('a,'b) cvt =  'a -> 'b
    val fold : {menv:('mod menv,'a) cvt,
	      module:(('typ,'tycon) mod, 'mod) cvt,
	       field:(field,'fd) cvt,
	         con:('fd con,'con) cvt,
		 typ:(('fd,'con) typ,'typ) cvt,
	       tycon:(tycon,'tycon) cvt} -> Semant.menv_info -> 'a 
		 
      
  end