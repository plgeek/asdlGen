(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
Here are the concrete implementations of the various property signatures.
Notice they all are implemented with the same extensible record type
implemented by the [[structure Properties]]. To keep the type distinct
the resulting structures use an opaque signature match [[:>]].
Each the actual external string used to refer to each property is
defined as the [[name]] field of the record passed to a function to
build the property. 
**)
(**:[[functor CommonProps]]:
This functor builds all the common functionally. It paramterized by a
string which is used for error reporting.
**)
functor CommonProps(val name : string) =
	    struct
		open Properties
		val p = make_desc name
		val new = from_inits p
		val parse = parse_inits p
		val (source_name,init_source_name) =
		    decl_path_opt p {name="source_name",default=NONE}
		val (doc_string,int_doc_string) =
		    decl_string_opt p {name="doc_string",default=NONE}
	    end
(**)
(**:[[structure FieldProps]]:**)
structure FieldProps :> FIELD_PROPS =
    struct
	structure P = CommonProps(val name = "field props")
	open P
    end
(**)
(**:[[structure ConProps]]:**)
structure ConProps :> CON_PROPS =
    struct
	structure P = CommonProps(val name = "con props")
	open P
	val (enum_value,_) =
	    decl_int_opt p {name="enum_value",default=NONE}
    end
(**)
(**:[[structure TypProps]]:**)
structure TypProps :> TYP_PROPS =
    struct
	structure P = CommonProps(val name = "typ props")
	open P
	val (user_attribute,_) =
	    decl_path_opt p {name="user_attribute",default=NONE}
	val (reader,_) =
	    decl_path_opt p {name="reader",default=NONE}
	val (writer,_) =
	    decl_path_opt p {name="writer",default=NONE}
	val (base_class,_) =
	    decl_path_opt p {name="base_class",default=NONE}
	val (user_init,_) =
	    decl_path_opt p {name="user_init",default=NONE}
	val (natural_type,_) =
	    decl_path_opt p {name="natural_type",default=NONE}
	val (natural_type_con,_) =
	    decl_path_opt p {name="natural_type_con",default=NONE}
	val (wrapper,_) =
	    decl_path_opt p {name="wrapper",default=NONE}
	val (unwrapper,_) =
	    decl_path_opt p {name="unwrapper",default=NONE}
    end
(**)
(**:[[structure ModProps]]:**)
structure ModProps :> MOD_PROPS =
    struct
	structure P = CommonProps(val name = "mod props")
	open P
	val (file,mk_file) = decl_string p {name="file",default="T"}
	val (custom_allocator,_) =
	    decl_string_opt p {name="c_allocator",default=NONE}

	val (interface_prologue,_) =
	    decl_string p {name="interface_prologue",default=""}
	val (interface_epilogue,_) =
	    decl_string p {name="interface_epilogue",default=""}

	val (implementation_prologue,_) =
	    decl_string p {name="implementation_prologue",default=""}
	val (implementation_epilogue,_) =
	    decl_string p {name="implementation_epilogue",default=""}
	val (suppress,_) =
	    decl_bool p {name="suppress",default=false}
	val (is_library,_) =
	    decl_bool p {name="is_library",default=false}
    end 
(**)
(**:[[structure ModEnvProps]]:**)
structure ModEnvProps :> MOD_ENV_PROPS =
    struct
	structure P = CommonProps(val name = "modenv props")
	open P
	val (mono_types,init_mono_types) =
	  decl_bool p {name="mono_types",default=false}
	val (pickler_kind,init_pickler_kind) =
	  decl_string p {name="pickler_kind",default="std"}
	val (explicit_sharing,init_explicit_sharing) =
	  decl_bool p {name="explicit_sharing",default=false}
	val (aux_mod_suffix,init_aux_mod_suffix) =
	    decl_string_opt p {name="aux_mod_suffix",default=NONE}
    end 
(**)
