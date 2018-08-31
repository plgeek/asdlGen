(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_signature=STD_PKL
  --base_structure=StdPkl
  --line_width=74
  --no_action=false
  --output_directory=asts
  --view=SML
  --xml_pickler=false
  *)
signature TypePickleUtil_SIG = 
  sig
    include STD_PKL
    
    
    
    val attrbs_field : TypePickle.field -> {type_map_key:StdPrims.int, label:StdPrims.identifier}
    val attrbs_type_map_value : TypePickle.type_map_value -> {pkl_tag:StdPrims.int}
    val read_qid : instream -> TypePickle.qid
    val read_prim : instream -> TypePickle.prim
    val read_field : instream -> TypePickle.field
    val read_cnstr_map_value : instream -> TypePickle.cnstr_map_value
    val read_cnstr_map_entry : instream -> TypePickle.cnstr_map_entry
    val read_cnstr_map : instream -> TypePickle.cnstr_map
    val read_type_map_value : instream -> TypePickle.type_map_value
    val read_type_map_entry : instream -> TypePickle.type_map_entry
    val read_type_map : instream -> TypePickle.type_map
    val read_module_map_value : instream -> TypePickle.module_map_value
    val read_module_map_entry : instream -> TypePickle.module_map_entry
    val read_module_map : instream -> TypePickle.module_map
    val read_type_env : instream -> TypePickle.type_env
    val read_type_map_entry_list : instream -> TypePickle.type_map_entry list
    val read_module_map_entry_list : instream -> TypePickle.module_map_entry list
    val read_field_list : instream -> TypePickle.field list
    val read_cnstr_map_entry_list : instream -> TypePickle.cnstr_map_entry list
    val write_qid : TypePickle.qid -> outstream -> unit
    val write_prim : TypePickle.prim -> outstream -> unit
    val write_field : TypePickle.field -> outstream -> unit
    val write_cnstr_map_value : TypePickle.cnstr_map_value -> outstream -> unit
    val write_cnstr_map_entry : TypePickle.cnstr_map_entry -> outstream -> unit
    val write_cnstr_map : TypePickle.cnstr_map -> outstream -> unit
    val write_type_map_value : TypePickle.type_map_value -> outstream -> unit
    val write_type_map_entry : TypePickle.type_map_entry -> outstream -> unit
    val write_type_map : TypePickle.type_map -> outstream -> unit
    val write_module_map_value : TypePickle.module_map_value -> outstream -> unit
    val write_module_map_entry : TypePickle.module_map_entry -> outstream -> unit
    val write_module_map : TypePickle.module_map -> outstream -> unit
    val write_type_env : TypePickle.type_env -> outstream -> unit
    val write_type_map_entry_list : TypePickle.type_map_entry list -> outstream -> unit
    val write_module_map_entry_list : TypePickle.module_map_entry list -> outstream -> unit
    val write_field_list : TypePickle.field list -> outstream -> unit
    val write_cnstr_map_entry_list : TypePickle.cnstr_map_entry list -> outstream -> unit
    
  end
