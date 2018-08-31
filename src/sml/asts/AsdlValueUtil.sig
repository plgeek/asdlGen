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
signature AsdlValueUtil_SIG = 
  sig
    include STD_PKL
    
    
    
    val attrbs_asdl_value : AsdlValue.asdl_value -> {typename:TypePickle.qid}
    val read_prim_value : instream -> AsdlValue.prim_value
    val read_asdl_value : instream -> AsdlValue.asdl_value
    val read_asdl_value_list : instream -> AsdlValue.asdl_value list
    val write_prim_value : AsdlValue.prim_value -> outstream -> unit
    val write_asdl_value : AsdlValue.asdl_value -> outstream -> unit
    val write_asdl_value_list : AsdlValue.asdl_value list -> outstream -> unit
    
  end
