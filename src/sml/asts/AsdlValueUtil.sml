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
structure AsdlValueUtil : AsdlValueUtil_SIG =
  struct
    open StdPkl
    
    
    
    fun attrbs_asdl_value x = 
        (case (x) of 
              (AsdlValue.SumValue{typename, con, attrbs, vs}) => {typename=typename}
            | (AsdlValue.ProductValue{typename, v, vs}) => {typename=typename}
            | (AsdlValue.SequenceValue{typename, vs}) => {typename=typename}
            | (AsdlValue.NoneValue{typename}) => {typename=typename}
            | (AsdlValue.SomeValue{typename, v}) => {typename=typename}
            | (AsdlValue.PrimValue{typename, v}) => {typename=typename})
    and read_prim_value s = 
        (case ((read_tag s)) of 
              1 => let val int1 =  (StdPrimsUtil.read_int s) in AsdlValue.IntValue(int1)
              end
            | 2 => let val string1 =  (StdPrimsUtil.read_string s) in AsdlValue.StringValue(string1)
              end
            | 3 => let val identifier1 =  (StdPrimsUtil.read_identifier s) in AsdlValue.IdentifierValue(identifier1)
              end
            | _ => (die ()))
    and read_asdl_value s = 
        (case ((read_tag s)) of 
              1 => let val typename =  (TypePickleUtil.read_qid s)
                val con =  (TypePickleUtil.read_qid s)
                val attrbs =  (read_list read_asdl_value s)
                val vs =  (read_list read_asdl_value s) in AsdlValue.SumValue{typename=typename, con=con, attrbs=attrbs, vs=vs}
              end
            | 2 => let val typename =  (TypePickleUtil.read_qid s)
                val v =  (read_asdl_value s)
                val vs =  (read_list read_asdl_value s) in AsdlValue.ProductValue{typename=typename, v=v, vs=vs}
              end
            | 3 => let val typename =  (TypePickleUtil.read_qid s)
                val vs =  (read_list read_asdl_value s) in AsdlValue.SequenceValue{typename=typename, vs=vs}
              end
            | 4 => let val typename =  (TypePickleUtil.read_qid s) in AsdlValue.NoneValue{typename=typename}
              end
            | 5 => let val typename =  (TypePickleUtil.read_qid s)
                val v =  (read_asdl_value s) in AsdlValue.SomeValue{typename=typename, v=v}
              end
            | 6 => let val typename =  (TypePickleUtil.read_qid s)
                val v =  (read_prim_value s) in AsdlValue.PrimValue{typename=typename, v=v}
              end
            | _ => (die ()))
    and read_asdl_value_list s = 
        (read_list read_asdl_value s)
    and write_prim_value x s = 
        (case (x) of 
              (AsdlValue.IntValue(int1)) => ((write_tag 1 s); (StdPrimsUtil.write_int int1 s))
            | (AsdlValue.StringValue(string1)) => ((write_tag 2 s); (StdPrimsUtil.write_string string1 s))
            | (AsdlValue.IdentifierValue(identifier1)) => ((write_tag 3 s); (StdPrimsUtil.write_identifier identifier1 s)))
    and write_asdl_value x s = 
        (case (x) of 
              (AsdlValue.SumValue{typename, con, attrbs, vs}) => ((write_tag 1 s); (TypePickleUtil.write_qid typename s); (TypePickleUtil.write_qid con s); (write_list write_asdl_value attrbs s); (write_list write_asdl_value vs s))
            | (AsdlValue.ProductValue{typename, v, vs}) => ((write_tag 2 s); (TypePickleUtil.write_qid typename s); (write_asdl_value v s); (write_list write_asdl_value vs s))
            | (AsdlValue.SequenceValue{typename, vs}) => ((write_tag 3 s); (TypePickleUtil.write_qid typename s); (write_list write_asdl_value vs s))
            | (AsdlValue.NoneValue{typename}) => ((write_tag 4 s); (TypePickleUtil.write_qid typename s))
            | (AsdlValue.SomeValue{typename, v}) => ((write_tag 5 s); (TypePickleUtil.write_qid typename s); (write_asdl_value v s))
            | (AsdlValue.PrimValue{typename, v}) => ((write_tag 6 s); (TypePickleUtil.write_qid typename s); (write_prim_value v s)))
    and write_asdl_value_list x s = 
        (write_list write_asdl_value x s)
    
    
  
  end
