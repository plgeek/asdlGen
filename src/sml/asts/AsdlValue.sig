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
signature AsdlValue_SIG = 
  sig
    include STD_PKL
    
    datatype prim_value = IntValue of (StdPrims.int)
        | StringValue of (StdPrims.string)
        | IdentifierValue of (StdPrims.identifier)
    and asdl_value = SumValue of {typename:TypePickle.qid, con:TypePickle.qid, attrbs:asdl_value list, vs:asdl_value list}
        | ProductValue of {typename:TypePickle.qid, v:asdl_value, vs:asdl_value list}
        | SequenceValue of {typename:TypePickle.qid, vs:asdl_value list}
        | NoneValue of {typename:TypePickle.qid}
        | SomeValue of {typename:TypePickle.qid, v:asdl_value}
        | PrimValue of {typename:TypePickle.qid, v:prim_value}
    
    
    
  end
