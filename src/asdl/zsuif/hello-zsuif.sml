structure ZSUIFHelloWorld =
    struct
	open zsuif
	val hello_string = "Hello, world!\n\000";

	local
	    val id = ref 1
	    val symb = ref 1
	in
	    fun new_type_id () =
		(!id) before (id := (!id) + 1)
	    fun new_symb (s) =
		{uid=(!id),
		 name=Identifier.fromString s}
		before (id := (!id) + 1)
	end
    
	val char_type_id = new_type_id ()
	val char_pointer_type_id = new_type_id ()
	val char_pointer_pointer_type_id = new_type_id ()
	val int_type_id = new_type_id ()
	val string_literal_type_id = new_type_id ()

	val char_type =
	    Data(Integer_type
		 {bit_size=Int 8, bit_alignment=Int 8})

	val int_type =
	    Data(Integer_type
		 {bit_size=Int 32, bit_alignment=Int 32})

	val char_pointer_type =
	    Data(Pointer_type
		      {bit_size=Int 32, bit_alignment=Int 32,
		       reference_type=char_type_id})

	val char_pointer_pointer_type = 
	    Data(Pointer_type
		 {bit_size=Int 32, bit_alignment=Int 32,
		  reference_type=char_pointer_type_id})

	val argc_type_id = int_type_id

	val argv_type_id = char_pointer_pointer_type_id
	    
	val main_type =
	    (Basic_procedure_type{bit_size=Int 32,
					   bit_alignment=Int 32,
					   result_types=[int_type],
					   args=
					   [int_type,
					    char_pointer_pointer_type]})
	val printf_type =
	    (Basic_procedure_type{bit_size=Int 32,
					   bit_alignment=Int 32,
					   result_types=[int_type],
					   args=[char_pointer_type]})
	val printf_type_id = new_type_id()

	val slen = ((String.size  hello_string))
	val string_literal_type =
	    Data(Array_type{element_type=char_type,
			    lower_bound=SOME(Int 0),
			    upper_bound=SOME(Int slen),
			    bit_size=Int (slen*8),
			    bit_alignment=Int 32})
	    
	val main_symbol = new_symb("main")

	val argv_symbol = new_symb ("argv")
	val argc_symbol = new_symb ("argc")
	    
	val string_literal_symbol = new_symb("temp")

	val string_literal_value_block =
	    let
		val chars = Vector.fromList (String.explode hello_string)
		fun init_block (i,c) =
		    {bit_offset=i*8,
		     block=Constant_value_block
		     {constant=ConstInt(Finite 0),
		      data_type=char_type_id}}
		val inits =
		    (Vector.foldr (op ::)
		     [] (Vector.mapi init_block (chars,0,NONE)))
	    in
	    Multi_value_block{data_type=char_type_id,
			      inits=inits}
	    end


	val printf_symbol = new_symb("printf")
	val printf_address_op =
	    SrcDst{instr=Load_address_instruction
		   {addressed_symbol=printf_symbol,
		    result_type=printf_type_id,
		    destination_op=DstTmp},
		   op_num=0}

	val printf_argument_op =
	    SrcDst{instr=Load_address_instruction
		   {addressed_symbol=string_literal_symbol,
		    result_type=char_pointer_type_id,
		    destination_op=DstTmp},
		   op_num=0}

	val printf_call =
	    Call_instruction{callee_address=printf_address_op,
			     arguments=[printf_argument_op],
			     return_values=[{result_type=int_type_id,
					     destination_op=DstTmp}]}

	val zero_value =
	    SrcDst{instr=
		   Load_constant_instruction
		   {constant=ConstInt(Finite 0),
		    result_type=int_type_id,
		    destination_op=DstTmp},
		   op_num=0}
	    
	val main_def =
	    {name=main_symbol,
	     qualifications=[],
	     procedure_type=main_type,
	     procedure_body=
	     SOME{params=[argc_symbol,argv_symbol],
		  body=Scope_statement
		  {definition_block={defined_variables=[string_literal_symbol],
				     defined_procedures=[]},
		   body=
		   Sequence_statement
		   {statements=
		    [Eval_statement   {instructions=[printf_call]},
		     Return_statement {return_values=[zero_value]}]}}}}

	val printf_def =
	    {name=printf_symbol,
	     qualifications=[],
	     procedure_type=printf_type,
	     procedure_body=NONE}

	val string_literal_def = 
	    {name=string_literal_symbol,
	     type'=string_literal_type,
	     value_block=SOME (string_literal_value_block)}

	val symbol_table =
	    {entries=
	     [ProcedureEntry
	      {key=main_symbol,address_taken=false,def=main_def},
	      ParameterEntry
	      {key=argc_symbol, address_taken=false,
	       name=argc_symbol, type'=int_type_id,
	       bit_alignment=Int 32, proc=main_symbol},
	      ParameterEntry
	      {key=argv_symbol, address_taken=false,
	       name=argv_symbol, type'=char_pointer_pointer_type_id,
	       bit_alignment=Int 32, proc=main_symbol},
	      VariableEntry
	      {key=string_literal_symbol, address_taken=true,
	       is_local=true,
	       def=string_literal_def}]}

	val type_table =
	    {entries=
	     [{key=char_type_id,
	       value=char_type},
	      {key=char_pointer_type_id,
	       value=char_pointer_type},
	      {key=char_pointer_pointer_type_id,
	       value=char_pointer_pointer_type},
	      {key=int_type_id,value=int_type},
	      {key=printf_type_id,value=Procedure printf_type},
	      {key=string_literal_type_id,value=string_literal_type}]}

	val definition_block =
	    {defined_variables=[],
	     defined_procedures=[main_symbol]}
	    
	val file_block:file_block = {source_file_name="hello-zsuif.sml",
			  definition_block=definition_block}

	val file_set_block =
	    {file_blocks=[file_block],
	     type_table=type_table:type_table,
	     symbol_table=symbol_table:symbol_table,
	     extern_symbol_table=
	     {entries=[ProcedureEntry
	      {key=printf_symbol, address_taken=true,def=printf_def}]},
	     information_block=C_information_block}:file_set_block
	    
	fun write_pickle s =
	    let
		val outs = BinIO.openOut s
	    in
		(zsuif.write_file_set_block file_set_block outs);
		BinIO.closeOut outs
	    end
	fun read_pickle s =
	    let
		val ins = BinIO.openIn s
	    in
		(zsuif.read_file_set_block ins) before (BinIO.closeIn ins)
	    end

    end




