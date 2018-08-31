class trans_instruction : public suif_visitor  {
private:
     zsuif_instruction* zinstr;
     trans_suif* t;
public:
     
    trans_instruction(trans_suif* t, instruction *s)  { 
	 this->t = t;
	 zinstr = NULL;
	 s->apply_pyg_visitor(this);
    }
    
    zsuif_instruction* answer(void) {
	 return zinstr;
    }

    void handle_binary_arithmetic_instruction
	 (binary_arithmetic_instruction *instr) {
	 zsuif_binop* opcode = t->get_binop(instr->opcode());
	 zsuif_source_op* source1 = t->trans(&(instr->source1()));
	 zsuif_source_op* source2 = t->trans(&(instr->source2()));
	 zsuif_type_id* result_type = t->trans(instr->result_type(0));
	 zsuif_destination_op* destination_op = 
	      t->trans(&(instr->get_destination_op()));

	 zinstr = new zsuif_Binary_arithmetic_instruction
	      (opcode, source1, source2, result_type, destination_op);
    }

    void handle_unary_arithmetic_instruction
	 (unary_arithmetic_instruction *instr) {
	 zsuif_unop* opcode = t->get_unop(instr->opcode());
	 zsuif_source_op* source = t->trans(&(instr->source()));
	 zsuif_type_id* result_type = t->trans(instr->result_type(0));
	 zsuif_destination_op* destination_op = 
	      t->trans(&(instr->get_destination_op()));
	 
	 zinstr = new zsuif_Unary_arithmetic_instruction
	      (opcode, source, result_type, destination_op);
    }

    void handle_copy_instruction(copy_instruction *instr) {
	 zsuif_source_op* source = t->trans(&(instr->source()));
	 zsuif_type_id* result_type = t->trans(instr->result_type(0));

	 zsuif_destination_op* destination_op = NULL;
	 zsuif_destination_op_list* destination_ops = NULL;
	 
	 s_count_t num_destinations = instr->num_destination_ops();
	 assert(num_destinations > 0);
	 /* cons things on backward so idx 0 is first */
	 while(num_destinations--) {
	     destination_op =  
		   t->trans(&(instr->get_destination_op(num_destinations)));

	      /* treat first one specially */
	      if(num_destinations > 0) {
		   destination_ops = 
			new zsuif_destination_op_list
			(destination_op,destination_ops);
	      }
	 }
	 zinstr = new zsuif_Copy_instruction
	      (source, result_type, destination_op, destination_ops);
    }


    void handle_select_instruction(select_instruction *instr) {
        zsuif_source_op* selector = t->trans(&(instr->selector()));
        zsuif_source_op* selection1 = t->trans(&(instr->selection1()));
        zsuif_source_op* selection2 = t->trans(&(instr->selection2()));
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Select_instruction
	     (selector, selection1, selection2, result_type, destination_op);
    }

    void handle_array_reference_instruction
	 (array_reference_instruction *instr) {
	 zsuif_source_op* base_array_address = 
	      t->trans(&(instr->base_array_address()));
	 zsuif_source_op* index = t->trans(&(instr->index()));
	 zsuif_type_id* result_type = t->trans(instr->result_type(0));
	 zsuif_destination_op* destination_op = 
	      t->trans(&(instr->get_destination_op()));
	 
	 zinstr = new zsuif_Array_reference_instruction
	      (base_array_address, index, result_type, destination_op);
    }

    void handle_field_access_instruction(field_access_instruction *instr) {
        zsuif_source_op* base_group_address = 
	     t->trans(&(instr->base_group_address()));
        zsuif_field_symbol* field = t->trans(instr->field());
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Field_access_instruction
	     (base_group_address, field, result_type, destination_op);
    }
    

    void handle_extract_fields_instruction(extract_fields_instruction *instr) {
	 zsuif_source_op* base_group_op =
	      t->trans(&(instr->base_group_op()));
	 zsuif_field_dst* field_dst = NULL ;
	 zsuif_field_dst_list* field_dsts = NULL;

	 /* cons things on backward so idx 0 is first */
	 s_count_t num_fields = instr->num_fields();
	 while(num_fields--) {
	      zsuif_type_id* typ =
		   t->trans(instr->result_type(num_fields));
	      zsuif_destination_op* dst =
		   t->trans(&(instr->get_destination_op(num_fields)));
	      zsuif_field_symbol* fs =
		   t->trans(instr->field(num_fields));
	      field_dst = new zsuif_field_dst(typ,dst,fs);

	      /* treat first one specially */
	      if(num_fields > 0) {
		   field_dsts = new zsuif_field_dst_list(field_dst,field_dsts);
	      }
	 }
	 
	 zinstr = new zsuif_Extract_fields_instruction
	      (base_group_op, field_dst, field_dsts);
    }

    void handle_set_fields_instruction(set_fields_instruction *instr) {
	 zsuif_source_op* base_group_op = 
	      t->trans(&(instr->base_group_op()));
	 zsuif_type_id* result_type =
	      t->trans(instr->result_type(0));
	 zsuif_destination_op* destination_op = 
	      t->trans(&(instr->get_destination_op()));

	 zsuif_field_src* field_src = NULL ;
	 zsuif_field_src_list* field_srcs = NULL;
	 /* cons things on backward so idx 0 is first */
	 s_count_t num_fields = instr->num_fields();
	 while(num_fields--) {
	      zsuif_source_op* src =
		   t->trans(&(instr->field_op(num_fields)));
	      zsuif_field_symbol* fs =
		   t->trans(instr->field(num_fields));
	      field_src = new zsuif_field_src(fs,src);

	      /* treat first one specially */
	      if(num_fields > 0) {
		   field_srcs = new zsuif_field_src_list(field_src,field_srcs);
	      }
	 }
        zinstr = new zsuif_Set_fields_instruction
	     (base_group_op, field_src,field_srcs, 
	      result_type, destination_op);
    }

    void handle_extract_elements_instruction
	 (extract_elements_instruction *instr) {
	 zsuif_source_op* base_array_op = 
	      t->trans(&(instr->base_array_op()));
	 zsuif_element_dst* element_dst = NULL;
	 zsuif_element_dst_list* element_dsts = NULL;
	 s_count_t num_elements = instr->num_elements();

	 while(num_elements--) {
	      zsuif_source_op* idx_op = 
		   t->trans(&(instr->index(num_elements)));
	      zsuif_destination_op* dst_op = 
		   t->trans(&(instr->get_destination_op(num_elements)));
	      zsuif_type_id* res_typ = 
		   t->trans(instr->result_type(num_elements));
	      
	      element_dst = 
		   new zsuif_element_dst(res_typ, dst_op, idx_op);

	      /* treat first one specially */
	      if(num_elements > 0) {
		   element_dsts = 
			new zsuif_element_dst_list(element_dst,element_dsts);
	      }
	 } 
	 zinstr =  new zsuif_Extract_elements_instruction
	      (base_array_op, element_dst, element_dsts);
    }
    
    void handle_set_elements_instruction(set_elements_instruction *instr) {
	 zsuif_source_op* base_array_op = 
	      t->trans(&(instr->base_array_op()));
	 zsuif_element_src* element_src = NULL;
	 zsuif_element_src_list* element_srcs = NULL;
	 s_count_t num_elements = instr->num_elements();

	 while(num_elements--) {
	      zsuif_source_op* idx_op = 
		   t->trans(&(instr->index(num_elements)));
	      zsuif_source_op* elm_op = 
		   t->trans(&(instr->element_op(num_elements)));
	      
	      element_src = 
		   new zsuif_element_src(idx_op, elm_op);

	      /* treat first one specially */
	      if(num_elements > 0) {
		   element_srcs = 
			new zsuif_element_src_list(element_src,element_srcs);
	      }
	 } 
	 zinstr =  new zsuif_Set_elements_instruction
	      (base_array_op, element_src, element_srcs);
    }

    void handle_bit_size_of_instruction(bit_size_of_instruction *instr) {
	 zsuif_type_id* ref_type = t->trans(instr->ref_type());
	 zsuif_type_id* result_type = t->trans(instr->result_type(0));
	 zsuif_destination_op* destination_op = 
	      t->trans(&(instr->get_destination_op()));
	 
	 zinstr = new zsuif_Bit_size_of_instruction
	      (ref_type, result_type, destination_op);
    }
    
    void handle_bit_alignment_of_instruction
	 (bit_alignment_of_instruction *instr) {
	 zsuif_type_id* ref_type = t->trans(instr->ref_type());
	 zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Bit_alignment_of_instruction
	     (ref_type, result_type, destination_op);
    }

    void handle_bit_offset_of_instruction(bit_offset_of_instruction *instr) {
        zsuif_field_symbol* field = t->trans(instr->field());
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));
	
        zinstr = new zsuif_Bit_offset_of_instruction
	     (field, result_type, destination_op);
    }

    void handle_byte_size_of_instruction(byte_size_of_instruction *instr) {
        zsuif_type_id* ref_type = t->trans(instr->ref_type());
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Byte_size_of_instruction
	     (ref_type, result_type, destination_op);
    }

    void handle_byte_alignment_of_instruction
	 (byte_alignment_of_instruction *instr) {
        zsuif_type_id* ref_type = t->trans(instr->ref_type());
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Byte_alignment_of_instruction
	     (ref_type, result_type, destination_op);
    }

    void handle_byte_offset_of_instruction(byte_offset_of_instruction *instr) {
        zsuif_field_symbol* field = t->trans(instr->field());
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Byte_offset_of_instruction
	     (field, result_type, destination_op);
    }

    void handle_va_arg_instruction(va_arg_instruction *instr) {
        zsuif_source_op* ap_address = t->trans(&(instr->ap_address()));
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Va_arg_instruction
	     (ap_address, result_type, destination_op);
    }

    void handle_sc_and_instruction(sc_and_instruction *instr) {
        zsuif_source_op* source1 = t->trans(&(instr->source1()));
        zsuif_source_op* source2 = t->trans(&(instr->source2()));
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Sc_and_instruction
	     (source1, source2, result_type, destination_op);
    }

    void handle_sc_or_instruction(sc_or_instruction *instr) {
        zsuif_source_op* source1 = t->trans(&(instr->source1()));
        zsuif_source_op* source2 = t->trans(&(instr->source2()));
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Sc_or_instruction
	     (source1, source2, result_type, destination_op);
    }

    void handle_sc_select_instruction(sc_select_instruction *instr) {
        zsuif_source_op* selector = t->trans(&(instr->selector()));
        zsuif_source_op* selection1 = t->trans(&(instr->selection1()));
        zsuif_source_op* selection2 = t->trans(&(instr->selection2()));
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Sc_select_instruction
	     (selector, selection1, selection2, result_type, destination_op);
    }

    void handle_load_instruction(load_instruction *instr) {
        zsuif_source_op* source_address = t->trans(&(instr->source_address()));
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Load_instruction
	     (source_address, result_type, destination_op);
    }

    void handle_load_address_instruction(load_address_instruction *instr) {
	 zsuif_symbol* addressed_symbol = t->trans(instr->addressed_symbol());
	 zsuif_type_id* result_type = t->trans(instr->result_type(0));
	 zsuif_destination_op* destination_op = 
	      t->trans(&(instr->get_destination_op()));

	 zinstr = new zsuif_Load_address_instruction
	      (addressed_symbol, result_type, destination_op);
    }

    void handle_load_constant_instruction(load_constant_instruction *instr) {
        zsuif_constant* constant = t->trans(&(instr->get_constant()));
        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));

        zinstr = new zsuif_Load_constant_instruction
	     (constant, result_type, destination_op);
    }


    void handle_load_value_block_instruction
	 (load_value_block_instruction *instr) {
	 zsuif_value_block* value_block = t->trans(instr->get_value_block());
	 zsuif_type_id* result_type = t->trans(instr->result_type(0));
	 zsuif_destination_op* destination_op = 
	      t->trans(&(instr->get_destination_op()));
	 
        zinstr = new zsuif_Load_value_block_instruction
	     (value_block, result_type, destination_op);
    }


    void handle_call_instruction(call_instruction *instr) {

        zsuif_source_op* callee_address = 
	     t->trans(&(instr->callee_address()));
	zsuif_source_op_list* arguments = NULL;
	zsuif_return_value_list* return_values = NULL;

        s_count_t num_arguments    = instr->num_arguments();
	/* cons things on backward so idx 0 is first */
	while(num_arguments--) {
	     zsuif_source_op* argument = 
		  t->trans(&(instr->argument(num_arguments)));
	     arguments = 
		  new zsuif_source_op_list(argument,arguments);
	}
	/* cons things on backward so idx 0 is first */
        s_count_t num_destinations = instr->num_destination_ops();
	while(num_destinations--) {
	     zsuif_destination_op* dst_op = 
		  t->trans(&(instr->get_destination_op(num_destinations)));
	     zsuif_type_id* res_typ = 
		  t->trans(instr->result_type(num_destinations));
	     zsuif_return_value* return_value = 
		  new zsuif_return_value(dst_op, res_typ);

	     return_values =
		  new zsuif_return_value_list(return_value, return_values);
	}
        zinstr = new zsuif_Call_instruction
	     (callee_address, arguments, return_values);
    }

    void handle_ssa_phi_instruction(ssa_phi_instruction *instr) {

        zsuif_type_id* result_type = t->trans(instr->result_type(0));
        zsuif_destination_op* destination_op = 
	     t->trans(&(instr->get_destination_op()));
	zsuif_variable_symbol_list* variables = NULL;
	
	/* cons things on backward so idx 0 is first */
        s_count_t num_merged = instr->num_merged();
	while(num_merged--) {
	     zsuif_variable_symbol* variable = 
		  t->trans(instr->merged_variable(num_merged));
	     variables = 
		  new zsuif_variable_symbol_list(variable,variables);
	}
        zinstr = new zsuif_Ssa_phi_instruction
	     (variables, result_type, destination_op);
    }

    void handle_mark_instruction(mark_instruction *instr) {
        zinstr = new zsuif_Mark_instruction();
    }



    void handle_assert_instruction(assert_instruction *instr) {
	 zsuif_source_op* asserted_value = 
	      t->trans(&(instr->asserted_value()));
	 zinstr = new zsuif_Assert_instruction(asserted_value);
    }

    void handle_branch_instruction(branch_instruction *instr) {
	 zsuif_source_op* decision_operand = 
	      t->trans(&(instr->decision_operand()));
	 zsuif_code_label_symbol* target = t->trans(instr->target());

	 if(instr->opcode() == k_branch_if_true) {
	      zinstr = new zsuif_Branch_true_instruction
		   (decision_operand, target);
	 } else if(instr->opcode() == k_branch_if_false) {
	      zinstr = new zsuif_Branch_false_instruction
		   (decision_operand, target);
	 } else {
	      error(-1,"Bad branch opcode");
	 }
    }

    void handle_jump_indirect_instruction(jump_indirect_instruction *instr) {
	 zsuif_source_op* target = t->trans(&(instr->target()));

	 zinstr = new zsuif_Jump_indirect_instruction(target);
    }

    void handle_jump_instruction(jump_instruction *instr) {
	 zsuif_code_label_symbol* target = t->trans(instr->target());

	 zinstr = new zsuif_Jump_instruction(target);
    }

    void handle_multi_way_branch_instruction
	 (multi_way_branch_instruction *instr) {
	 zsuif_source_op* decision_operand = 
	      t->trans(&(instr->decision_operand()));
	 zsuif_code_label_symbol* default_target = 
	      t->trans(instr->default_target());

	 /* cons things on backward so idx 0 is first */
	 zsuif_multi_way_branch_case_list* cases = NULL;
	 s_count_t enumeration_count = instr->enumeration_count();
	 while(enumeration_count--) {
	      zsuif_constant* case_constant = 
		   t->trans(&(instr->case_constant(enumeration_count)));

	      zsuif_code_label_symbol* case_target =
		   t->trans(instr->case_target(enumeration_count));

	      zsuif_multi_way_branch_case* arm =
		   new zsuif_multi_way_branch_case(case_constant, case_target);

	      cases = new zsuif_multi_way_branch_case_list(arm,cases);
	 }

	 zinstr = new zsuif_Multi_way_branch_instruction
	      (decision_operand, default_target, cases);
    }


    void handle_store_instruction(store_instruction *instr) {
	 zsuif_source_op* data_operand = t->trans(&(instr->data_operand()));
	 zsuif_source_op* destination_address = 
	      t->trans(&(instr->destination_address()));

	 zinstr = new zsuif_Store_instruction
	      (data_operand, destination_address);
    }

    void handle_va_end_instruction(va_end_instruction *instr) {
	 zsuif_source_op* ap_address = t->trans(&(instr->ap_address()));

	 zinstr = new zsuif_Va_end_instruction(ap_address);
    }

    void handle_va_start_old_instruction(va_start_old_instruction *instr) {
	 zsuif_source_op* ap_address = t->trans(&(instr->ap_address()));

	 zinstr = new zsuif_Va_start_old_instruction(ap_address);
    }

    void handle_va_start_instruction(va_start_instruction *instr) {
	 zsuif_source_op* ap_address = t->trans(&(instr->ap_address()));
	 zsuif_parameter_symbol* parmn = t->trans(instr->parmn());

	 zinstr = new zsuif_Va_start_instruction(ap_address, parmn);
    }


};



