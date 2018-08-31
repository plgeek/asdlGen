class trans_statement : public suif_visitor  {
private:
     zsuif_statement* zstmt;
     trans_suif* t;
public:
     
    trans_statement(trans_suif* t,  cfo *c)  { 
    	 this->t = t;
	 zstmt  = NULL;
	 if(c) c->apply_pyg_visitor(this);
    }
    trans_statement(trans_suif* t, statement *s) { 
	 this->t = t;
	 zstmt = NULL;
	 if(s) s->apply_pyg_visitor(this);
    }
    
    zsuif_statement* answer(void) {
	 if(zstmt) {
	      return zstmt;
	 } else {
	      return new zsuif_Nop_statement();
	 }
    }
    
    void handle_jump_cfgn(jump_cfgn *jump_cfgn) { 
	 handle_cfg_node(jump_cfgn); }

    void handle_jump_indirect_cfgn(
	 jump_indirect_cfgn *jump_indirect_cfgn)
	 { handle_cfg_node(jump_indirect_cfgn); }
    
    void handle_branch_cfgn(branch_cfgn *branch_cfgn)
	 { handle_cfg_node(branch_cfgn); }
    
    void handle_multi_way_branch_cfgn(
	 multi_way_branch_cfgn *multi_way_branch_cfgn)
	 { handle_cfg_node(multi_way_branch_cfgn); }
    
    void handle_no_exit_cfgn(no_exit_cfgn *no_exit_cfgn)
	 { handle_cfg_node(no_exit_cfgn); }

    void handle_statement_list(statement_list *stmts) {
	 zstmt = new zsuif_Sequence_statement(t->trans(stmts));
    }
    
    void handle_assert_statement(assert_statement *stmt) {
	 zsuif_source_op* asserted_value = 
	      t->trans(&(stmt->asserted_value()));

	 zstmt = new zsuif_Assert_statement(asserted_value);
    }

    void handle_branch_statement(branch_statement *stmt) {
	 zsuif_source_op* decision_operand = 
	      t->trans(&(stmt->decision_operand()));
	 zsuif_code_label_symbol* target = t->trans(stmt->target());

	 if(stmt->opcode() == k_branch_if_true) {
	      zstmt = new zsuif_Branch_true_statement
		   (decision_operand, target);
	 } else if(stmt->opcode() == k_branch_if_false) {
	      zstmt = new zsuif_Branch_false_statement
		   (decision_operand, target);
	 } else {
	      error(-1,"Bad branch opcode");
	 }
    }

    void handle_do_while_statement(do_while_statement *stmt) {
	 zsuif_source_op* condition = t->trans(&(stmt->condition()));
	 zsuif_statement* body = t->trans(stmt->body());
	 zsuif_code_label_symbol* break_label = t->trans(stmt->break_label());
	 zsuif_code_label_symbol* continue_label = 
	      t->trans(stmt->continue_label());

	 zstmt = new zsuif_Do_while_statement
	      (condition, body, break_label, continue_label);
    }


    void handle_eval_statement(eval_statement *stmt) {
	 zsuif_instruction_list* instrs = NULL;
	 s_count_t num_instructions = stmt->num_instructions();

	 /* cons thing on in reverse so the idx 0 is the first in the list */
	 while(num_instructions--) {
	      zsuif_instruction * instr = 
		   t->trans(stmt->get_instruction(num_instructions));
	      instrs = new zsuif_instruction_list(instr,instrs);
	 }
	 zstmt = new zsuif_Eval_statement(instrs);
    }

    void handle_for_statement(for_statement *stmt) {
	 zsuif_variable_symbol* index = t->trans(stmt->index());
	 zsuif_source_op* lower_bound = t->trans(&(stmt->lower_bound()));
	 zsuif_source_op* upper_bound = t->trans(&(stmt->upper_bound()));
	 zsuif_source_op* step = t->trans(&(stmt->step()));
	 zsuif_binop* comparison_opcode = 
	      t->get_cmpop(stmt->comparison_opcode());

	 zsuif_statement* body = t->trans(stmt->body());
	 zsuif_statement* pre_pad = t->trans(stmt->pre_pad());
	 zsuif_statement* post_pad = t->trans(stmt->post_pad());

	 zsuif_code_label_symbol* break_label = t->trans(stmt->break_label());
	 zsuif_code_label_symbol* continue_label = 
	      t->trans(stmt->continue_label());

	 zstmt = new zsuif_For_statement
	      (index, lower_bound, upper_bound, step, 
	       comparison_opcode, body, pre_pad, 
	       post_pad, break_label, continue_label);
    }

    void handle_if_statement(if_statement *stmt) {
	 zsuif_source_op* condition = t->trans(&(stmt->condition()));
	 zsuif_statement* then_part = t->trans(stmt->then_part());
	 zsuif_statement* else_part = t->trans(stmt->else_part());

	 zstmt = new zsuif_If_statement(condition, then_part, else_part);
    }

    void handle_jump_indirect_statement(jump_indirect_statement *stmt) {
	 zsuif_source_op* target = t->trans(&(stmt->target()));

	 zstmt = new zsuif_Jump_indirect_statement(target);
    }

    void handle_jump_statement(jump_statement *stmt) {
	 zsuif_code_label_symbol* target = t->trans(stmt->target());

	 zstmt = new zsuif_Jump_statement(target);
    }

    void handle_label_location_statement(label_location_statement *stmt) {
	 zsuif_code_label_symbol* defined_label = 
	      t->trans(stmt->defined_label());

	 zstmt = new zsuif_Label_location_statement(defined_label);
    }

    void handle_mark_statement(mark_statement *stmt) {

	 zstmt = new zsuif_Mark_statement();
    }

    void handle_multi_way_branch_statement(multi_way_branch_statement *stmt) {
	 zsuif_source_op* decision_operand = 
	      t->trans(&(stmt->decision_operand()));
	 zsuif_code_label_symbol* default_target = 
	      t->trans(stmt->default_target());

	 /* cons things on backward so idx 0 is first */
	 zsuif_multi_way_branch_case_list* cases = NULL;
	 s_count_t enumeration_count = stmt->enumeration_count();
	 while(enumeration_count--) {
	      zsuif_constant* case_constant = 
		   t->trans(&(stmt->case_constant(enumeration_count)));

	      zsuif_code_label_symbol* case_target =
		   t->trans(stmt->case_target(enumeration_count));

	      zsuif_multi_way_branch_case* arm =
		   new zsuif_multi_way_branch_case(case_constant, case_target);
	      cases = new zsuif_multi_way_branch_case_list(arm,cases);
	 }

	 zstmt = new zsuif_Multi_way_branch_statement
	      (decision_operand, default_target, cases);
    }

    void handle_return_statement(return_statement *stmt) {

	 /* cons things on backward so idx 0 is first */
	 zsuif_source_op_list* return_values = NULL;
	 s_count_t num_return_values = stmt->num_return_values();

	 while(num_return_values--) {
	      zsuif_source_op* return_value = 
		   t->trans(&(stmt->return_value(num_return_values)));
	      return_values = 
		   new zsuif_source_op_list(return_value,return_values);
	      }

	 zstmt = new zsuif_Return_statement(return_values);
    }

    void handle_scope_statement(scope_statement *stmt) {
	 zsuif_statement* body = t->trans(stmt->body());
	 zsuif_definition_block* definition_block = 
	      t->trans(stmt->get_definition_block());
	 t->do_table(stmt->get_symbol_table());

	 zstmt = new zsuif_Scope_statement
	      (body, definition_block);
    }

    void handle_store_statement(store_statement *stmt) {
	 zsuif_source_op* data_operand = t->trans(&(stmt->data_operand()));
	 zsuif_source_op* destination_address = 
	      t->trans(&(stmt->destination_address()));

	 zstmt = new zsuif_Store_statement(data_operand, destination_address);
    }

    void handle_va_end_statement(va_end_statement *stmt) {
	 zsuif_source_op* ap_address = t->trans(&(stmt->ap_address()));

	 zstmt = new zsuif_Va_end_statement(ap_address);
    }

    void handle_va_start_old_statement(va_start_old_statement *stmt) {
	 zsuif_source_op* ap_address = t->trans(&(stmt->ap_address()));

	 zstmt = new zsuif_Va_start_old_statement(ap_address);
    }

    void handle_va_start_statement(va_start_statement *stmt) {
	 zsuif_source_op* ap_address = t->trans(&(stmt->ap_address()));
	 zsuif_parameter_symbol* parmn = t->trans(stmt->parmn());

	 zstmt = new zsuif_Va_start_statement(ap_address, parmn);
    }

    void handle_while_statement(while_statement *stmt) {
	 zsuif_source_op* condition = t->trans(&(stmt->condition()));
	 zsuif_statement* body = t->trans(stmt->body()); 
	 zsuif_code_label_symbol* break_label = 
	      t->trans(stmt->break_label()); 
	 zsuif_code_label_symbol*
	      continue_label = t->trans(stmt->continue_label());
	 
	 zstmt = new zsuif_While_statement
	      (condition, body, break_label, continue_label); 
    }

};


