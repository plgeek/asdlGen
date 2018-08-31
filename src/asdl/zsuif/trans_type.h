class trans_type : public suif_visitor {
private:
  trans_suif    *trans;
  type          *t;
  zsuif_type    *typ;
  zsuif_procedure_type *proc_typ;
  zsuif_type_id *tid;
public:

  trans_type(trans_suif* trans_suif, type* t) {
    this->trans = trans_suif;
    this->t     = t;
    this->typ   = NULL;
  }

  zsuif_type_id *get_type_id(void) {
    if(!(trans->in_table(t))) {
      tid = trans->make_type_id(t);
      t->apply_pyg_visitor(this);
      trans->add_entry(new zsuif_type_table_entry(tid,typ));
    } else {
      tid = trans->make_type_id(t);
    }
    return tid;
  }

  zsuif_type *get_type(void) {
    if(!(trans->in_table(t))) {
      tid = trans->make_type_id(t);
      t->apply_pyg_visitor(this);
      trans->add_entry(new zsuif_type_table_entry(tid,typ));
    } else {
      /* TODO: should search type table for existing rahter than 
         rebuilding the type */
      t->apply_pyg_visitor(this);
    }
    return typ;
  }
  zsuif_qualification_list *get_qualifications(void) {
    zsuif_type* tmp_typ = get_type();

    switch(tmp_typ->kind()) {
    case zsuif_type::zsuif_Qualified_enum: {
      zsuif_Qualified *qual = (zsuif_Qualified*)tmp_typ;
      return qual->qualifications;
    }
    default:
      return NULL;
    }
      
  }

  zsuif_procedure_type *get_procedure_type(void) {
    zsuif_type* tmp_typ = get_type();
    while(tmp_typ) {
      switch(tmp_typ->kind()) {
	/* unqualifed procedure */
      case zsuif_type::zsuif_Procedure_enum: {
	zsuif_Procedure *proc = (zsuif_Procedure*)tmp_typ;
	return proc->procedure_type1;
      }
      case zsuif_type::zsuif_Qualified_enum: {
	zsuif_Qualified *qual = (zsuif_Qualified*)tmp_typ;
	tmp_typ = qual->type;
	break;
      }
      default:
	error(-1,"get_procedure on non procedure type");
	return NULL;
      }
    }
  }

  /* fix me */
  void handle_type(type* t) { 
       typ = new zsuif_Void(); 
  }

  void handle_qualified_type(qualified_type* t) { 
       trans_type tmp(trans,t->get_unqualified_type());
       zsuif_type* utyp = tmp.get_type();
       s_count_t num_qualifications = t->num_qualifications();
       zsuif_qualification_list* qualifications = NULL;
       while(num_qualifications--) {
	    zsuif_qualification* q = 
		 new zsuif_qualification
		 (t->get_qualification(num_qualifications));
	    qualifications = 
		 new zsuif_qualification_list(q,qualifications);
       }
       typ = new zsuif_Qualified(qualifications,utyp);
  }

  void handle_basic_procedure_type(basic_procedure_type* p) { 
       zsuif_int_or_source_op* bit_size = trans->get_type_size(p);
       zsuif_int_or_source_op* bit_alignment = trans->get_type_alignment(p); 


       zsuif_type_list* args = NULL;
       s_count_t num_arguments = p->num_arguments();
       while(num_arguments--) {
	    trans_type arg(trans,p->argument(num_arguments));
	    args = new zsuif_type_list(arg.get_type(),args);
       }

       zsuif_type_list* result_types = NULL;
       s_count_t num_results = p->num_results();
       while(num_results--) {
	    trans_type res(trans,p->result(num_results));
	    result_types = 
		 new zsuif_type_list(res.get_type(),result_types);
       }

       zsuif_procedure_type *pt =
	    new zsuif_Basic_procedure_type(bit_size,
					   bit_alignment,
					   result_types,args);
       typ = new zsuif_Procedure(pt);
  }

  void handle_boolean_type(boolean_type *x) {
       zsuif_int_or_source_op* bit_size = trans->get_type_size(x);
       zsuif_int_or_source_op* bit_alignment = trans->get_type_alignment(x); 

       typ =  new zsuif_Data(new zsuif_Boolean_type(bit_size,bit_alignment));
  }

  void handle_integer_type(integer_type *x) {
       zsuif_int_or_source_op* bit_size = trans->get_type_size(x);
       zsuif_int_or_source_op* bit_alignment = trans->get_type_alignment(x); 

       if(x->is_signed()) {
	 typ =  new zsuif_Data(new zsuif_Integer_type(bit_size,bit_alignment));
       } else {
	 typ = 
	   new zsuif_Data(new zsuif_UInteger_type(bit_size,bit_alignment));
       }
  }


  void handle_floating_point_type(floating_point_type *x) {
       zsuif_int_or_source_op* bit_size = trans->get_type_size(x);
       zsuif_int_or_source_op* bit_alignment = trans->get_type_alignment(x); 

       typ =  new zsuif_Data(new zsuif_Floating_point_type
			     (bit_size,bit_alignment));
  }

  void handle_pointer_type(pointer_type *x) {
       zsuif_int_or_source_op* bit_size = trans->get_type_size(x);
       zsuif_int_or_source_op* bit_alignment = trans->get_type_alignment(x); 
       zsuif_type_id *ref_type = trans->trans(x->reference_type());
	
       typ =  new zsuif_Data(new zsuif_Pointer_type
			     (bit_size, bit_alignment,ref_type));
       
  }

  void handle_enumerated_type(enumerated_type *x) {
       zsuif_int_or_source_op* bit_size = trans->get_type_size(x);
       zsuif_int_or_source_op* bit_alignment = trans->get_type_alignment(x); 

       zsuif_enumerate_case_list* enum_cases = NULL;
       s_count_t num_cases = x->num_cases();
       while(num_cases--) {
	    zsuif_enumerate_case* enum_case =
		 new zsuif_enumerate_case
		 (x->case_name(num_cases),
		  trans->trans(x->case_constant(num_cases)));
	    enum_cases = 
		 new zsuif_enumerate_case_list(enum_case, enum_cases);
       }
       
       typ = new zsuif_Data
	    (new zsuif_Enumerated_type(bit_size, bit_alignment,
				       x->name(), enum_cases));
  }
  
  void handle_array_type(array_type *x) {
       zsuif_int_or_source_op* bit_size = 
	    trans->get_type_size(x); 

       zsuif_int_or_source_op* bit_alignment = 
	    trans->trans(&(x->bit_alignment_const_or_op()));

       zsuif_int_or_source_op* lower_bound =
	    trans->trans_opt(&(x->lower_bound_const_or_op()));

       zsuif_int_or_source_op* upper_bound =
	    trans->trans_opt(&(x->upper_bound_const_or_op()));

       trans_type et(trans,x->element_type());
       zsuif_type *element_type = et.get_type();

       typ = new zsuif_Data
	    (new zsuif_Array_type(bit_size, bit_alignment,
			    element_type, lower_bound, upper_bound));
  }

  void handle_group_type(group_type *x) {
       zsuif_int_or_source_op* bit_size = trans->get_type_size(x);
       zsuif_int_or_source_op* bit_alignment = trans->get_type_alignment(x); 

       zsuif_group_field_list* group_fields = NULL;
       s_count_t num_fields = x->num_fields();
       while(num_fields--) {
	    zsuif_field_symbol* name =
		 trans->trans(x->field(num_fields));

	    trans_type ft(trans,x->field_type(num_fields));
	    zsuif_type* field_type = ft.get_type();

	    zsuif_int_or_source_op* bit_offset =
		 trans->get_field_offset(x,num_fields);
	    zsuif_group_field* group_field =
		 new zsuif_group_field(name,field_type,bit_offset);

	    group_fields = 
		 new zsuif_group_field_list(group_field, group_fields);
       }
       
       typ = new zsuif_Data
	    (new zsuif_Group_type(bit_size, bit_alignment,
				  x->name(), group_fields));
       
  }


};

