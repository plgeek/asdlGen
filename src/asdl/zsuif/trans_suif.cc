#include <assert.h>
#include <stdlib.h>
#include "trans_suif.h"
#include "trans_type.h"
#include "trans_statement.h"
#include "trans_instruction.h"
static lstring zsuif_atag_symb = lstring("/trans_suif/sid");
static lstring zsuif_atag_type = lstring("/trans_suif/tid");

/*****************************************/
trans_suif::trans_suif(void) { 

  next_symb_id = 1;
  next_type_id = 1;
  state = NORMAL;

  (void)MP_set(64);
  null_symb =
    new zsuif_symbol(0,lstring("NullSymbol"));
  
  null_type = 
    new zsuif_type_id(0);
  
  type_table_entries = NULL;
  symbol_table_entries = NULL;
  extern_symbol_table_entries = NULL;
  file_blocks = NULL;
  information_block = zsuif_C_information_block;
}

/*****************************************/
/* code to annotate symbols with their unqiue ids */
boolean trans_suif::in_table(symbol* s) {

  annote *a = s->peek_annote(zsuif_atag_symb);
  return (a ? TRUE : FALSE);
}

zsuif_symbol* trans_suif::make_symb(symbol* s) {

  if(s) {
    /* get a  symbol to keep track of identity */
    external_pointer_annote<zsuif_symbol> *a = 
      (external_pointer_annote<zsuif_symbol>*)
      s->peek_annote(zsuif_atag_symb);
    
    if(a == NULL) { 
      /* seen for the first time */
      identifier name = lstring(s->name());
      int uid = next_symb_id++;
      a = new external_pointer_annote<zsuif_symbol>
	(zsuif_atag_symb,new zsuif_symbol(uid,name));
      s->append_annote(a);
    }
    return a->get_pointer();
  } else {
    return null_symb;
  }
    
}

zsuif_symbol* trans_suif::add_entry(zsuif_symbol_table_entry *e) {
  /* just cons it on the list */
  symbol_table_entries =
    new zsuif_symbol_table_entry_list(e,symbol_table_entries);
  return e->key;
}
zsuif_symbol* trans_suif::add_entry_extern(zsuif_symbol_table_entry *e) {
  /* just cons it on the list */
  extern_symbol_table_entries =
    new zsuif_symbol_table_entry_list(e,extern_symbol_table_entries);
  return e->key;
}
/*****************************************/
boolean trans_suif::in_table(type* s) {
  if(s) {
    annote *a = s->peek_annote(zsuif_atag_type);
    return (a ? TRUE : FALSE);
  } else {
    return TRUE;
  }
}
zsuif_type_id* trans_suif::make_type_id(type* s) {
  if(s) {
    /* get a unqiue id for a use annotations to keep track of identity */
    external_pointer_annote<zsuif_type_id> *a = 
      (external_pointer_annote<zsuif_type_id>*)
      s->peek_annote(zsuif_atag_type);
    if(a == NULL) { 
      /* seen for the first time */
      int uid = next_type_id++;
      a = new external_pointer_annote<zsuif_type_id>
	(zsuif_atag_type,new zsuif_type_id(uid));
      s->append_annote(a);
    }
    return a->get_pointer();
  } else {
    return null_type;
  }
}

zsuif_type_id* trans_suif::add_entry(zsuif_type_table_entry *e) {
  /* just cons it on the list */
  type_table_entries =
    new zsuif_type_table_entry_list(e,type_table_entries);
  return e->key;
}

void trans_suif::init_entry_attribs(zsuif_symbol_table_entry* e,
				    symbol* s) {
  e->key = make_symb(s);
  e->address_taken = (s->is_address_taken() ? StdTypes_TRUE : StdTypes_FALSE);
}
/*****************************************/
zsuif_int_or_source_op* trans_suif::get_type_size(type *t) {
  if((t->bit_size_const()).is_finite()) {
    return new zsuif_Int((t->bit_size_const()).c_int());
  } else {
    return new  zsuif_SrcOp(trans(&(t->bit_size_op())));
  }
}
/*****************************************/
zsuif_int_or_source_op* trans_suif::get_type_alignment(type *t) {
  if((t->bit_alignment_const()).is_finite()) {
    return new zsuif_Int((t->bit_alignment_const()).c_int());
  } else {
    return new  zsuif_SrcOp(trans(&(t->bit_alignment_op())));
  }
}
/*****************************************/
zsuif_int_or_source_op* trans_suif::get_field_offset(group_type *t,
						     s_count_t idx) {
  if((t->field_bit_offset_const(idx)).is_finite()) {
    return new zsuif_Int((t->field_bit_offset_const(idx)).c_int());
  } else {
    return new  zsuif_SrcOp(trans(&(t->field_bit_offset_op(idx))));
  }
}
/*****************************************/
zsuif_suif_int* trans_suif::trans(i_integer i) {
  if (i.is_finite()) {
    char  buf[256];
    char *p = buf;
    char *end;
    MP_T  sign;
    MP_T  res, final;

    i.write(&buf[0]);

    sign = MP_new(1);
    if (buf[0] == '-') {
      sign = MP_fromint(sign, -1);
      p++;
    }
    res = MP_new(0);
    res = MP_fromstr(res, p, 10, &end);

    final = MP_mul(MP_new(0), sign, res);

    free(sign); free(res);

    return new zsuif_Finite(final);
  }
  if (i.is_undetermined()) {
    return new zsuif_Undetermined();
  }
  if (i == (i_signless_infinity())) {
    return new zsuif_UnsignedInf();
  }
  if (i == (i_negative_infinity())) {
    return new zsuif_NegInf();
  }
  if (i == (i_positive_infinity())) {
    return new zsuif_PlusInf();
  }
}

/*****************************************/
zsuif_variable_symbol* trans_suif::trans(variable_symbol* s) {
 return new zsuif_variable_symbol(trans((symbol*)s));
}

/*****************************************/
zsuif_procedure_symbol* trans_suif::trans(procedure_symbol* s) {
   return new zsuif_procedure_symbol(trans((symbol*)s));
}

/*****************************************/
/* gone in suif
zsuif_register_symbol* trans_suif::trans(register_symbol* s) {
  return new zsuif_register_symbol(trans((symbol*)s));
}
*/
/*****************************************/
zsuif_field_symbol* trans_suif::trans(field_symbol* s) {
  return new zsuif_field_symbol(trans((symbol*)s));

}
/*****************************************/
zsuif_parameter_symbol* trans_suif::trans(parameter_symbol* s) {
  if(in_table(s)) {
    return new zsuif_parameter_symbol(make_symb(s));
  } else {
    zsuif_parameter_symbol* name =  new zsuif_parameter_symbol(make_symb(s));
    zsuif_int_or_source_op* bit_alignment =  
      get_type_alignment(s->get_type());
    zsuif_type_id* type = trans(s->get_type());
    zsuif_procedure_symbol *proc = 
      trans((s->get_procedure_definition())->get_procedure_symbol());
    zsuif_symbol_table_entry* e = 
      new zsuif_ParameterEntry(name,bit_alignment,type,proc);

    init_entry_attribs(e,s);
    add_entry(e);
    return name;
  }
}
/*****************************************/
/* walk over the symbol table and dump results into them */
class trans_symbol : public suif_visitor {
  trans_suif * trans;
  zsuif_symbol* zsymb;
  symbol* symb;
public:
  trans_symbol(trans_suif* trans,symbol* s) {
    this->trans = trans;
    this->zsymb = NULL;
    this->symb = s;

  }
  
  zsuif_symbol* answer(void) {
    if(trans->in_table(symb)) {
      return trans->make_symb(symb);
    }
    symb->apply_pyg_visitor(this);
    if(zsymb) {
      return zsymb;
    } else {
      /*      error(-1,"Bad symbol\n"); */
      return NULL;
    }
  }
  
  void return_entry(zsuif_symbol_table_entry* e, symbol* s) {
    trans->init_entry_attribs(e,s);
    trans->add_entry(e);
    zsymb = e->key;
  }

  void handle_procedure_symbol(procedure_symbol* s) { 
      procedure_symbol* ps = s;
      zsuif_procedure_definition* def;
      procedure_definition* pd = ((s->definition()).get());
      if(pd == NULL) { 
	zsuif_procedure_symbol* name =  
	  new zsuif_procedure_symbol(trans->make_symb(ps));
	trans_type typ(trans,ps->get_type());
	zsuif_procedure_type* procedure_type = typ.get_procedure_type();
	zsuif_qualification_list* qualifications = typ.get_qualifications();
	
	def = new zsuif_procedure_definition
	  (name, qualifications, procedure_type, NULL); 
      } else {
	trans_suif::trans_state hack = trans->state;
	trans->state = trans_suif::NORMAL;
	def = trans->trans(pd);
	trans->state = hack;
      }
      zsuif_symbol_table_entry* e = new zsuif_ProcedureEntry(def);
      if(trans->state != trans_suif::EXTERN_PROCS) {
	return_entry(e,s);
      } else {
	trans->init_entry_attribs(e,s);
	trans->add_entry_extern(e);
	zsymb = e->key;
      }
  }

  void handle_parameter_symbol(parameter_symbol* s) { 
    zsuif_parameter_symbol* name =  
      new zsuif_parameter_symbol(trans->make_symb(s));

    zsuif_int_or_source_op* bit_alignment =  
      trans->get_type_alignment(s->get_type());

    zsuif_type_id* type = trans->trans(s->get_type());

    zsuif_procedure_symbol *proc = 
      trans->trans((s->get_procedure_definition())->get_procedure_symbol());

    zsuif_symbol_table_entry* e = 
      new zsuif_ParameterEntry(name,bit_alignment,type,proc);

    return_entry(e,s);
  }

  void handle_code_label_symbol(code_label_symbol* s) { 
    zsuif_symbol_table_entry* e = new zsuif_CodeLabelEntry();
    return_entry(e,s);
  }

  void handle_field_symbol(field_symbol* s) { 
    zsuif_int_or_source_op* bit_offset = 
      trans->trans(&(s->bit_offset_const_or_op()));
    zsuif_symbol_table_entry* e = 
      new zsuif_FieldEntry(bit_offset);
    return_entry(e,s);
  }

  void handle_register_symbol(variable_symbol* s) { 
    zsuif_symbol_table_entry* e = 
      new zsuif_RegisterEntry(trans->trans(i_integer(32)));
    return_entry(e,s);
  }

  void handle_variable_symbol(variable_symbol* s) {
    variable_definition* vd = ((s->definition()).get());
    if(vd) {
	zsuif_variable_definition* def = trans->trans(vd);
	zsuif_symbol_table_entry* e = 
	  new zsuif_VariableEntry(def,StdTypes_FALSE);
	return_entry(e,s);
      } else {
	
	zsuif_variable_symbol* vs = 
	  new zsuif_variable_symbol(trans->make_symb(s));
	
	trans_type vtype(trans,s->get_type());
	zsuif_type* type = vtype.get_type();
	
	switch(trans->state) {
	case trans_suif::EXTERN_VARS: {
	  zsuif_variable_definition* def = 
	    new zsuif_variable_definition(vs,type,NULL);
	  
	  zsuif_symbol_table_entry* e = 
	    new zsuif_VariableEntry(def,StdTypes_FALSE);
	  
	  trans->init_entry_attribs(e,s);
	  trans->add_entry_extern(e);
	  zsymb = e->key;
	}  break;
	case trans_suif::NORMAL: {
	  zsuif_variable_definition* def = 
	    new zsuif_variable_definition(vs,type,NULL);
	  zsuif_symbol_table_entry* e =  
	    new zsuif_VariableEntry(def,StdTypes_TRUE);
	  return_entry(e,s);
	}
	break;
	case trans_suif::EXTERN_PROCS:
	  assert(0); break; /* should never happen */
	}
      }
    }
};

zsuif_symbol * trans_suif::trans(symbol* s) {
  if(s) {
    trans_symbol symb(this,s);
    return symb.answer();
  } else {
    return null_symb;
  }
}
/*****************************************/
zsuif_code_label_symbol* trans_suif::trans(code_label_symbol* s) {
  return new zsuif_code_label_symbol(trans((symbol*)s));
}
/*****************************************/
/* walk over the symbol table and dump results into them */
class trans_symbol_table : public suif_visitor {
  trans_suif * trans;
public:
  trans_symbol_table(trans_suif* trans,symbol_table* symtab) {
    this->trans = trans;
    ro_tos_ref<sto *> entries = symtab->stos();
    s_count_t num_entries = entries.count();
    int i = 0;
    for(i = 0; i < num_entries ; i++) {
      sto* entry = entries.elem_by_num(i);
      if(entry) {
	entry->apply_pyg_visitor(this);
      }
    }
  }
  void handle_procedure_symbol(procedure_symbol* s) { 
    /* when doing extern variables ignore procedures */
    if (trans->state != trans_suif::EXTERN_VARS) {   trans->trans(s); } 
  }
  void handle_parameter_symbol(parameter_symbol* s) { trans->trans(s); }
  void handle_code_label_symbol(code_label_symbol* s) { trans->trans(s); }
  void handle_field_symbol(field_symbol* s) { trans->trans(s); }
  void handle_register_symbol(variable_symbol* s) { trans->trans(s); }
  void handle_variable_symbol(variable_symbol* s) {
     /* don't deal with variables when doing extern procedures */
    if (trans->state != trans_suif::EXTERN_PROCS) {
      trans->trans(s); 
    }
  }

};
/*****************************************/
void trans_suif::do_table(symbol_table *s) {
  assert(s != NULL);
  trans_symbol_table trans_symbtab(this,s);
}
/*****************************************/
void trans_suif::handle_file_set_block(file_set_block* fbs) {
  assert(fbs != NULL);
  s_count_t num_blocks = fbs->file_block_count();

  /* we have to walk over the extern_symbol table twice 
     first to figure out which variables are extern, then
     a second time to handle procedures to work around a bug.
     This will get fixed in the future. (danwang)
  */
  state = EXTERN_VARS;
  do_table(fbs->get_external_symbol_table());

  state = EXTERN_PROCS;
  do_table(fbs->get_external_symbol_table());

  state = NORMAL;
  do_table(fbs->get_file_set_symbol_table());

  int i;
  for(i=0; i < num_blocks ; i++) {
    file_block *fb = ((fbs->get_file_block(i)).get());
    handle_file_block(fb);
  }
}
/*****************************************/
void trans_suif::handle_file_block(file_block *fb) {
  assert(fb != NULL);
  string source_file_name = fb->source_file_name();
  zsuif_definition_block *zdb = trans(fb->get_definition_block());

  zsuif_file_block *zfb = 
    new zsuif_file_block(source_file_name,zdb);

  /* cons on a new file block */
    file_blocks = new zsuif_file_block_list(zfb,file_blocks);
  
}
/*****************************************/
zsuif_definition_block* trans_suif::trans(definition_block* db) {
  assert(db != NULL);
  zsuif_variable_symbol_list *defined_variables   = NULL;
  zsuif_procedure_symbol_list *defined_procedures = NULL;

  s_count_t n,i;
  
  zref<variable_definition> v;
  tos_ref<zref<variable_definition> > vl = db->variable_definitions();
  n = vl.count();
  info(i_integer(1),"Var Count is %d\n",n);
  for(i = 0; i < n; i++) {
    v = vl.elem_by_num(i);
    variable_symbol *vs = (v.get())->get_variable_symbol();
    defined_variables = 
      new zsuif_variable_symbol_list(trans(vs),defined_variables);
  }

  zref<procedure_definition> p;
  tos_ref<zref<procedure_definition> > pl = db->procedure_definitions();
  n = pl.count();
  info(i_integer(1),"Proc Count is %d\n",n);
  for(i = 0; i < n; i++) {
    p = pl.elem_by_num(i);
    procedure_symbol *ps = (p.get())->get_procedure_symbol();
    defined_procedures = 
      new zsuif_procedure_symbol_list(trans(ps),defined_procedures);
  }
  
  return new zsuif_definition_block(defined_variables,defined_procedures);
}

/*****************************************/
zsuif_file_set_block* trans_suif::trans(file_set_block* block) {
  assert(block != NULL);
   handle_file_set_block(block);
   return new zsuif_file_set_block
     (file_blocks,
      new zsuif_type_table(type_table_entries),
      new zsuif_symbol_table(symbol_table_entries),
      new zsuif_symbol_table(extern_symbol_table_entries),
      information_block);
  
}

zsuif_type_id* trans_suif::trans(type* t){ 
  trans_type trans(this,t);
  return trans.get_type_id();
}

 
/*****************************************/
zsuif_int_or_source_op* trans_suif::trans(int_or_source_op* op){ 
  assert(op != NULL);
  if(op->is_integer()) {
    i_integer i = op->get_integer();
    return new zsuif_Int(i.c_int());
  } else {
    zsuif_source_op *s = trans(&(op->get_source_op()));
    return new zsuif_SrcOp(s);
  }
}

/*****************************************/
zsuif_int_or_source_op* trans_suif::trans_opt(int_or_source_op* op){ 
  assert(op != NULL);
  if(op->is_integer()) {
    i_integer i = op->get_integer();
    if(i.is_finite()) {
      return new zsuif_Int(i.c_int());
    } else {
      return NULL;
    }
  } else {
    zsuif_source_op *s = trans(&(op->get_source_op()));
    return new zsuif_SrcOp(s);
  }
}

/*****************************************/
zsuif_source_op* trans_suif::trans(source_op* src){ 
  assert(src != NULL);
  if(src->is_variable()) {
    zsuif_variable_symbol *var = trans(src->get_variable());
    return new zsuif_SrcVar(var);
  }
  /*  gone in suif 2.0
  if(src->is_register()) {
    zsuif_register_symbol *reg = trans(src->get_register());
    zsuif_type_id *type = trans(src->type());
    return new zsuif_SrcReg(reg,type);
  } */
  if(src->is_instr_source()) {
    zsuif_instruction *instr = trans(src->get_instruction());
    s_count_t op_num =  (src->get_instr_operand_num());
    return new zsuif_SrcDst(instr,op_num);
  }
  if(src->is_null()) {
    return new zsuif_SrcZero();
  }
  error(-1,"trans_suif bad source_op");
  return NULL; /* not reached */
}

/*****************************************/
zsuif_destination_op* trans_suif::trans(destination_op* dst){ 
  assert(dst != NULL);
  if(dst->is_null()) {
    return new zsuif_DstTmp();
  }
  if(dst->is_variable()) {
    zsuif_variable_symbol *var = trans(dst->get_variable());
    return new zsuif_DstVar(var);
  }
  /* gone in new version of suif
  if(dst->is_register()) {
    zsuif_register_symbol *reg = trans(dst->get_register());
    zsuif_type_id *type = trans(dst->type());
    return new zsuif_DstReg(reg,type);
  } */
  if(dst->is_instr_destination()) {
    return new zsuif_DstTmp();
    /*    zsuif_instruction *instr = 
    s_count_t op_num =  (dst->get_instr_operand_num());
    return new zsuif_DstSrc(instr,op_num);*/
  }
  error(-1,"trans_suif bad destintion_op");
  return NULL; /* not reached */
}

/*****************************************/
zsuif_statement* trans_suif::trans(cfo* c){ 
  trans_statement stmt(this,c);
  return  stmt.answer();
}
/*****************************************/
zsuif_statement* trans_suif::trans(statement* s){ 
  trans_statement stmt(this,s);
  return  stmt.answer();
}
/*****************************************/
zsuif_statement_list* trans_suif::trans(statement_list* sl){ 
  tos_ref<statement *> stmts = sl->data();
  s_count_t num_stmts = stmts.count();
  zsuif_statement_list* zstmts = NULL;
  /* cons thing on in reverse so the idx 0 is the first in the list */
  while(num_stmts--) {
    zstmts = 
      new zsuif_statement_list(trans(stmts.elem_by_num(num_stmts)),zstmts);
  }
  return  zstmts;
}

/*****************************************/
zsuif_instruction* trans_suif::trans(instruction* i){ 
  assert(i != NULL);
  trans_instruction instr(this,i);
  return instr.answer();
}

/*****************************************/
zsuif_constant* trans_suif::trans(constant* c){
  assert(c != NULL);
  if(c->is_integer()) {
    return new zsuif_ConstInt(trans(c->get_integer()));
  }
  if(c->is_string()) {
    return new zsuif_ConstString(c->get_string());
  }
  if(c->is_bit_block()) {
    s_count_t num_bytes = c->get_bit_size();
    char *str = new char[(2*num_bytes)+1];
    unsigned char *bits =  c->get_bits();
    int i = 0;
    static const char hexdigits[] = "0123456789abcdef";
    
    while(i < num_bytes) {
      int j = 2*i;
      str[j]   = hexdigits[(bits[i]>>4)%16];
      str[j+1] = hexdigits[bits[i]%16];
      i++;
    }
    str[(2*i)+1] = '\0';
    string res(str);
    delete str;
    return new zsuif_ConstBits(res);
  }
  error(-1,"Null Constant");
  return NULL; /* not reached */
}

/*****************************************/
class trans_value_block : public suif_visitor {
private:
  zsuif_value_block *zvb;
  trans_suif* t;
public:
  trans_value_block(trans_suif *trans, value_block *vb) {
    assert(trans != NULL);
    assert(vb != NULL);
    this->t = trans; 
    vb->apply_pyg_visitor(this);
    zvb->data_type = t->trans(vb->get_type());
  }

  zsuif_value_block * answer() { return zvb; }

  void handle_constant_value_block(constant_value_block *vb) {
    zvb = new zsuif_Constant_value_block(t->trans(&(vb->get_constant())));
  }
  
  void handle_undefined_value_block(undefined_value_block *vb) {
    zvb = new zsuif_Undefined_value_block();
  }

  void handle_multi_value_block(multi_value_block *vb) {
    zsuif_multi_value_block_init_list* inits = NULL;
    s_count_t num_sub_blocks = (vb->num_sub_blocks());

    /* cons thing on in reverse so the idx 0 is the first in the list */
    while(num_sub_blocks--) {
      int bit_offset = (vb->bit_offset(num_sub_blocks)).c_int();
      trans_value_block sub_block(t,vb->sub_block(num_sub_blocks));
      
      zsuif_multi_value_block_init *init =
	new zsuif_multi_value_block_init(bit_offset,
					 sub_block.answer());
      inits =
	new zsuif_multi_value_block_init_list(init,inits);
    }
    zvb = new zsuif_Multi_value_block(inits);
  }

  void handle_expression_value_block(expression_value_block *vb) {
    zsuif_source_op* expression = t->trans(&(vb->get_expression()));
    zvb = new zsuif_Expression_value_block(expression);
  }

  void handle_repeat_value_block(repeat_value_block *vb) {
    zsuif_suif_int* count = t->trans(vb->num_repetitions());
    trans_value_block sub_block(t,vb->sub_block());
    zsuif_value_block* block = sub_block.answer();
    zvb = new zsuif_Repeat_value_block(count,block);
  }

};

/*****************************************/
zsuif_value_block* trans_suif::trans(value_block* vb) {
  assert(vb != NULL);
  trans_value_block block(this,vb);
  return block.answer();
}

/*****************************************/
zsuif_procedure_definition* trans_suif::trans(procedure_definition* def){ 
  assert(def != NULL);
  procedure_symbol* ps = def->get_procedure_symbol();
  zsuif_procedure_symbol* name =  new zsuif_procedure_symbol(make_symb(ps));

  trans_type typ(this,ps->get_type());
  zsuif_procedure_type* procedure_type = typ.get_procedure_type();
  zsuif_qualification_list* qualifications = typ.get_qualifications();

  if(def->body()) {
    zsuif_statement* body = trans(def->body());
    zsuif_definition_block* definition_block = 
      trans(def->get_definition_block());
    do_table(def->get_symbol_table());
    zsuif_statement* sbody =
      new zsuif_Scope_statement(body, definition_block);

    zsuif_parameter_symbol_list* params = NULL;
    s_count_t num_params = (def->num_formal_parameters());

    /* cons thing on in reverse so the idx 0 is the first in the list */
    while(num_params--) {
      params =
	new zsuif_parameter_symbol_list
	(trans(def->formal_parameter(num_params)),params);
    }

    return new zsuif_procedure_definition
      (name, qualifications, procedure_type,       
       new zsuif_procedure_body(params,sbody));

  } else {
    return new zsuif_procedure_definition
      (name, qualifications, procedure_type, NULL); 
  }
}


/*****************************************/
zsuif_variable_definition*  trans_suif::trans(variable_definition* def){ 
  assert(def != NULL);
  variable_symbol* vs = def->get_variable_symbol();
  zsuif_variable_symbol* name =  new zsuif_variable_symbol(make_symb(vs));

  zsuif_value_block* vb = trans(def->initialization());
  trans_type vtype(this,vs->get_type());
  return new zsuif_variable_definition(name,vtype.get_type(),vb); 
}


/*****************************************/
zsuif_binop* trans_suif::get_binop(lstring x) {
  switch(QUICK_OPCODE_FROM_STRING(x)) {

  case OPCODE_add: return zsuif_Add;
  case OPCODE_subtract: return zsuif_Subtract;
  case OPCODE_multiply: return zsuif_Multiply;
  case OPCODE_divide: return zsuif_Divide;
    
  case OPCODE_remainder: return zsuif_Remainder;
  case OPCODE_bitwise_and: return zsuif_Bitwise_and;
  case OPCODE_bitwise_or: return zsuif_Bitwise_or;
    
  case OPCODE_bitwise_nand: return zsuif_Bitwise_nand;
  case OPCODE_bitwise_nor: return zsuif_Bitwise_nor;
  case OPCODE_bitwise_xor: return zsuif_Bitwise_xor;
    
  case OPCODE_left_shift: return zsuif_Left_shift;
  case OPCODE_right_shift: return zsuif_Right_shift;
  case OPCODE_rotate: return zsuif_Rotate;
  case OPCODE_is_equal_to: return zsuif_Is_equal_to;
    
  case OPCODE_is_not_equal_to: return zsuif_Is_not_equal_to;
  case OPCODE_is_less_than: return zsuif_Is_less_than;
    
  case OPCODE_is_less_than_or_equal_to: return zsuif_Is_less_than_or_equal_to;

  case OPCODE_is_greater_than: return zsuif_Is_greater_than;
    
  case OPCODE_is_greater_than_or_equal_to:
    return zsuif_Is_greater_than_or_equal_to;
  case OPCODE_logical_and: return zsuif_Logical_and;
  case OPCODE_logical_or: return zsuif_Logical_or;
    
  case OPCODE_maximum: return zsuif_Maximum;
  case OPCODE_minimum: return zsuif_Minimum;

  default:
      error(-1,"Bad binary op\n");
      return NULL;
  }
}
/*****************************************/
zsuif_binop* trans_suif::get_cmpop(lstring x) {
  switch(QUICK_OPCODE_FROM_STRING(x)) {
  case OPCODE_is_equal_to: return zsuif_Is_equal_to;
  case OPCODE_is_not_equal_to: return zsuif_Is_not_equal_to;
  case OPCODE_is_less_than: return zsuif_Is_less_than;
  case OPCODE_is_less_than_or_equal_to: return zsuif_Is_less_than_or_equal_to;
  case OPCODE_is_greater_than: return zsuif_Is_greater_than;
  case OPCODE_is_greater_than_or_equal_to:
    return zsuif_Is_greater_than_or_equal_to;
  default:
    error(-1,"Bad binary op\n");
    return NULL;
  }
}
/*****************************************/
zsuif_unop*  trans_suif::get_unop(lstring x) {
  switch(QUICK_OPCODE_FROM_STRING(x)) {
  case OPCODE_negate: return zsuif_Negate;
  case OPCODE_invert: return zsuif_Invert;
  case OPCODE_absolute_value: return zsuif_Absolute_value;
  case OPCODE_bitwise_not: return zsuif_Bitwise_not;
  case OPCODE_logical_not: return zsuif_Logical_not;
  case OPCODE_convert: return zsuif_Convert;
  case OPCODE_treat_as: return zsuif_Treat_as;
  default:
    error(-1,"Bad unary op\n");
    return NULL;
  }
}
