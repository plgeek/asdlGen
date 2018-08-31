#include "gpickle.h"
#include "assoc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#define T(x) TypePickle_##x
#define V(x) AsdlValue_##x
#define P(x) GPickle_##x

struct P(maps_s) {
  int max_tkey;
  T(type_map_value_ty)*  tmap;

  int max_ckey;
  T(cnstr_map_value_ty)* cmap;
  
  int max_mkey;
  T(module_map_value_ty)* mmap;

  Assoc_ty tassoc;
  Assoc_ty cassoc;
};

static T(qid_ty) get_name (T(type_map_value_ty) ty);
static V(asdl_value_ty) read_type(P(maps_ty) maps, int tid,instream_ty s);
static void write_value(P(maps_ty) maps, V(asdl_value_ty),outstream_ty s);

P(maps_ty) P(make_maps)(T(type_env_ty) tenv) {
     P(maps_ty) new_map;

     T(type_map_value_ty)   *tmap;
     T(cnstr_map_value_ty)  *cmap;
     T(module_map_value_ty) *mmap;

     Assoc_ty tassoc;
     Assoc_ty cassoc;

     list_ty tentries;
     list_ty centries;
     list_ty mentries;
     int num_entries;
     int entry_num;

     new_map = malloc(sizeof(*new_map));
     assert(new_map != NULL);

     new_map->max_tkey = tenv->tmap->max_key;
     new_map->max_ckey = tenv->cmap->max_key;
     new_map->max_mkey = tenv->mmap->max_key;

     tmap = calloc(sizeof(T(type_map_value_ty)),(new_map->max_tkey+1));
     cmap = calloc(sizeof(T(cnstr_map_value_ty)),(new_map->max_ckey+1));
     mmap = calloc(sizeof(T(module_map_value_ty)),(new_map->max_mkey+1));

     tassoc = Assoc_MakeData();
     cassoc = Assoc_MakeData();

     assert(tmap != NULL);
     assert(cmap != NULL);
     assert(mmap != NULL);
     assert(tassoc != NULL);
     assert(cassoc != NULL);


     new_map->tmap = tmap;
     new_map->cmap = cmap;
     new_map->mmap = mmap;

     new_map->tassoc = tassoc;
     new_map->cassoc = cassoc;

     tentries = tenv->tmap->entries;
     num_entries = Seq_length(tentries);
     entry_num = 0;
     while(entry_num < num_entries) {
          T(type_map_entry_ty) entry = Seq_get(tentries,entry_num);
	  tmap[entry->key] = entry->v;
	  Assoc_SetData(tassoc,get_name(entry->v),entry);
	  entry_num++;
     }

     centries = tenv->cmap->entries;
     num_entries = Seq_length(centries);
     entry_num = 0;
     while(entry_num < num_entries) {
          T(cnstr_map_entry_ty) entry = Seq_get(centries,entry_num);
	  cmap[entry->key] = entry->v;
	  Assoc_SetData(cassoc,entry->v->name,entry);
	  entry_num++;
     }

     mentries = tenv->mmap->entries;
     num_entries = Seq_length(mentries);
     entry_num = 0;
     while(entry_num < num_entries) {
          T(module_map_entry_ty) entry = Seq_get(mentries,entry_num);
	  mmap[entry->key] = entry->v;
	  entry_num++;
     }
     return new_map;
}

T(type_map_value_ty) P(lookup_type)(P(maps_ty) m,int i) {
     assert(i <= m->max_tkey);
     return m->tmap[i];
}

T(cnstr_map_value_ty) P(lookup_cnstr)(P(maps_ty) m,int i) {
     assert(i <= m->max_ckey);
     return m->cmap[i];
}

T(module_map_value_ty) P(lookup_module)(P(maps_ty) m,int i) {
     assert(i <= m->max_mkey);
     return m->mmap[i];
}

int P(lookup_type_idx_by_name)(P(maps_ty) m,T(qid_ty) n) {
     T(type_map_entry_ty) tentry;

     tentry = Assoc_GetData(m->tassoc,n);
     if(tentry == NULL) {
	  return -1;
     } else {
	  return tentry->key;
     }
}

int P(lookup_cnstr_idx_by_name)(P(maps_ty) m,T(qid_ty) n) {
     T(cnstr_map_entry_ty) centry;

     centry = Assoc_GetData(m->cassoc,n);
     if(centry == NULL) {
	  return -1;
     } else {
	  return centry->key;
     }
}

T(cnstr_map_value_ty)  P(lookup_cnstr_by_tag)(P(maps_ty) m,
					      T(type_map_value_ty) ty,
					      int tag) {
     /* should  precompute this rather than using this naive search */
     list_ty cnstrs;
     int entry_num,num_entries;

     switch(ty->kind) {
     case T(Defined_enum): 
	  cnstrs = ty->v.T(Defined).cnstr_map_keys;
	  break;
     default: assert(0); return NULL;
     }
     num_entries = Seq_length(cnstrs);
     entry_num = 0;
     while(entry_num < num_entries) {
       StdTypes_nat_ty *entry = Seq_get(cnstrs,entry_num++);
       T(cnstr_map_value_ty) cnstr = P(lookup_cnstr)(m,*entry);
       if(cnstr->pkl_tag == tag) {
	 return cnstr;
       }
     }
     assert(0); return NULL;
}

static V(prim_value_ty) read_prim(T(prim_ty) p,instream_ty s) {
     switch(*p) {
     case T(Int_enum)       :  return V(IntValue)(read_int(s));       
     case T(String_enum)    :  return V(StringValue)(read_string(s));
     case T(Identifier_enum):  return V(IdentifierValue)(read_identifier(s)); 
     }
     assert(0);  return NULL; 
}

static T(qid_ty) int_qid;
static T(qid_ty) str_qid;
static T(qid_ty) id_qid;

static T(qid_ty) get_name (T(type_map_value_ty) ty) {

     if(!int_qid) {
	   int_qid = T(qid)(Seq_new(0),Atom_string("int"));
	   str_qid = T(qid)(Seq_new(0),Atom_string("string"));
	   id_qid = T(qid)(Seq_new(0),Atom_string("identifier"));
     }

     switch(ty->kind) {
     case T(Defined_enum): return ty->v.T(Defined).name;
     case T(Prim_enum)   : 
       switch(*(ty->v.T(Prim).p)) {
       case T(Int_enum)       : return int_qid;
       case T(String_enum)    : return str_qid;
       case T(Identifier_enum): return id_qid;
       default: assert(0); return NULL;
       }
     default: assert(0); return NULL;
     }

}
static V(asdl_value_ty) read_field(P(maps_ty) maps,T(field_ty) fd, 
                                  instream_ty s) {
     
     int                 tid = fd->type_map_key;
     T(type_map_value_ty) ty = P(lookup_type)(maps,tid);
     T(qid_ty) name          = get_name(ty);

     switch(fd->kind) {
     case T(Id_enum)     : return read_type(maps,tid,s);
     case T(Option_enum) : {
	  int t = read_tag(s);
	  if (t == 0) {
	       return V(NoneValue(name));
	  } else {
	       V(asdl_value_ty) v = read_type(maps,tid,s);
	       return V(SomeValue)(name,v);
	  }
     }
     case T(Sequence_enum) : {
	  int len = read_tag(s);
	  list_ty vs = Seq_new(len);
	  while(len > 0) {
	    Seq_addhi(vs,read_type(maps,tid,s));
	    len--;
	  }
	  return V(SequenceValue)(name,vs);
     }
     default : assert(0); return NULL;
     }
     
}

static list_ty read_fields(P(maps_ty) maps, list_ty fds, instream_ty s) {
	  int num_fields = Seq_length(fds);
	  int field_num = 0;
	  list_ty vs = Seq_new(num_fields);

	  while(field_num < num_fields) {
	    Seq_addhi(vs,read_field(maps,Seq_get(fds,field_num),s));
	    field_num++;
	  }
	  return vs;
}

static list_ty read_fields_off(P(maps_ty) maps, int off,
			       list_ty fds, instream_ty s) {
	  int num_fields = Seq_length(fds)-off;
	  int field_num = 0;
	  list_ty vs = Seq_new(num_fields);

	  while(field_num < num_fields) {
	    Seq_addhi(vs,read_field(maps,Seq_get(fds,off+field_num),s));
	    field_num++;
	  }
	  return vs;
}

static V(asdl_value_ty) read_cnstr(P(maps_ty) maps,
				   T(type_map_value_ty) ty,
				   list_ty fds,
				   T(qid_ty) name,
				   instream_ty  s) {

     int tag = read_tag(s);
     T(cnstr_map_value_ty) cnstr =  P(lookup_cnstr_by_tag)(maps,ty,tag);
     
     list_ty attrbs = read_fields(maps,fds,s);
     list_ty vs     = read_fields(maps,cnstr->fields,s);
     
     return V(SumValue)(name,cnstr->name,attrbs,vs);
}
				   


static V(asdl_value_ty) read_type(P(maps_ty) maps, int tid,instream_ty  s) {

     T(type_map_value_ty) ty = P(lookup_type)(maps,tid);

     T(qid_ty) name = get_name(ty);
     switch(ty->kind) {
     case T(Defined_enum): {
	  list_ty cnstrs;
	  cnstrs = ty->v.T(Defined).cnstr_map_keys;
	  if(Seq_length(cnstrs) == 0) { /* product type */
	    V(asdl_value_ty) v = 
	      read_field(maps,
			 Seq_get(ty->v.T(Defined).fields,0),s);
	    list_ty vs =  read_fields_off(maps,1,ty->v.T(Defined).fields,s);
	       return V(ProductValue)(name,v,vs);
	  } else { /* sum type */
	       return read_cnstr(maps,ty,ty->v.T(Defined).fields,name,s);
	  }
     }
     case T(Prim_enum): {
	  V(prim_value_ty) v = read_prim(ty->v.T(Prim).p,s);

	  return V(PrimValue)(name,v);
     }
	  
	
     default: assert(0); return NULL;
     }
}
V(asdl_value_ty) P(read_asdl_value)(P(maps_ty) maps,T(qid_ty) qid,
				    instream_ty s) {
     int idx = P(lookup_type_idx_by_name)(maps,qid);
     if(idx != -1) {
	  return read_type(maps,idx,s);
     } else {
	  return NULL;
     }
}


static void write_prim(V(prim_value_ty) val, outstream_ty output)
{
        switch(val->kind) {
	case V(IntValue_enum):
	     write_int(val->v.V(IntValue).int1,output);
	     break;
	case V(StringValue_enum):
	     write_string(val->v.V(StringValue).string1, output);
	     break;
	case V(IdentifierValue_enum):
	     write_identifier(val->v.V(IdentifierValue).identifier1,output);
	     break;
	default: assert(0); 
        }
}
 
static void write_value_list(P(maps_ty) m, list_ty l, outstream_ty output) {
  int num_entries = Seq_length(l);
  int entry_num = 0;
     while (entry_num < num_entries) {
	  write_value(m,Seq_get(l,entry_num), output);
	  entry_num++;
     }
}
 

static void write_value(P(maps_ty) m,
			V(asdl_value_ty) val, outstream_ty output) {
        int num;
	int tidx;
        T(cnstr_map_value_ty) cnstr;
	
 
        switch(val->kind) {
	case V(SumValue_enum):
	     tidx = P(lookup_cnstr_idx_by_name)(m,val->v.V(SumValue).con);
	     assert(tidx != -1);
	     cnstr = P(lookup_cnstr)(m,tidx);
	     num = cnstr->pkl_tag;
	     write_tag(num, output);
	     write_value_list(m,val->v.V(SumValue).attrbs, output);
	     write_value_list(m,val->v.V(SumValue).vs, output);
	     break;
	case V(ProductValue_enum):
	     write_value(m,val->v.V(ProductValue).v, output);
	     write_value_list(m,val->v.V(ProductValue).vs, output);
	     break;
	case V(SequenceValue_enum):
	     num = Seq_length(val->v.V(SequenceValue).vs);
	     write_tag(num, output);
	     write_value_list(m,val->v.V(SequenceValue).vs, output);
                break;
	case V(SomeValue_enum):
	     write_tag(1, output);
	     write_value(m,val->v.V(SomeValue).v, output);
	     break;
	case V(NoneValue_enum):
	     write_tag(0, output);
	     break;
	case V(PrimValue_enum):
	     write_prim(val->v.V(PrimValue).v, output);
	     break;
	default:
	     assert(0);
        }
}
void P(write_asdl_value)(P(maps_ty) maps,V(asdl_value_ty) v,outstream_ty  s) {
     write_value(maps,v,s);
}
