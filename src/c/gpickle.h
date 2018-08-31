#ifndef _GPickle_
#define _GPickle_
#include "TypePickle.h"
#include "AsdlValue.h"

#define T(x) TypePickle_##x
#define V(x) AsdlValue_##x
#define P(x) GPickle_##x

typedef struct P(maps_s) *P(maps_ty);

P(maps_ty)             P(make_maps)(T(type_env_ty));

int                    P(lookup_type_idx_by_name)(P(maps_ty) m,T(qid_ty) n);
int                    P(lookup_cnstr_idx_by_name)(P(maps_ty) m,T(qid_ty) n);

T(cnstr_map_value_ty)  P(lookup_cnstr_by_tag)(P(maps_ty) m,
					      T(type_map_value_ty) ty,
					      int tag);

T(type_map_value_ty)   P(lookup_type)(P(maps_ty) m,int i);
T(cnstr_map_value_ty)  P(lookup_cnstr)(P(maps_ty) m,int i);
T(module_map_value_ty) P(lookup_module)(P(maps_ty) m,int i);

V(asdl_value_ty)       P(read_asdl_value)(P(maps_ty) maps,
					  T(qid_ty) qid,instream_ty s);

void                   P(write_asdl_value)(P(maps_ty) maps,
					   V(asdl_value_ty) v,outstream_ty s);


#undef T
#undef V
#undef P
#endif
