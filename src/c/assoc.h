#ifndef _Assoc_
#define _Assoc_
#include "TypePickle.h"
typedef struct Assoc_s  * Assoc_ty;
typedef struct Assoc_ctx_s  * Assoc_ctx_ty;
typedef TypePickle_qid_ty Assoc_key_ty;
typedef void*             Assoc_value_ty;

extern Assoc_ty Assoc_MakeData();

extern Assoc_value_ty Assoc_SetData(Assoc_ty a, 
				    Assoc_key_ty k,  Assoc_value_ty v);
extern void   Assoc_DeleteData(Assoc_ty, Assoc_key_ty k);
extern Assoc_value_ty Assoc_GetData(Assoc_ty a,Assoc_key_ty k);

extern Assoc_ctx_ty Assoc_BeginSearch(Assoc_ty a);
extern Assoc_key_ty Assoc_NextEntry(Assoc_ctx_ty a);
extern void         Assoc_EndSearch(Assoc_ctx_ty c);


#endif
