#include "assoc.h"
#include <malloc.h>
#include <memory.h>
#include <stdlib.h>
#include <assert.h>
/* todo use cii Tables */
#define TBL_SZ 1031

typedef struct bucket_s { /* private type */
     unsigned int hash; 
     Assoc_key_ty id; 
     Assoc_value_ty data;
     struct bucket_s *next;
} *bucket_ty;

struct Assoc_ctx_s {
     bucket_ty *bucketlist;
     bucket_ty bucket;
     int index;
};

struct Assoc_s {
     int size;
     bucket_ty *buckets;
};


/* static bucket_ty buckets[TBL_SZ]; */

Assoc_ty Assoc_MakeData(void){
        Assoc_ty a = malloc(sizeof(struct Assoc_s));
	bucket_ty *buckets = calloc(sizeof(bucket_ty),TBL_SZ);
	assert(a != NULL);
	assert(buckets != NULL);

	a->buckets = buckets;
	a->size = TBL_SZ;
	return a;
}


/* A function to hash a character.  The computation is:
 *
 *   h = 33 * h + 720 + c
 *
 */
static unsigned int hashString(char *x) {

     unsigned int acc;
     for(acc=0;*x;x++) {
	  acc = (acc<<5) + acc + 720 + (*x);
     }
     return acc;
}

static int eqKey(TypePickle_qid_ty x, TypePickle_qid_ty y) {
     list_ty xq;
     list_ty yq;
     int i;
     int j;

     if(x == y) { return 1; }         /* hackish optimiziation */
     
     if((x->base != y->base)) { return 0; }
     xq = x->qualifier;
     yq = y->qualifier;

     if (xq == yq) { return 1;} /* hackish optimiziation */

     i = Seq_length(xq);
     j = Seq_length(yq);

     if (i != j ) { return 0; }
     
     j = 0;
     while(j < i) {
       if(Seq_get(xq,j) != Seq_get(yq,j)) { return 0; } 
       j++; 
     }
     return 1;
}

static unsigned int hashKey(TypePickle_qid_ty qid) {
     unsigned int x;
     int i = Seq_length(qid->qualifier);
     /* just hash the base and first qualifier */
     x = hashString((char*)qid->base);

     if(i > 0) { 
	  x += hashString((char*)Seq_get(qid->qualifier,0));
     }
     return x;
}


Assoc_value_ty Assoc_SetData(Assoc_ty a, 
			     Assoc_key_ty x,  Assoc_value_ty data) {
     unsigned int hc = hashKey(x);
     int idx = hc % TBL_SZ;
     
     bucket_ty p = a->buckets[idx];

     while(p) {
	  if(p->hash != hc) {
	       p=p->next;
	       continue;
	  }
	  if(eqKey(x,p->id)) {
	       p->data = data;
	       return data;
	  } 
	  p=p->next;
     }

     /* new entry */
     p = malloc(sizeof(struct bucket_s));
     p->id=x;
     p->hash=hc;
     p->data=data;
     p->next=a->buckets[idx];
     a->buckets[idx]=p;
     return data;
}

Assoc_value_ty Assoc_GetData(Assoc_ty a, Assoc_key_ty key) {
     unsigned int hc = hashKey(key);
     int idx = hc % TBL_SZ;
     bucket_ty p = a->buckets[idx];

     while(p) {
	  if(p->hash != hc) {
	       p=p->next;
	       continue;
	  }
	  if(eqKey(key,p->id)) {
	       return p->data;
	  } 
	  p=p->next;
     }
     return NULL;
}


Assoc_key_ty Assoc_NextEntry(Assoc_ctx_ty search) {
	int i = search->index;
	if (search->bucket)
	{
		search->bucket = search->bucket->next;
		if (search->bucket)
			return search->bucket->id;
		i++;
	}
	while ((!search->bucketlist[i]) && (i < (TBL_SZ-1)))
		i++;
	search->bucket = search->bucketlist[i];
	search->index = i;
	return search->bucket ? search->bucket->id : NULL;
}

Assoc_ctx_ty Assoc_BeginSearch(Assoc_ty a) {

	Assoc_ctx_ty search = malloc(sizeof(struct Assoc_ctx_s));
	search->bucketlist = a->buckets;
	search->bucket = NULL;
	search->index = 0;
	return search;
}

void Assoc_EndSearch(Assoc_ctx_ty search) {
	free(search);
}
