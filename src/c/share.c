#include <stdlib.h>
#include <stdio.h>
#include "share.h"
#include <cii/table.h>
#include <cii/atom.h>
#define die() (fprintf(stderr,"%s:%d: Fatal Error\n",__FILE__,__LINE__), \
		exit(-1))
static Table_T ptr2key_tbl = NULL;
static Table_T key2ptr_tbl = NULL;
static int cmp_ptr(const void *x, const void *y) {
     return  x != y;
}
static unsigned hash_ptr(const void *key) {
     return ((unsigned)key);
}
static void ensure_tables (void) {
     if(key2ptr_tbl == NULL) 
	  key2ptr_tbl = Table_new(1024,NULL,NULL);
     if(ptr2key_tbl == NULL)
	  ptr2key_tbl = Table_new(1024,cmp_ptr,hash_ptr);
}
static const char* new_key(void) {
     static int k = 0;
     k++;
     return Atom_int(k);
}
void share_clear_table(void) {
     if(key2ptr_tbl) {
	  Table_free(&key2ptr_tbl);
	  key2ptr_tbl = NULL;
     }
     if(ptr2key_tbl) {
	  Table_free(&ptr2key_tbl);
	  ptr2key_tbl = NULL;
     }
}
share_ty ptr2share(void *ptr) {
     share_ty ret;
     const char* key;

     ensure_tables();
     key = Table_get(ptr2key_tbl,ptr);
     if (key == NULL) { /* this is a def */
	  ret.key = new_key();
	  ret.value = ptr;
	  /* register the pointer */
	  Table_put(ptr2key_tbl,ptr,ret.key);
	  Table_put(key2ptr_tbl,ret.key,ptr);
     } else {
	  ret.key = key; /* this is a use */
	  ret.value = NULL;
     }
     return ret;
}
void * share2ptr(share_ty s) {
     void* ret;

     ensure_tables();
     if (s.value == NULL) { /* this is a use */
	  ret = Table_get(key2ptr_tbl,s.key);
     } else {  /* this is a def */
	  /* register the pointer */
	  Table_put(ptr2key_tbl,s.value,s.key);
	  Table_put(key2ptr_tbl,s.key,s.value);
	  ret = s.value;
     }
     if (ret == NULL)  /* use before def */  die();
     return ret;
}

