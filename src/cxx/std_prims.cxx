#include "StdPrims.hxx"
#include <stdlib.h>
#include <string.h>
#define WRITE_BYTES(x,sz,s) (fwrite(x,sizeof(char),sz,s))
#define READ_BYTES(x,sz,s) (fread(x,sizeof(char),sz,s))

void StdPkl_write_tag(int x,outstream s) { write_uint32(x,s); }
int StdPkl_read_tag(instream s) { return read_uint32(s); }

void StdPrims_write_int(int x,outstream s) { write_uint32(x,s); }
int StdPrims_read_int(instream s) { return read_uint32(s); }

void StdPrims_write_big_int(big_int x,outstream s) { write_big_int(x,s); }
big_int StdPrims_read_big_int(instream s) { return read_big_int(s); }

void StdPrims_write_string(StdPrims_string x,outstream s) {
  int sz = strlen(x);
  StdPkl_write_tag(sz,s); 
  WRITE_BYTES(x,sz,s);
}
StdPrims_string StdPrims_read_string(instream s) {
  int sz = StdPkl_read_tag(s);
  char *ret = new char[sz+1];
  
  READ_BYTES(ret,sz,s);
  ret[sz]='\0';
  return ret;
}

static char* uniquify(char *x);
void StdPrims_write_identifier(StdPrims_identifier x,outstream s) {
  StdPrims_write_string(x,s);
}
StdPrims_identifier StdPrims_read_identifier(instream s) {
  char *x = StdPrims_read_string(s);
  char *y = uniquify(x);
  
  if(x != y) {  delete [] x;
  }
  return y;
}

#define TBL_SZ 1031
typedef struct _bucket {
     unsigned int hash; 
     char *id; 
     struct _bucket *next;} *bucket_ty;
static bucket_ty buckets[TBL_SZ];


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

static char *uniquify(char *x) {
     unsigned int hc = hashString(x);
     int idx = hc % TBL_SZ;
     bucket_ty p = buckets[idx];

     while(p) {
	  if(p->hash != hc) {
	       p=p->next;
	       continue;
	  }
	  if(strcmp(x,p->id)==0) {
	       return p->id;
	  } 
	  p=p->next;
     }
     /* new entry */
     p = new _bucket;
     p->id=x;
     p->hash=hc;
     p->next=buckets[idx];
     buckets[idx]=p;
     return x;
}

