#include "StdPkl.h"
#include "StdPrims.h"
#define WRITE_BYTES(x,sz,s) (fwrite(x,sizeof(char),sz,s))
#define READ_BYTES(x,sz,s) (fread(x,sizeof(char),sz,s))
#define WRITE_TAG(x,s) (StdPkl_write_tag(x,s))
#define READ_TAG(s) (StdPkl_read_tag(s))
int StdPkl_read_tag(instream_ty s) {
  return read_int32(s);
}

void StdPkl_write_tag(int x, outstream_ty s) {
  write_int32(x,s);
}

list_ty StdPkl_read_list(generic_reader_ty rd,instream_ty s) {
  int len = READ_TAG(s);
  Seq_T ret = Seq_new(len);
  
  while(len) {
    Seq_addhi(ret,(*rd)(s));
    len--;
  }
  return ret;
}

opt_ty StdPkl_read_option(generic_reader_ty rd,instream_ty s) {
  if(READ_TAG(s) == 0) {
    return NULL;
  }
  return (*rd)(s);
}

share_ty StdPkl_read_share(generic_reader_ty rd,instream_ty s) {
  int len = read_int32(s);
  share_ty ret;
  char* str;
  if (len == 0) die();
  if (len < 0) { /* definition */
    len = -len;
    str = malloc(len);
    if(str == NULL) die();
    READ_BYTES(str,len,s);
    ret.key = Atom_new(str,len);
    ret.value = (*rd)(s);
  } else {
    str = malloc(len);
    if(str == NULL) die();
    READ_BYTES(str,len,s);
    ret.key = Atom_new(str,len);
    ret.value = NULL;
  }
  return ret;
}

void StdPkl_write_list(generic_writer_ty wr,list_ty v, outstream_ty s) {
  int len = Seq_length(v);
  int i;
  
  WRITE_TAG(len,s);
  for(i=0;i<len;i++) {
    (*wr)(Seq_get(v,i),s);
  }
}

void StdPkl_write_option(generic_writer_ty wr,opt_ty v, outstream_ty s) {
  if (v == NULL) {
    WRITE_TAG(0,s);
  } else {
    WRITE_TAG(1,s);
    (*wr)(v,s);
  }
}

void StdPkl_write_share(generic_writer_ty wr,share_ty v,outstream_ty s) {
  if(v.value == NULL) {
    int len = Atom_length(v.key);
    write_int32(len,s);
    WRITE_BYTES(v.key,len,s);
  } else {
    int len = Atom_length(v.key);
    write_int32(-len,s);
    WRITE_BYTES(v.key,len,s);
    (*wr)(v.value,s);
  }
}

StdPrims_int_ty StdPrims_read_int(instream_ty s) {
     return read_int32(s);
}

StdPrims_big_int_ty StdPrims_read_big_int(instream_ty s) {
     return read_big_int(s);
}

Text_T StdPrims_read_string(instream_ty s) {
     Text_T ret;

     ret.len = READ_TAG(s);
     ret.str = malloc(ret.len);
     if(ret.str == NULL) die();
     READ_BYTES((char*)ret.str,ret.len,s);
     return ret;
}

StdPrims_identifier_ty StdPrims_read_identifier(instream_ty s) {
     Text_T txt = StdPrims_read_string(s);
     return Atom_new(txt.str,txt.len);
}

void StdPrims_write_int(StdPrims_int_ty x, outstream_ty s) {
     write_int32(x,s);
}

void StdPrims_write_big_int(StdPrims_big_int_ty x, outstream_ty s) {
     write_big_int(x,s);
}

void StdPrims_write_string(StdPrims_string_ty x,outstream_ty s) {
     WRITE_TAG(x.len,s); 
     WRITE_BYTES(x.str,x.len,s);
}
void StdPrims_write_identifier(StdPrims_identifier_ty x,outstream_ty s) {
     Text_T txt;
     txt.len = Atom_length(x);
     txt.str = x;
     StdPrims_write_string(txt,s);
}

void* StdPrims_read_generic_int(instream_ty s) {
   return read_generic_int32(s);
}

void* StdPrims_read_generic_string(instream_ty s) {
  Text_T* ret = malloc(sizeof(Text_T));
  *ret = StdPrims_read_string(s);
  return ret;
}

void* StdPrims_read_generic_identifier(instream_ty s) {
  return (void*)StdPrims_read_identifier(s);
}

void StdPrims_write_generic_int(void *x,instream_ty s) {
     write_generic_int32(x,s);
}

void StdPrims_write_generic_string(void *x,instream_ty s) {
  StdPrims_write_string(*((Text_T*)x),s);
}

void StdPrims_write_generic_identifier(void *x,instream_ty s) {
  StdPrims_write_identifier(x,s);
}
