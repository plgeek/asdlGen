#ifndef _XML_PKL_
#define _XML_PKL_
#include <cii/seq.h>
#include <stdio.h>
#include <stdlib.h>
#include "share.h"
typedef FILE* instream_ty;
typedef FILE* outstream_ty;
typedef Seq_T list_ty;
typedef void* opt_ty;

#define die() (fprintf(stderr,"%s:%d: Fatal Error\n",__FILE__,__LINE__), \
		exit(-1))

typedef void *(*generic_reader_ty)(instream_ty s);
typedef void (*generic_writer_ty)(void *x,outstream_ty s);

void XMLPkl_write_element_begin(const char* n,outstream_ty s);
void XMLPkl_write_element_end(const char* n,outstream_ty s);
void XMLPkl_read_element_begin(const char* n,instream_ty s);
void XMLPkl_read_element_end(const char* n,instream_ty s);
int XMLPkl_read_tagged_element(instream_ty s);

list_ty XMLPkl_read_list(const char* n,
		      generic_reader_ty rd,instream_ty s);

opt_ty XMLPkl_read_option(const char *n, 
		       generic_reader_ty rd,instream_ty s);

void XMLPkl_write_list(const char *n, 
		    generic_writer_ty rd,list_ty x,outstream_ty s);

void XMLPkl_write_option(const char *n,
		     generic_writer_ty rd,opt_ty x,outstream_ty s);

struct XMLPkl_tag_map_entry_s {
  const char* name;
  int tag;
};
extern struct XMLPkl_tag_map_entry_s XMLPkl_tag_map[];
#endif /* _XML_PKL_ */

