#ifndef _STD_PKL_
#define _STD_PKL_
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

list_ty StdPkl_read_list(generic_reader_ty rd,instream_ty s);
opt_ty StdPkl_read_option(generic_reader_ty rd,instream_ty s);
share_ty StdPkl_read_share(generic_reader_ty rd,instream_ty s);    

void StdPkl_write_list(generic_writer_ty wr,list_ty v, outstream_ty s);
void StdPkl_write_option(generic_writer_ty wr,opt_ty v, outstream_ty s);
void StdPkl_write_share(generic_writer_ty wr,share_ty v,outstream_ty s); 

void  StdPkl_write_tag(int x,outstream_ty s);
int   StdPkl_read_tag(instream_ty s);
#endif
