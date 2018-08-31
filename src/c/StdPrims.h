#ifndef _STD_PRIMS_
#define _STD_PRIMS_
#include <cii/atom.h>
#include <cii/text.h>
#include <cii/mp.h>
#include <stdio.h>
#include <stdlib.h>
#include "pkl-int.h"
#include "StdPkl.h"
typedef Text_T StdPrims_string_ty;
typedef int32 StdPrims_int_ty;
typedef MP_T  StdPrims_big_int_ty;
typedef const char* StdPrims_identifier_ty; /* atom type */

StdPrims_int_ty         StdPrims_read_int(instream_ty s);
StdPrims_big_int_ty     StdPrims_read_big_int(instream_ty s);
StdPrims_string_ty      StdPrims_read_string(instream_ty s);
StdPrims_identifier_ty  StdPrims_read_identifier(instream_ty s);

void  StdPrims_write_int(StdPrims_int_ty x,outstream_ty s);
void  StdPrims_write_big_int(StdPrims_big_int_ty x,outstream_ty s);
void  StdPrims_write_string(StdPrims_string_ty x,outstream_ty s);
void  StdPrims_write_identifier(StdPrims_identifier_ty x,outstream_ty s);

void* StdPrims_read_generic_int(instream_ty s);
void* StdPrims_read_generic_big_int(instream_ty s);
void* StdPrims_read_generic_string(instream_ty s);
void* StdPrims_read_generic_identifier(instream_ty s);

void  StdPrims_write_generic_int(void *x, outstream_ty s);
void  StdPrims_write_generic_big_int(void *x, outstream_ty s);
void  StdPrims_write_generic_string(void *x, outstream_ty s);
void  StdPrims_write_generic_identifier(void *x, outstream_ty s);
#endif
