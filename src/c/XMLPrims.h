#ifndef _XML_PRIMS_
#define _XML_PRIMS_
#include <cii/atom.h>
#include <cii/text.h>
#include <cii/mp.h>
#include <stdio.h>
#include <stdlib.h>
#include "pkl-int.h"
#include "XMLPkl.h"
typedef Text_T XMLPrims_string_ty;
typedef int32 XMLPrims_int_ty;
typedef MP_T  XMLPrims_big_int_ty;
typedef const char* XMLPrims_identifier_ty; /* atom type */

XMLPrims_int_ty         XMLPrims_read_int(instream_ty s);
XMLPrims_big_int_ty     XMLPrims_read_big_int(instream_ty s);
XMLPrims_string_ty      XMLPrims_read_string(instream_ty s);
XMLPrims_identifier_ty  XMLPrims_read_identifier(instream_ty s);

void  XMLPrims_write_int(XMLPrims_int_ty x,outstream_ty s);
void  XMLPrims_write_big_int(XMLPrims_big_int_ty x,outstream_ty s);
void  XMLPrims_write_string(XMLPrims_string_ty x,outstream_ty s);
void  XMLPrims_write_identifier(XMLPrims_identifier_ty x,outstream_ty s);

void* XMLPrims_read_generic_int(instream_ty s);
void* XMLPrims_read_generic_big_int(instream_ty s);
void* XMLPrims_read_generic_string(instream_ty s);
void* XMLPrims_read_generic_identifier(instream_ty s);

void  XMLPrims_write_generic_int(void *x, outstream_ty s);
void  XMLPrims_write_generic_big_int(void *x, outstream_ty s);
void  XMLPrims_write_generic_string(void *x, outstream_ty s);
void  XMLPrims_write_generic_identifier(void *x, outstream_ty s);
#endif
