#ifndef _StdPrims_XX_
#define _StdPrims_XX_
#include "StdPkl.hxx"

typedef int  StdPrims_int;
typedef char* StdPrims_string;
typedef char* StdPrims_identifier;
typedef MP_T StdPrims_big_int;

extern void  StdPrims_write_int(StdPrims_int x, outstream s);
extern StdPrims_int        StdPrims_read_int(instream s);

extern void  StdPrims_write_big_int(StdPrims_big_int x, outstream s);
extern StdPrims_big_int        StdPrims_read_big_int(instream s);

extern void StdPrims_write_string(StdPrims_string x, outstream s);
extern StdPrims_string StdPrims_read_string(instream s);

// identifiers
extern void StdPrims_write_identifier(StdPrims_identifier x, outstream s);
extern StdPrims_identifier        StdPrims_read_identifier(instream s);

#endif
