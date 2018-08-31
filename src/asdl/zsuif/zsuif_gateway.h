/* file "zsuif_gateway.h" */


/*
       Copyright (c) 1997, 1998 Stanford University

       All rights reserved.

       This software is provided under the terms described in
       the "suif_copyright.h" include file.
*/

#include <suif_copyright.h>


#ifndef ZSUIF_GATEWAY_H
#define ZSUIF_GATEWAY_H

#ifndef SUPPRESS_PRAGMA_INTERFACE
#pragma interface
#endif


/*
      This is the declaration for an example pass that creates a small
      SUIF file.
*/


class zsuif_gateway_pass : public suif_back_end_pass  {
public:
    zsuif_gateway_pass(void)  { }
    virtual ~zsuif_gateway_pass(void)  { }

    virtual lstring name(void) const  { return "zsuif-gateway"; }
    virtual lstring memory_model(void) const { return k_mm_all_in_memory; }
    virtual boolean takes_output_file_name(void) const { return TRUE; }
    virtual boolean takes_multiple_file_names(void) const { return FALSE; }
    virtual void do_all(file_set_block *fsb, string outf);
};


extern "C" void enter_zsuif_gateway(int *argc, char *argv[]);
extern "C" void exit_zsuif_gateway(void);


#endif /* ZSUIF_GATEWAY_H */

