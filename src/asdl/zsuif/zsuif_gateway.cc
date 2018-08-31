/* file "zsuif_gateway.cc */


#define _MODULE_ "libzsuif_gateway"

#pragma implementation "zsuif_gateway.h"


#include <suif.h>
#include <suifpasses.h>
#include <limits.h>
#include "zsuif_gateway.h"
#include "trans_suif.h"

void zsuif_gateway_pass::do_all(file_set_block* fsb,string outf) {
  trans_suif t;
  FILE *outs;

  
  outs = fopen(outf.chars(),"wb");
  if (outs == NULL) {
    error(-1,"zsuif_gateway: error opening file %s for output\n",
	  outf.chars());
  }
  
  info(i_integer(1),"Translationg to zsuif\n");
  zsuif_file_set_block* ret = t.trans(fsb);

  info(i_integer(1),"writting pickle to %s\n",outf.chars());
  ret->write(ret,outs);

  info(i_integer(1),"wrote pickle\n");
  fclose(outs);
  
}


extern "C" void enter_zsuif_gateway(int *argc, char *argv[])
  {
    enter_suifpasses(argc, argv);
    enter_suif(argc, argv);
    register_suif_pass(new zsuif_gateway_pass());
  }

extern "C" void exit_zsuif_gateway(void)
  {
    /* empty */
  }


