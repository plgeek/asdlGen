#include "gpickle.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define T(x) TypePickle_##x
#define V(x) AsdlValue_##x
#define P(x) GPickle_##x

static T(qid_ty) str2qid(const char *s) {
     char buf[1024]; /* hack */
     char *dot,*ptr;
     identifier_ty base;
     list_ty qualifier = Seq_new(1);
     strcpy(buf,s);

     ptr = buf;
     dot = strchr(ptr,'.');

     if(dot == NULL) {
	  base = Atom_string(ptr);
     } else {
	  *dot = '\0';
          /* hack not as fully general as it should be */
	  Seq_addhi(qualifier,(void*)Atom_string(ptr));
	  base = Atom_string(dot+1);
     }
     return T(qid)(qualifier,base);
}

static int type_eq(P(maps_ty) m, T(qid_ty) x,T(qid_ty) y) {
     int xidx = P(lookup_type_idx_by_name)(m,x);
     int yidx = P(lookup_type_idx_by_name)(m,y);
     return (xidx == yidx);
}

extern int getopt(int, char *const *, const char *);
extern char *optarg;
extern int optind;

int main(int argc,char **argv) {
     int c;

     int tflg = 0;
     int gflg = 0;
     int errflg = 0;
     instream_ty  efile = NULL;
     instream_ty  ifile = stdin;
     outstream_ty ofile = stdout;
 
     T(qid_ty) name;
     T(type_env_ty) te;
     V(asdl_value_ty) v;
     P(maps_ty) m;

     while ((c = getopt(argc, argv, "gti:o:e:")) != EOF) {
	  switch (c) {
	  case 't':
	       if (gflg) { errflg = 1; }
	       else { tflg = 1; }
	       break;
	  case 'g':
	       if (tflg) { errflg = 1; }
	       else { gflg = 1; }
	       break;
	  case 'o': /* b for binary mode  ignored on unix */
	       ofile = fopen(optarg,"wb"); 
	       fprintf(stderr,"ouput file = %s\n", optarg);
	       break;
	  case 'i': /* b for binary mode  ignored on unix */
	       ifile = fopen(optarg,"rb");
	       fprintf(stderr,"input file = %s\n", optarg);
	       break;
	  case 'e': /* b for binary mode  ignored on unix */
	       efile = fopen(optarg,"rb");
	       fprintf(stderr,"env file = %s\n", optarg);
	       break;
	  case '?':
	       errflg++;
	  }
     }

     if (errflg 
	 || (efile==NULL) 
	 || (ifile==NULL) 
	 || (ofile==NULL) 
	 || (!tflg && !gflg)) {
	  fprintf(stderr,
 "usage: %s {-t|-g} -e env.typ [-o <filen>] [-i <fname>]  mod.type ... \n",
		  argv[0]);
	  exit (2);
     }

     /* read the type environment */
     te = T(read_type_env)(efile);
     m = P(make_maps)(te);  
     fclose(efile);
     
     if(gflg) { /* convert typed to generic */
	  for ( ; optind < argc; optind++) {
	       name = str2qid(argv[optind]);
	       v = P(read_asdl_value)(m,name,ifile);
	       if(v != NULL) {
		    V(write_asdl_value)(v,ofile);
		    fprintf(stderr,"Wrote typed value %s as generic value\n",
			    argv[optind]);
	       } else {
		    fprintf(stderr,"Error reading typed value %s\n",
			    argv[optind]);
	       }
	  }
     }

     if(tflg) { /* convert generic to typed*/
	  for ( ; optind < argc; optind++) {
	       name = str2qid(argv[optind]);
	       v = V(read_asdl_value)(ifile);
	       if(type_eq(m,name,v->typename)) {
		    fprintf(stderr,"Wrote generic value %s as typed value\n",
			    argv[optind]);
	       } else {
		    fprintf(stderr,"Expected generic value %s\n",argv[optind]);
	       }
		    P(write_asdl_value)(m,v,ofile);
	  }
     }
     fclose(ifile);
     fclose(ofile);
     return 0;
}
