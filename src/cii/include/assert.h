/* $Id: assert.h,v 1.1.1.1 1998/02/20 01:31:48 danwang Exp $ */
#undef assert
#ifdef NDEBUG
#define assert(e) ((void)0)
#else
#include "except.h"
extern void assert(int e);
#define assert(e) ((void)((e)||(RAISE(Assert_Failed),0)))
#endif
