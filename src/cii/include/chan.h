/* $Id: chan.h,v 1.1.1.1 1998/02/20 01:31:49 danwang Exp $ */
#ifndef CHAN_INCLUDED
#define CHAN_INCLUDED
#define T Chan_T
typedef struct T *T;
extern T   Chan_new    (void);
extern int Chan_send   (T c, const void *ptr, int size);
extern int Chan_receive(T c,       void *ptr, int size);
#undef T
#endif
