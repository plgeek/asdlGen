/* $Id: thread.h,v 1.1.1.1 1998/02/20 01:31:50 danwang Exp $ */
#ifndef THREAD_INCLUDED
#define THREAD_INCLUDED
#include "except.h"
#define T Thread_T
typedef struct T *T;
extern const Except_T Thread_Failed;
extern const Except_T Thread_Alerted;
extern int  Thread_init (int preempt, ...);
extern T    Thread_new  (int apply(void *),
				void *args, int nbytes, ...);
extern void Thread_exit (int code);
extern void Thread_alert(T t);
extern T    Thread_self (void);
extern int  Thread_join (T t);
extern void Thread_pause(void);
#undef T
#endif
