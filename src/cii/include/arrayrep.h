/* $Id: arrayrep.h,v 1.1.1.1 1998/02/20 01:31:48 danwang Exp $ */
#ifndef ARRAYREP_INCLUDED
#define ARRAYREP_INCLUDED
#define T Array_T
struct T {
	int length;
	int size;
	char *array;
};
extern void ArrayRep_init(T array, int length,
	int size, void *ary);
#undef T
#endif
