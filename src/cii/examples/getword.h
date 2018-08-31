/* $Id: getword.h,v 1.1.1.1 1998/02/20 01:31:50 danwang Exp $ */
#include <stdio.h>
extern int getword(FILE *fp, char *buf, int size,
	int first(int c), int rest(int c));
