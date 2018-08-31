static char rcsid[] = "$Id: assert.c,v 1.1.1.1 1998/02/20 01:31:46 danwang Exp $";
#include "assert.h"
const Except_T Assert_Failed = { "Assertion failed" };
void (assert)(int e) {
	assert(e);
}
