/* $Id: atom.h,v 1.1.1.1 1998/02/20 01:31:49 danwang Exp $ */
#ifndef ATOM_INCLUDED
#define ATOM_INCLUDED
extern       int   Atom_length(const char *str);
extern const char *Atom_new   (const char *str, int len);
extern const char *Atom_string(const char *str);
extern const char *Atom_int   (long n);
#endif
