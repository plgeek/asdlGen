#ifndef _ASDL_BASE_XX_
#define _ASDL_BASE_XX_

#include <stdio.h>
typedef FILE* outstream;
typedef FILE* instream;


extern "C" {
#include "pkl-int.h"
}
#define read_int read_C_signed_int
#define write_int write_C_signed_int

#include "sty.h"

typedef lstring identifier;
typedef string  string_option;
typedef identifier identifier_option;
typedef int* int_option;



template <class T> 
class List {
public:
  T head;
  List<T>* tail;
  inline List(T head, List<T>* tail) {
    { this->head = head; this->tail = tail; }
  }
};

typedef List<int> int_list;
typedef List<string> string_list;
typedef List<identifier> identifier_list;

extern void   write_tag(int x,outstream s);
extern int    read_tag(instream s);

extern void          write_int(int x,outstream s);
extern void          write_int_option(int_option x,outstream s);
extern void          write_int_list(int_list* x, outstream s);

extern int           read_int(instream s);
extern int_option    read_int_option(instream s);
extern int_list*     read_int_list(instream s);

extern void          write_string(string x,outstream s);
extern void          write_string_option(string_option x,outstream s);
extern void          write_string_list(string_list* x, outstream s);

extern string        read_string(instream s);
extern string_option read_string_option(instream s);
extern string_list*  read_string_list(instream s);

extern void          write_identifier(identifier x,outstream s);
extern void          write_identifier_option(identifier_option x,outstream s);
extern void          write_identifier_list(identifier_list* x, outstream s);

extern identifier        read_identifier(instream s);
extern identifier_option read_identifier_option(instream s);
extern identifier_list*  read_identifier_list(instream s);
extern void die();


#endif





