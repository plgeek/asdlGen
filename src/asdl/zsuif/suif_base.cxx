#include "suif_base.hxx"
#include <stdlib.h>
#include <string.h>

#define WRITE_BYTES(x,sz,s) (fwrite(x,sizeof(char),sz,s))
#define READ_BYTES(x,sz,s) (fread(x,sizeof(char),sz,s))

void write_tag(int x,outstream s) { write_uint32(x,s); }
int read_tag(instream s) { return read_uint32(s); }


void write_string(string x,outstream s) {
  if (x.is_empty()) {
    write_tag(0,s); 
  } else {
    int sz = strlen(x.chars());
    write_tag(sz,s); 
    WRITE_BYTES(x.chars(),sz,s);
  }
}

string read_string(instream s) {
  int sz = read_tag(s);
  string ret = string();
  char *buf = new char[sz+1];
  
  READ_BYTES(buf,sz,s);
  buf[sz]='\0';
  ret.set_chars(buf);
  delete buf;
  return ret;
}

void write_identifier(identifier x,outstream s) {
  write_string(x,s);
}

static char* uniquify(char *x);

identifier read_identifier(instream s) {
  return lstring(read_string(s));
}


void write_int_option(int_option x,outstream s) {
    if(x!=NULL) {
      write_int(1,s);
      write_int(*x,s);
    } else {
      write_int(0,s);
    }
}

int_option read_int_option(instream s) {

    if(read_tag(s)!=0) {
     int *ret = new int; 
     *ret = read_int(s);
      return ret;
    } else {
      return NULL;
    }
}

void write_string_option(string_option x,outstream s) {
    if(x.chars()!=NULL) {
      write_tag(1,s);
      write_string(x,s);
    } else {
      write_tag(0,s);
    }
}

string_option read_string_option(instream s) {
    if(read_tag(s)!=0) {
      return (read_string(s));
    } else {
      return NULL;
    }
}

void write_identifier_option(identifier_option x,outstream s) {
    if(x.chars()!=NULL) {
      write_tag(1,s);
      write_identifier(x,s);
    } else {
      write_tag(0,s);
    }
}
int_list* read_int_list(instream s)
{
     int_list* t;
     
     {
          int t1;
          int_list* t2;
          t1 = read_tag(s);
          if(t1 != 0)
              t = new int_list(read_int(s),NULL);
          else
              return NULL;
          t1 = t1 - 1;
          t2 = t;
          while(t1 != 0)
          {
               t2->tail = new int_list(read_int(s), NULL);
               t2 = t2->tail;
               t1 = t1 - 1;
          }
     }
     return t;
}

void write_int_list(int_list* x, outstream s)
{
     int t1;
     int_list* t2;
     t1 = 0;
     t2 = x;
     while(t2 != NULL)
     {
          t2 = t2->tail;
          t1 = t1 + 1;
     }
     write_tag(t1, s);
     t2 = x;
     while(t1 != 0)
     {
          write_int(t2->head, s);
          t2 = t2->tail;
          t1 = t1 - 1;
     }
}

void write_string_list(string_list* x, outstream s)
{
     int t1;
     string_list* t2;
     t1 = 0;
     t2 = x;
     while(t2 != NULL)
     {
          t2 = t2->tail;
          t1 = t1 + 1;
     }
     write_tag(t1, s);
     t2 = x;
     while(t1 != 0)
     {
          write_string(t2->head, s);
          t2 = t2->tail;
          t1 = t1 - 1;
     }
}
string_list* read_string_list(instream s)
{
     string_list* t;
     
     {
          int t1;
          string_list* t2;
          t1 = read_tag(s);
          if(t1 != 0)
              t = new string_list(read_string(s),
                   NULL);
          else
              return NULL;
          t1 = t1 - 1;
          t2 = t;
          while(t1 != 0)
          {
               t2->tail = new string_list(read_string(s),
                           NULL);
               t2 = t2->tail;
               t1 = t1 - 1;
          }
     }
     return t;
}
void write_identifier_list(identifier_list* x, outstream s)
{
     int t1;
     identifier_list* t2;
     t1 = 0;
     t2 = x;
     while(t2 != NULL)
     {
          t2 = t2->tail;
          t1 = t1 + 1;
     }
     write_tag(t1, s);
     t2 = x;
     while(t1 != 0)
     {
          write_identifier(t2->head, s);
          t2 = t2->tail;
          t1 = t1 - 1;
     }
}
identifier_list* read_identifier_list(instream s)
{
     identifier_list* t;
     
     {
          int t1;
          identifier_list* t2;
          t1 = read_tag(s);
          if(t1 != 0)
              t = new identifier_list(read_identifier(s),
                   NULL);
          else
              return NULL;
          t1 = t1 - 1;
          t2 = t;
          while(t1 != 0)
          {
               t2->tail = new identifier_list(read_identifier(s),
                           NULL);
               t2 = t2->tail;
               t1 = t1 - 1;
          }
     }
     return t;
}


identifier_option read_identifier_option(instream s) {
    if(read_tag(s)!=0) {
      return (read_identifier(s));
    } else {
      return lstring();
    }
}


void die() { 
  fputs("Pickler error\n",stderr);
  exit(-1); 
}
