#ifndef _STD_PKL_XX_
#define _STD_PKL_XX_
#include <stdio.h>
#include <assert.h>
typedef FILE* outstream;
typedef FILE* instream;

extern "C" {
#include "pkl-int.h"
}

template <class T> 
class Seq {
private:
  int len_;
  T* elems;
public:  
  Seq() { len_= 0; elems = NULL;}
  Seq(int len) {
    elems = new T[len];
  }

  Seq<T>& operator = (Seq<T> x) {
    len_ = x.len_;
    elems = x.elems;
    return *this;
  }

  int len() { return len_; }
  T get(int idx) { 
    assert((idx >= 0) && (idx < len_));
    return elems[idx];
  }
  void set(int idx,T v) {
    assert((idx >= 0) && (idx < len_));
    elems[idx] = v;
  }
};
template <class T>
class Opt {
private:
 T v_;
 int is_some_;
public:
  Opt(void) { is_some_ = 0;}
  Opt(T v) { v_ = v; is_some_ = 1;}
  Opt<T>& operator = (Opt<T> x) {
    v_ = x.v_;
    is_some_ = x.is_some_;
    return *this;
  }
  inline int is_some(void) { return is_some_; }
  inline T get_val(void) {  assert(is_some_); return v_; }
};
class Error {
  public:
  char* msg;
  Error(char* x) { msg = x; }
};
void StdPkl_write_tag(int, outstream);
int StdPkl_read_tag(instream);
#endif
