#include "pkl-int.h"
#include <stdlib.h>
#include <limits.h>

#define SET_NEG(x) (x | (0x40))
#define IS_NEG_BIT_SET(x) (x & (0x40))
#define MASK_NEG(x) (x & 0x3f)

#define SET_CONTINUE(x) (x | (0x80))
#define IS_CONTINUE_BIT_SET(x) (x & (0x80))
#define NIBBLE(x) (x & (0x7f))

#define WRITE_BYTE(x,s) (putc(x,s))
#define WRITE_BYTES(x,sz,s) (fwrite(x,sizeof(char),sz,s))
#define READ_BYTE(x,s) (x=getc(s))
#define READ_BYTES(x,sz,s) (fread(x,sizeof(char),sz,s))
#define die() (fprintf(stderr,"%s:%d: Fatal Error\n",__FILE__,__LINE__), \
		exit(-1))

/* todo handle IO errors */     
/* declare static so gcc might inline it  */
static signed long read_C_signed_long(FILE* s) {
  signed long acc = 0L;
  int shift = 0;
  int x;
  READ_BYTE(x,s);
  while(IS_CONTINUE_BIT_SET(x)) {
    acc |= (NIBBLE(x)<<shift);
    shift+=7;
    READ_BYTE(x,s);
  }
  /* Check the sign first to handle 2's complement asymmetry */
  if(IS_NEG_BIT_SET(x)) {
    acc = -acc;
    acc -= (MASK_NEG(x) << shift);
  } else {
    acc += (MASK_NEG(x) << shift);
  }
  return acc;   
}

static unsigned long read_C_unsigned_long(FILE* s) {
  unsigned long acc = 0L;
  int shift = 0;
  int x;
  
  READ_BYTE(x,s);
  while(IS_CONTINUE_BIT_SET(x)) {
    acc |= (NIBBLE(x)<<shift);
    shift+=7;
    READ_BYTE(x,s);
  }
  if(IS_NEG_BIT_SET(x)) {
    fprintf(stderr,"Warrning ignoring sign bit on unsigned read\n");
  }
  acc += (MASK_NEG(x) << shift);
  return acc;   
}
static MP_T read_cii_MP_T(FILE *s) {
     MP_T acc = MP_new(0L);
     MP_T tmp = MP_new(0L);
     int shift = 0;
     int x;

     READ_BYTE(x,s);
     while(IS_CONTINUE_BIT_SET(x)) {
	  MP_fromint(tmp,NIBBLE(x));
	  MP_lshift(tmp,tmp,shift);
	  MP_or(acc,acc,tmp);
	  shift+=7;
	  READ_BYTE(x,s);
     }
     MP_fromint(tmp,MASK_NEG(x));
     MP_lshift(tmp,tmp,shift);
     MP_or(acc,acc,tmp);
     if(IS_NEG_BIT_SET(x)) {
	  MP_neg(acc,acc);
     }
     free(tmp);
     return acc;
}

static void write_C_signed_long(signed long x, FILE* s) {
  int is_neg  =  (x < 0) ;      
   if (x == LONG_MIN) {
    /* handle 2's complement asymmetry */
    WRITE_BYTE(SET_CONTINUE(0),s);
    x = labs(LONG_MIN / 128);
  } else {
    x = labs(x);
  }
  while( x > 63) {
    WRITE_BYTE(SET_CONTINUE(NIBBLE(x)),s);
    x >>= 7;
  }
  if(is_neg) { WRITE_BYTE(SET_NEG(x),s); }
  else { WRITE_BYTE(x,s); }
}

static void write_C_unsigned_long(unsigned long x, FILE* s) {
  while( x > 63) {
    WRITE_BYTE(SET_CONTINUE(NIBBLE(x)),s);
    x >>= 7;
  }
  WRITE_BYTE(x,s); 
}
static void write_cii_MP_T(MP_T x,FILE* s) {
     int set_neg_bit;
     int v;
     MP_T tmp = MP_new(0L);
     MP_addi(tmp,x,0L);
     set_neg_bit = (MP_cmpi(x,0L) < 0);

     if(set_neg_bit) { MP_neg(tmp,tmp); }

     while( MP_cmpi(tmp,63L) > 0) {
	  /* v is the lower order 7 bits and the continue flag set*/
	  v = (SET_CONTINUE(NIBBLE(tmp[0])));
	  WRITE_BYTE(v,s);
	  MP_rshift(tmp,tmp,7);
     }
     /* v is the lower order 7 bits and the continue flag unset*/
     v = tmp[0];
     if(set_neg_bit) { v = SET_NEG(v); }
     WRITE_BYTE(v,s);
     free(tmp); 
}
big_int read_big_int(FILE* s) { return read_cii_MP_T(s); }
nat read_nat(FILE* s) { return read_C_unsigned_long(s); }
bool read_bool(FILE *s) {
  switch (read_C_unsigned_long(s)) {
  case 1 : return false; /* this is not a bug */
  case 2 : return true; /* this is not a bug */
  default : die();
  }
  return false; /* not reached */
}
ieee_real read_ieee_real(FILE *s) { die(); return 0.0; /* not reached */}

int8  read_int8(FILE* s) { return read_C_signed_long(s); }
int16 read_int16(FILE* s) { return read_C_signed_long(s); }
int32 read_int32(FILE* s) { return read_C_signed_long(s); }
int64 read_int64(FILE* s) { return read_cii_MP_T(s); }


uint8  read_uint8(FILE* s) { return read_C_unsigned_long(s); }
uint16 read_uint16(FILE* s) { return read_C_unsigned_long(s); }
uint32 read_uint32(FILE* s) { return read_C_unsigned_long(s); }
uint64 read_uint64(FILE* s) { return read_cii_MP_T(s); }

void write_nat(nat x,FILE* s) { write_C_unsigned_long(x,s); }
void write_bool(bool x,FILE *s) {
  switch (x) {
  case false : write_C_unsigned_long(1,s); break;
  case true  :  write_C_unsigned_long(2,s); break;
  default : die();
  }
}
void write_big_int(big_int x, FILE *s) { write_cii_MP_T(x,s); }
void write_ieee_real(ieee_real x,FILE *s) { die(); }
void write_int8(int8 x, FILE *s)   { write_C_signed_long(x,s); }
void write_int16(int16 x, FILE *s) { write_C_signed_long(x,s); }
void write_int32(int32 x, FILE *s) { write_C_signed_long(x,s); }
void write_int64(int64 x, FILE *s) { write_cii_MP_T(x,s); }

void write_uint8(uint8 x, FILE *s)   { write_C_unsigned_long(x,s); }
void write_uint16(uint16 x, FILE *s) { write_C_unsigned_long(x,s); }
void write_uint32(uint32 x, FILE *s) { write_C_unsigned_long(x,s); }
void write_uint64(uint64 x, FILE *s) { write_cii_MP_T(x,s); }

#define DECL_READ_GENERIC(t) \
 void* read_generic_##t(FILE* s) { \
      t* ret = (t*) malloc(sizeof(*ret)); \
      if (ret == NULL) die(); \
      *ret = read_##t(s); \
      return ret; }

#define DECL_WRITE_GENERIC(t) \
 void write_generic_##t(void* x, FILE* s) { \
      write_##t(*((t*)x), s); }

DECL_READ_GENERIC(nat)
DECL_READ_GENERIC(bool)
DECL_READ_GENERIC(ieee_real)
DECL_READ_GENERIC(int8)
DECL_READ_GENERIC(int16)
DECL_READ_GENERIC(int32)
void *read_generic_int64(FILE *s) {
  return (void*)(read_cii_MP_T(s));
}
void *read_generic_big_int(FILE *s) {
  return (void*)(read_cii_MP_T(s));
}

DECL_READ_GENERIC(uint8)
DECL_READ_GENERIC(uint16)
DECL_READ_GENERIC(uint32)
void *read_generic_uint64(FILE *s) {
   return (void*)(read_cii_MP_T(s));
}

DECL_WRITE_GENERIC(nat)
DECL_WRITE_GENERIC(bool)
DECL_WRITE_GENERIC(ieee_real)

DECL_WRITE_GENERIC(int8)
DECL_WRITE_GENERIC(int16)
DECL_WRITE_GENERIC(int32)
void write_generic_int64(void *x,FILE *s) {
  write_cii_MP_T((MP_T)x,s);
}
void write_generic_big_int(void *x,FILE *s) {
  write_cii_MP_T((MP_T)x,s);
}

DECL_WRITE_GENERIC(uint8)
DECL_WRITE_GENERIC(uint16)
DECL_WRITE_GENERIC(uint32)
void write_generic_uint64(void *x,FILE *s) {
  write_cii_MP_T((MP_T)x,s);
}





