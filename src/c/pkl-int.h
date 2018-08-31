#ifndef _Pkl_int_
#define _Pkl_int_
#include <stdio.h>
#include <cii/mp.h>
/* typdefs based on minimum ANSI C guarantees */
/* note this is a mess need to find a better way to do this */
typedef unsigned int nat;
#ifndef __cplusplus
typedef enum {false, true} bool;
#endif
typedef double ieee_real;

typedef signed char int8;
typedef signed short int int16;
typedef signed long  int int32;
typedef unsigned char uint8;
typedef unsigned short int uint16;
typedef unsigned long  int uint32;
typedef MP_T big_int;
#if 0  
/* use long long in future */
typedef   signed long long int64;
typedef unsigned long long uint64;
#else
typedef big_int int64;
typedef big_int uint64;
#endif
#ifdef __cplusplus
extern "C" {
#endif
big_int   read_big_int(FILE *s);
nat       read_nat(FILE *s);
bool      read_bool(FILE *s);
ieee_real read_ieee_real(FILE *s);

int8  read_int8(FILE *s);
int16 read_int16(FILE *s);
int32 read_int32(FILE *s);
int64 read_int64(FILE *s);

uint8  read_uint8(FILE *s);
uint16 read_uint16(FILE *s);
uint32 read_uint32(FILE *s);
uint64 read_uint64(FILE *s);

void write_big_int(big_int x,FILE *s);
void write_nat(nat x,FILE *s);
void write_bool(bool x,FILE *s);
void write_ieee_real(ieee_real x,FILE *s);

void write_int8(int8 x,FILE *s);
void write_int16(int16 x,FILE *s);
void write_int32(int32 x,FILE *s);
void write_int64(int64 x,FILE *s);

void write_uint8(uint8 x,FILE *s);
void write_uint16(uint16 x,FILE *s);
void write_uint32(uint32 x,FILE *s);
void write_uint64(uint64 x,FILE *s);

void* read_generic_nat(FILE *s);
void* read_generic_bool(FILE *s);
void* read_generic_ieee_real(FILE *s);

void* read_generic_int8(FILE *s);
void* read_generic_int16(FILE *s);
void* read_generic_int32(FILE *s);
void* read_generic_int64(FILE *s);

void* read_generic_uint8(FILE *s);
void* read_generic_uint16(FILE *s);
void* read_generic_uint32(FILE *s);
void* read_generic_uint64(FILE *s);

void write_generic_nat(void* x, FILE *s);
void write_generic_bool(void* x, FILE *s);
void write_generic_ieee_real(void* x, FILE *s);

void write_generic_int8(void* x, FILE *s);
void write_generic_int16(void* x, FILE *s);
void write_generic_int32(void* x, FILE *s);
void write_generic_int64(void* x, FILE *s);

void write_generic_uint8(void* x, FILE *s);
void write_generic_uint16(void* x, FILE *s);
void write_generic_uint32(void* x, FILE *s);
void write_generic_uint64(void* x, FILE *s);
#ifdef __cplusplus
}
#endif
#endif
