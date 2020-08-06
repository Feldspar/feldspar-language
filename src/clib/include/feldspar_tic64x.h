//
// Copyright (c) 2009-2011, ERICSSON AB
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//     * Redistributions of source code must retain the above copyright notice,
//       this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of the ERICSSON AB nor the names of its contributors
//       may be used to endorse or promote products derived from this software
//       without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

#ifndef FELDSPAR_TI_C64X_H
#define FELDSPAR_TI_C64X_H



char pow_fun_char(char, char);
short pow_fun_short(short, short);
int pow_fun_int(int, int);
long pow_fun_long(long, long);
long long pow_fun_llong(long long, long long);
unsigned char pow_fun_uchar(unsigned char, unsigned char);
unsigned short pow_fun_ushort(unsigned short, unsigned short);
unsigned pow_fun_uint(unsigned, unsigned);
unsigned long pow_fun_ulong(unsigned long, unsigned long);
unsigned long long pow_fun_ullong(unsigned long long, unsigned long long);

char abs_fun_char(char);
short abs_fun_short(short);
long abs_fun_long(long);
long long abs_fun_llong(long long);

char signum_fun_char(char);
short signum_fun_short(short);
int signum_fun_int(int);
long signum_fun_long(long);
long long signum_fun_llong(long long);
unsigned char signum_fun_uchar(unsigned char);
unsigned short signum_fun_ushort(unsigned short);
unsigned signum_fun_uint(unsigned);
unsigned long signum_fun_ulong(unsigned long);
unsigned long long signum_fun_ullong(unsigned long long);
float signum_fun_float(float);

float logBase_fun_float(float, float);



char setBit_fun_char(char, unsigned);
short setBit_fun_short(short, unsigned);
int setBit_fun_int(int, unsigned);
long setBit_fun_long(long, unsigned);
long long setBit_fun_llong(long long, unsigned);
unsigned char setBit_fun_uchar(unsigned char, unsigned);
unsigned short setBit_fun_ushort(unsigned short, unsigned);
unsigned setBit_fun_uint(unsigned, unsigned);
unsigned long setBit_fun_ulong(unsigned long, unsigned);
unsigned long long setBit_fun_ullong(unsigned long long, unsigned);

char clearBit_fun_char(char, unsigned);
short clearBit_fun_short(short, unsigned);
int clearBit_fun_int(int, unsigned);
long clearBit_fun_long(long, unsigned);
long long clearBit_fun_llong(long long, unsigned);
unsigned char clearBit_fun_uchar(unsigned char, unsigned);
unsigned short clearBit_fun_ushort(unsigned short, unsigned);
unsigned clearBit_fun_uint(unsigned, unsigned);
unsigned long clearBit_fun_ulong(unsigned long, unsigned);
unsigned long long clearBit_fun_ullong(unsigned long long, unsigned);

char complementBit_fun_char(char, unsigned);
short complementBit_fun_short(short, unsigned);
int complementBit_fun_int(int, unsigned);
long complementBit_fun_long(long, unsigned);
long long complementBit_fun_llong(long long, unsigned);
unsigned char complementBit_fun_uchar(unsigned char, unsigned);
unsigned short complementBit_fun_ushort(unsigned short, unsigned);
unsigned complementBit_fun_uint(unsigned, unsigned);
unsigned long complementBit_fun_ulong(unsigned long, unsigned);
unsigned long long complementBit_fun_ullong(unsigned long long, unsigned);

int testBit_fun_char(char, unsigned);
int testBit_fun_short(short, unsigned);
int testBit_fun_int(int, unsigned);
int testBit_fun_long(long, unsigned);
int testBit_fun_llong(long long, unsigned);
int testBit_fun_uchar(unsigned char, unsigned);
int testBit_fun_ushort(unsigned short, unsigned);
int testBit_fun_uint(unsigned, unsigned);
int testBit_fun_ulong(unsigned long, unsigned);
int testBit_fun_ullong(unsigned long long, unsigned);

char rotateL_fun_char(char, int);
short rotateL_fun_short(short, int);
long rotateL_fun_long(long, int);
long long rotateL_fun_llong(long long, int);
int rotateL_fun_int(int, int);
unsigned char rotateL_fun_uchar(unsigned char, int);
unsigned short rotateL_fun_ushort(unsigned short, int);
unsigned long rotateL_fun_ulong(unsigned long, int);
unsigned long long rotateL_fun_ullong(unsigned long long, int);

char rotateR_fun_char(char, int);
short rotateR_fun_short(short, int);
int rotateR_fun_int(int, int);
long rotateR_fun_long(long, int);
long long rotateR_fun_llong(long long, int);
unsigned char rotateR_fun_uchar(unsigned char, int);
unsigned short rotateR_fun_ushort(unsigned short, int);
unsigned rotateR_fun_uint(unsigned, int);
unsigned long rotateR_fun_ulong(unsigned long, int);
unsigned long long rotateR_fun_ullong(unsigned long long, int);

char reverseBits_fun_char(char);
short reverseBits_fun_short(short);
int reverseBits_fun_int(int);
long reverseBits_fun_long(long);
long long reverseBits_fun_llong(long long);
unsigned char reverseBits_fun_uchar(unsigned char);
unsigned short reverseBits_fun_ushort(unsigned short);
unsigned long reverseBits_fun_ulong(unsigned long);
unsigned long long reverseBits_fun_ullong(unsigned long long);

unsigned bitScan_fun_char(char);
unsigned bitScan_fun_short(short);
unsigned bitScan_fun_int(int);
unsigned bitScan_fun_long(long);
unsigned bitScan_fun_llong(long long);
unsigned bitScan_fun_uchar(unsigned char);
unsigned bitScan_fun_ushort(unsigned short);
unsigned bitScan_fun_uint(unsigned);
unsigned bitScan_fun_ulong(unsigned long);
unsigned bitScan_fun_ullong(unsigned long long);

unsigned bitCount_fun_char(char);
unsigned bitCount_fun_short(short);
unsigned bitCount_fun_int(int);
unsigned bitCount_fun_long(long);
unsigned bitCount_fun_llong(long long);
unsigned bitCount_fun_uchar(unsigned char);
unsigned bitCount_fun_ushort(unsigned short);
unsigned bitCount_fun_ulong(unsigned long);
unsigned bitCount_fun_ullong(unsigned long long);



typedef struct {
    char re;
    char im;
} complexOf_char;

typedef struct {
    int re;
    int im;
} complexOf_int;

typedef struct {
    long re;
    long im;
} complexOf_long;

typedef struct {
    long long re;
    long long im;
} complexOf_llong;

typedef struct {
    unsigned char re;
    unsigned char im;
} complexOf_uchar;

typedef struct {
    unsigned re;
    unsigned im;
} complexOf_uint;

typedef struct {
    unsigned long re;
    unsigned long im;
} complexOf_ulong;

typedef struct {
    unsigned long long re;
    unsigned long long im;
} complexOf_ullong;

typedef struct {
    float re;
    float im;
} complexOf_float;

int equal_fun_complexOf_char(complexOf_char, complexOf_char);
int equal_fun_complexOf_int(complexOf_int, complexOf_int);
int equal_fun_complexOf_long(complexOf_long, complexOf_long);
int equal_fun_complexOf_llong(complexOf_llong, complexOf_llong);
int equal_fun_complexOf_uchar(complexOf_uchar, complexOf_uchar);
int equal_fun_complexOf_uint(complexOf_uint, complexOf_uint);
int equal_fun_complexOf_ulong(complexOf_ulong, complexOf_ulong);
int equal_fun_complexOf_ullong(complexOf_ullong, complexOf_ullong);
int equal_fun_complexOf_float(complexOf_float, complexOf_float);

complexOf_char negate_fun_complexOf_char(complexOf_char);
complexOf_int negate_fun_complexOf_int(complexOf_int);
complexOf_long negate_fun_complexOf_long(complexOf_long);
complexOf_llong negate_fun_complexOf_llong(complexOf_llong);
complexOf_uchar negate_fun_complexOf_uchar(complexOf_uchar);
complexOf_uint negate_fun_complexOf_uint(complexOf_uint);
complexOf_ulong negate_fun_complexOf_ulong(complexOf_ulong);
complexOf_ullong negate_fun_complexOf_ullong(complexOf_ullong);
complexOf_float negate_fun_complexOf_float(complexOf_float);

complexOf_char abs_fun_complexOf_char(complexOf_char);
complexOf_int abs_fun_complexOf_int(complexOf_int);
complexOf_long abs_fun_complexOf_long(complexOf_long);
complexOf_llong abs_fun_complexOf_llong(complexOf_llong);
complexOf_uchar abs_fun_complexOf_uchar(complexOf_uchar);
complexOf_uint abs_fun_complexOf_uint(complexOf_uint);
complexOf_ulong abs_fun_complexOf_ulong(complexOf_ulong);
complexOf_ullong abs_fun_complexOf_ullong(complexOf_ullong);
complexOf_float abs_fun_complexOf_float(complexOf_float);
unsigned abs_fun_complexOf_short(unsigned);
unsigned abs_fun_complexOf_ushort(unsigned);

complexOf_char signum_fun_complexOf_char(complexOf_char);
complexOf_int signum_fun_complexOf_int(complexOf_int);
complexOf_long signum_fun_complexOf_long(complexOf_long);
complexOf_llong signum_fun_complexOf_llong(complexOf_llong);
complexOf_uchar signum_fun_complexOf_uchar(complexOf_uchar);
complexOf_uint signum_fun_complexOf_uint(complexOf_uint);
complexOf_ulong signum_fun_complexOf_ulong(complexOf_ulong);
complexOf_ullong signum_fun_complexOf_ullong(complexOf_ullong);
complexOf_float signum_fun_complexOf_float(complexOf_float);
unsigned signum_fun_complexOf_short(unsigned);
unsigned signum_fun_complexOf_ushort(unsigned);

complexOf_char add_fun_complexOf_char(complexOf_char, complexOf_char);
complexOf_int add_fun_complexOf_int(complexOf_int, complexOf_int);
complexOf_long add_fun_complexOf_long(complexOf_long, complexOf_long);
complexOf_llong add_fun_complexOf_llong(complexOf_llong, complexOf_llong);
complexOf_uchar add_fun_complexOf_uchar(complexOf_uchar, complexOf_uchar);
complexOf_uint add_fun_complexOf_uint(complexOf_uint, complexOf_uint);
complexOf_ulong add_fun_complexOf_ulong(complexOf_ulong, complexOf_ulong);
complexOf_ullong add_fun_complexOf_ullong(complexOf_ullong, complexOf_ullong);
complexOf_float add_fun_complexOf_float(complexOf_float, complexOf_float);

complexOf_char sub_fun_complexOf_char(complexOf_char, complexOf_char);
complexOf_int sub_fun_complexOf_int(complexOf_int, complexOf_int);
complexOf_long sub_fun_complexOf_long(complexOf_long, complexOf_long);
complexOf_llong sub_fun_complexOf_llong(complexOf_llong, complexOf_llong);
complexOf_uchar sub_fun_complexOf_uchar(complexOf_uchar, complexOf_uchar);
complexOf_uint sub_fun_complexOf_uint(complexOf_uint, complexOf_uint);
complexOf_ulong sub_fun_complexOf_ulong(complexOf_ulong, complexOf_ulong);
complexOf_ullong sub_fun_complexOf_ullong(complexOf_ullong, complexOf_ullong);
complexOf_float sub_fun_complexOf_float(complexOf_float, complexOf_float);

complexOf_char mult_fun_complexOf_char(complexOf_char, complexOf_char);
complexOf_int mult_fun_complexOf_int(complexOf_int, complexOf_int);
complexOf_long mult_fun_complexOf_long(complexOf_long, complexOf_long);
complexOf_llong mult_fun_complexOf_llong(complexOf_llong, complexOf_llong);
complexOf_uchar mult_fun_complexOf_uchar(complexOf_uchar, complexOf_uchar);
complexOf_uint mult_fun_complexOf_uint(complexOf_uint, complexOf_uint);
complexOf_ulong mult_fun_complexOf_ulong(complexOf_ulong, complexOf_ulong);
complexOf_ullong mult_fun_complexOf_ullong(complexOf_ullong, complexOf_ullong);
complexOf_float mult_fun_complexOf_float(complexOf_float, complexOf_float);
unsigned mult_fun_complexOf_short(unsigned, unsigned);
unsigned mult_fun_complexOf_ushort(unsigned, unsigned);

complexOf_float div_fun_complexOf_float(complexOf_float, complexOf_float);

complexOf_float exp_fun_complexOf_float(complexOf_float);

complexOf_float sqrt_fun_complexOf_float(complexOf_float);

complexOf_float log_fun_complexOf_float(complexOf_float);

complexOf_float pow_fun_complexOf_float(complexOf_float, complexOf_float);

complexOf_float logBase_fun_complexOf_float(complexOf_float, complexOf_float);

complexOf_float sin_fun_complexOf_float(complexOf_float);

complexOf_float cos_fun_complexOf_float(complexOf_float);

complexOf_float tan_fun_complexOf_float(complexOf_float);

complexOf_float sinh_fun_complexOf_float(complexOf_float);

complexOf_float cosh_fun_complexOf_float(complexOf_float);

complexOf_float tanh_fun_complexOf_float(complexOf_float);

complexOf_float asin_fun_complexOf_float(complexOf_float);

complexOf_float acos_fun_complexOf_float(complexOf_float);

complexOf_float atan_fun_complexOf_float(complexOf_float);

complexOf_float asinh_fun_complexOf_float(complexOf_float);

complexOf_float acosh_fun_complexOf_float(complexOf_float);

complexOf_float atanh_fun_complexOf_float(complexOf_float);

complexOf_char complex_fun_char(char, char);
complexOf_int complex_fun_int(int, int);
complexOf_long complex_fun_long(long, long);
complexOf_llong complex_fun_llong(long long, long long);
complexOf_uchar complex_fun_uchar(unsigned char, unsigned char);
complexOf_uint complex_fun_uint(unsigned, unsigned);
complexOf_ulong complex_fun_ulong(unsigned long, unsigned long);
complexOf_ullong complex_fun_ullong(unsigned long long, unsigned long long);
complexOf_float complex_fun_float(float, float);

char creal_fun_complexOf_char(complexOf_char);
int creal_fun_complexOf_int(complexOf_int);
long creal_fun_complexOf_long(complexOf_long);
long long creal_fun_complexOf_llong(complexOf_llong);
unsigned char creal_fun_complexOf_uchar(complexOf_uchar);
unsigned creal_fun_complexOf_uint(complexOf_uint);
unsigned long creal_fun_complexOf_ulong(complexOf_ulong);
unsigned long long creal_fun_complexOf_ullong(complexOf_ullong);
float creal_fun_complexOf_float(complexOf_float);
short creal_fun_complexOf_short(unsigned);
unsigned short creal_fun_complexOf_ushort(unsigned);

char cimag_fun_complexOf_char(complexOf_char);
int cimag_fun_complexOf_int(complexOf_int);
long cimag_fun_complexOf_long(complexOf_long);
long long cimag_fun_complexOf_llong(complexOf_llong);
unsigned char cimag_fun_complexOf_uchar(complexOf_uchar);
unsigned cimag_fun_complexOf_uint(complexOf_uint);
unsigned long cimag_fun_complexOf_ulong(complexOf_ulong);
unsigned long long cimag_fun_complexOf_ullong(complexOf_ullong);
float cimag_fun_complexOf_float(complexOf_float);
short cimag_fun_complexOf_short(unsigned);
unsigned short cimag_fun_complexOf_ushort(unsigned);

complexOf_char conj_fun_complexOf_char(complexOf_char);
complexOf_int conj_fun_complexOf_int(complexOf_int);
complexOf_long conj_fun_complexOf_long(complexOf_long);
complexOf_llong conj_fun_complexOf_llong(complexOf_llong);
complexOf_uchar conj_fun_complexOf_uchar(complexOf_uchar);
complexOf_uint conj_fun_complexOf_uint(complexOf_uint);
complexOf_ulong conj_fun_complexOf_ulong(complexOf_ulong);
complexOf_ullong conj_fun_complexOf_ullong(complexOf_ullong);
complexOf_float conj_fun_complexOf_float(complexOf_float);
unsigned conj_fun_complexOf_short(unsigned);
unsigned conj_fun_complexOf_ushort(unsigned);

char magnitude_fun_complexOf_char(complexOf_char);
int magnitude_fun_complexOf_int(complexOf_int);
long magnitude_fun_complexOf_long(complexOf_long);
long long magnitude_fun_complexOf_llong(complexOf_llong);
unsigned char magnitude_fun_complexOf_uchar(complexOf_uchar);
unsigned magnitude_fun_complexOf_uint(complexOf_uint);
unsigned long magnitude_fun_complexOf_ulong(complexOf_ulong);
unsigned long long magnitude_fun_complexOf_ullong(complexOf_ullong);
float magnitude_fun_complexOf_float(complexOf_float);
short magnitude_fun_complexOf_short(unsigned);
unsigned short magnitude_fun_complexOf_ushort(unsigned);

char phase_fun_complexOf_char(complexOf_char);
int phase_fun_complexOf_int(complexOf_int);
long phase_fun_complexOf_long(complexOf_long);
long long phase_fun_complexOf_llong(complexOf_llong);
unsigned char phase_fun_complexOf_uchar(complexOf_uchar);
unsigned phase_fun_complexOf_uint(complexOf_uint);
unsigned long phase_fun_complexOf_ulong(complexOf_ulong);
unsigned long long phase_fun_complexOf_ullong(complexOf_ullong);
float phase_fun_complexOf_float(complexOf_float);
short phase_fun_complexOf_short(unsigned);
unsigned short phase_fun_complexOf_ushort(unsigned);

complexOf_char mkPolar_fun_char(char, char);
complexOf_int mkPolar_fun_int(int, int);
complexOf_long mkPolar_fun_long(long, long);
complexOf_llong mkPolar_fun_llong(long long, long long);
complexOf_uchar mkPolar_fun_uchar(unsigned char, unsigned char);
complexOf_uint mkPolar_fun_uint(unsigned, unsigned);
complexOf_ulong mkPolar_fun_ulong(unsigned long, unsigned long);
complexOf_ullong mkPolar_fun_ullong(unsigned long long, unsigned long long);
complexOf_float mkPolar_fun_float(float, float);
unsigned mkPolar_fun_short(short, short);
unsigned mkPolar_fun_ushort(unsigned short, unsigned short);

complexOf_char cis_fun_char(char);
complexOf_int cis_fun_int(int);
complexOf_long cis_fun_long(long);
complexOf_llong cis_fun_llong(long long);
complexOf_uchar cis_fun_uchar(unsigned char);
complexOf_uint cis_fun_uint(unsigned);
complexOf_ulong cis_fun_ulong(unsigned long);
complexOf_ullong cis_fun_ullong(unsigned long long);
complexOf_float cis_fun_float(float);
unsigned cis_fun_short(short);
unsigned cis_fun_ushort(unsigned short);

#endif /* FELDSPAR_TI_C64X_H */
