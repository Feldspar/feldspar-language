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

#include "feldspar_tic64x.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

float hypotf(float x, float y) {
  return sqrtf(x * x + y * y);
}



/*--------------------------------------------------------------------------*
 *                 pow(), abs(), signum(), logBase()                        *
 *--------------------------------------------------------------------------*/

char pow_fun_char(char a, char b) {
    char r = 1;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %d `pow` %d", a, b);
        exit(1);
    }
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

short pow_fun_short(short a, short b) {
    short r = 1;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %d `pow` %d", a, b);
        exit(1);
    }
    for(int i = 0; i < b; ++i)
        r *= a;
    return r;
}

int pow_fun_int(int a, int b) {
    int r = 1;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %d `pow` %d", a, b);
        exit(1);
    }
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

long pow_fun_long(long a, long b) {
    long r = 1;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %ld `pow` %ld", a, b);
        exit(1);
    }
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

long long pow_fun_llong(long long a, long long b) {
    long long r = 1;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %lld `pow` %lld", a, b);
        exit(1);
    }
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

unsigned char pow_fun_uchar(unsigned char a, unsigned char b) {
    unsigned char r = 1;
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

unsigned short pow_fun_ushort(unsigned short a, unsigned short b) {
    unsigned short r = 1;
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

unsigned pow_fun_uint(unsigned a, unsigned b) {
    unsigned r = 1;
    for(int i = 0; i < b; ++i)
        r *= a;
    return r;
}

unsigned long pow_fun_ulong(unsigned long a, unsigned long b) {
    unsigned long r = 1;
    for(int i = 0; i < b; ++i)
        r *= a;
    return r;
}

unsigned long long pow_fun_ullong(unsigned long long a, unsigned long long b) {
    unsigned long long r = 1;
    for(int i = 0; i < b; ++i)
        r *= a;
    return r;
}

char abs_fun_char(char a) {
    // From Bit Twiddling Hacks:
    //    "Compute the integer absolute value (abs) without branching"
    char mask = a >> 7;
    return (a + mask) ^ mask;
}

short abs_fun_short(short a) {
    // From Bit Twiddling Hacks:
    //    "Compute the integer absolute value (abs) without branching"
    short mask = a >> 15;
    return (a + mask) ^ mask;
}

long abs_fun_long(long a) {
    // From Bit Twiddling Hacks:
    //    "Compute the integer absolute value (abs) without branching"
    long mask = a >> 39;
    return (a + mask) ^ mask;
}

long long abs_fun_llong(long long a) {
    // From Bit Twiddling Hacks:
    //    "Compute the integer absolute value (abs) without branching"
    long long mask = a >> 63;
    return (a + mask) ^ mask;
}

char signum_fun_char(char a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 7);
}

short signum_fun_short(short a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 15);
}

int signum_fun_int(int a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 31);
}

long signum_fun_long(long a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 39);
}

long long signum_fun_llong(long long a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 63);
}

unsigned char signum_fun_uchar(unsigned char a) {
    return a > 0;
}

unsigned short signum_fun_ushort(unsigned short a) {
    return a > 0;
}

unsigned signum_fun_uint(unsigned a) {
    return a > 0;
}

unsigned long signum_fun_ulong(unsigned long a) {
    return a > 0;
}

unsigned long long signum_fun_ullong(unsigned long long a) {
    return a > 0;
}

float signum_fun_float(float a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a > 0) - (a < 0);
}

float logBase_fun_float(float a, float b) {
    return logf(b) / logf(a);
}

/*--------------------------------------------------------------------------*
 *                 Bit operations                                           *
 *--------------------------------------------------------------------------*/

char setBit_fun_char(char x, unsigned i) {
    return x | 1 << i;
}

short setBit_fun_short(short x, unsigned i) {
    return x | 1 << i;
}

int setBit_fun_int(int x, unsigned i) {
    return x | 1 << i;
}

long setBit_fun_long(long x, unsigned i) {
    return x | 1 << i;
}

long long setBit_fun_llong(long long x, unsigned i) {
    return x | 1 << i;
}

unsigned char setBit_fun_uchar(unsigned char x, unsigned i) {
    return x | 1 << i;
}

unsigned short setBit_fun_ushort(unsigned short x, unsigned i) {
    return x | 1 << i;
}

unsigned setBit_fun_uint(unsigned x, unsigned i) {
    return x | 1 << i;
}

unsigned long setBit_fun_ulong(unsigned long x, unsigned i) {
    return x | 1 << i;
}

unsigned long long setBit_fun_ullong(unsigned long long x, unsigned i) {
    return x | 1 << i;
}

char clearBit_fun_char(char x, unsigned i) {
    return x & ~(1 << i);
}

short clearBit_fun_short(short x, unsigned i) {
    return x & ~(1 << i);
}

int clearBit_fun_int(int x, unsigned i) {
    return x & ~(1 << i);
}

long clearBit_fun_long(long x, unsigned i) {
    return x & ~(1 << i);
}

long long clearBit_fun_llong(long long x, unsigned i) {
    return x & ~(1 << i);
}

unsigned char clearBit_fun_uchar(unsigned char x, unsigned i) {
    return x & ~(1 << i);
}

unsigned short clearBit_fun_ushort(unsigned short x, unsigned i) {
    return x & ~(1 << i);
}

unsigned clearBit_fun_uint(unsigned x, unsigned i) {
    return x & ~(1 << i);
}

unsigned long clearBit_fun_ulong(unsigned long x, unsigned i) {
    return x & ~(1 << i);
}

unsigned long long clearBit_fun_ullong(unsigned long long x, unsigned i) {
    return x & ~(1 << i);
}

char complementBit_fun_char(char x, unsigned i) {
    return x ^ 1 << i;
}

short complementBit_fun_short(short x, unsigned i) {
    return x ^ 1 << i;
}

int complementBit_fun_int(int x, unsigned i) {
    return x ^ 1 << i;
}

long complementBit_fun_long(long x, unsigned i) {
    return x ^ 1 << i;
}

long long complementBit_fun_llong(long long x, unsigned i) {
    return x ^ 1 << i;
}

unsigned char complementBit_fun_uchar(unsigned char x, unsigned i) {
    return x ^ 1 << i;
}

unsigned short complementBit_fun_ushort(unsigned short x, unsigned i) {
    return x ^ 1 << i;
}

unsigned complementBit_fun_uint(unsigned x, unsigned i) {
    return x ^ 1 << i;
}

unsigned long complementBit_fun_ulong(unsigned long x, unsigned i) {
    return x ^ 1 << i;
}

unsigned long long complementBit_fun_ullong(unsigned long long x, unsigned i) {
    return x ^ 1 << i;
}

int testBit_fun_char(char x, unsigned i) {
    return (x & 1 << i) != 0;
}

int testBit_fun_short(short x, unsigned i) {
    return (x & 1 << i) != 0;
}

int testBit_fun_int(int x, unsigned i) {
    return (x & 1 << i) != 0;
}

int testBit_fun_long(long x, unsigned i) {
    return (x & 1 << i) != 0;
}

int testBit_fun_llong(long long x, unsigned i) {
    return (x & 1 << i) != 0;
}

int testBit_fun_uchar(unsigned char x, unsigned i) {
    return (x & 1 << i) != 0;
}

int testBit_fun_ushort(unsigned short x, unsigned i) {
    return (x & 1 << i) != 0;
}

int testBit_fun_uint(unsigned x, unsigned i) {
    return (x & 1 << i) != 0;
}

int testBit_fun_ulong(unsigned long x, unsigned i) {
    return (x & 1 << i) != 0;
}

int testBit_fun_ullong(unsigned long long x, unsigned i) {
    return (x & 1 << i) != 0;
}

char rotateL_fun_char(char x, int i) {
    if ((i %= 8) == 0) return x;
    return (x << i) | ((0x7f >> (7 - i)) & (x >> (8 - i)));
}

short rotateL_fun_short(short x, int i) {
    if ((i %= 16) == 0) return x;
    return (x << i) | ((0x7fff >> (15 - i)) & (x >> (16 - i)));
}

long rotateL_fun_long(long x, int i) {
    if ((i %= 40) == 0) return x;
    return (x << i) | ((0x7fffffffffl >> (39 - i)) & (x >> (40 - i)));
}

long long rotateL_fun_llong(long long x, int i) {
    if ((i %= 64) == 0) return x;
    return (x << i) | ((0x7fffffffffffffffll >> (63 - i)) & (x >> (64 - i)));
}

int rotateL_fun_int(int x, int i) {
    return (int)_rotl((unsigned)x, (unsigned)i);
}

unsigned char rotateL_fun_uchar(unsigned char x, int i) {
    if ((i %= 8) == 0) return x;
    return (x << i) | (x >> (8 - i));
}

unsigned short rotateL_fun_ushort(unsigned short x, int i) {
    if ((i %= 16) == 0) return x;
    return (x << i) | (x >> (16 - i));
}

unsigned long rotateL_fun_ulong(unsigned long x, int i) {
    if ((i %= 40) == 0) return x;
    return (x << i) | (x >> (40 - i));
}

unsigned long long rotateL_fun_ullong(unsigned long long x, int i) {
    if ((i %= 64) == 0) return x;
    return (x << i) | (x >> (64 - i));
}

char rotateR_fun_char(char x, int i) {
    if ((i %= 8) == 0) return x;
    return (x << (8 - i)) | ((0x7f >> (i - 1)) & (x >> i));
}

short rotateR_fun_short(short x, int i) {
    if ((i %= 16) == 0) return x;
    return (x << (16 - i)) | ((0x7fff >> (i - 1)) & (x >> i));
}

int rotateR_fun_int(int x, int i) {
    if ((i %= 32) == 0) return x;
    return (x << (32 - i)) | ((0x7fffffff >> (i - 1)) & (x >> i));
}

long rotateR_fun_long(long x, int i) {
    if ((i %= 40) == 0) return x;
    return (x << (40 - i)) | ((0x7fffffffffl >> (i - 1)) & (x >> i));
}

long long rotateR_fun_llong(long long x, int i) {
    if ((i %= 64) == 0) return x;
    return (x << (64 - i)) | ((0x7fffffffffffffffll >> (i - 1)) & (x >> i));
}

unsigned char rotateR_fun_uchar(unsigned char x, int i) {
    if ((i %= 8) == 0) return x;
    return (x << (8 - i)) | (x >> i);
}

unsigned short rotateR_fun_ushort(unsigned short x, int i) {
    if ((i %= 16) == 0) return x;
    return (x << (16 - i)) | (x >> i);
}

unsigned rotateR_fun_uint(unsigned x, int i) {
    if ((i %= 32) == 0) return x;
    return (x << (32 - i)) | (x >> i);
}

unsigned long rotateR_fun_ulong(unsigned long x, int i) {
    if ((i %= 40) == 0) return x;
    return (x << (40 - i)) | (x >> i);
}

unsigned long long rotateR_fun_ullong(unsigned long long x, int i) {
    if ((i %= 64) == 0) return x;
    return (x << (64 - i)) | (x >> i);
}

char reverseBits_fun_char(char x) {
    char r = x;
    int i = 7;
    for (x = x >> 1 & 0x7f; x; x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

short reverseBits_fun_short(short x) {
    short r = x;
    int i = 15;
    for (x = x >> 1 & 0x7fff; x; x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

int reverseBits_fun_int(int x) {
    int r = x;
    int i = 31;
    for (x = x >> 1 & 0x7fffffff; x; x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

long reverseBits_fun_long(long x) {
    long r = x;
    int i = 39;
    for (x = x >> 1 & 0x7fffffffffl; x; x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

long long reverseBits_fun_llong(long long x) {
    long long r = x;
    int i = 63;
    for (x = x >> 1 & 0x7fffffffffffffffll; x; x >>= 1)
    {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

unsigned char reverseBits_fun_uchar(unsigned char x) {
    unsigned char r = x;
    int i = 7;
    while (x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

unsigned short reverseBits_fun_ushort(unsigned short x) {
    unsigned short r = x;
    int i = 15;
    while (x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

unsigned long reverseBits_fun_ulong(unsigned long x) {
    unsigned long r = x;
    int i = 39;
    while (x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

unsigned long long reverseBits_fun_ullong(unsigned long long x) {
    unsigned long long r = x;
    int i = 63;
    while (x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

unsigned bitScan_fun_char(char x) {
    unsigned r = 0;
    char s = (x & 0x80);
    if (x == 0) return 7;
    while (((x <<= 1) & 0x80) == s)
        ++r;
    return r;
}

unsigned bitScan_fun_short(short x) {
    unsigned r = 0;
    short s = (x & 0x8000);
    if (x == 0) return 15;
    while (((x <<= 1) & 0x8000) == s)
        ++r;
    return r;
}

unsigned bitScan_fun_int(int x) {
    unsigned r = 0;
    int s = (x & 0x80000000);
    if (x == 0) return 31;
    while (((x <<= 1) & 0x80000000) == s)
        ++r;
    return r;
}

unsigned bitScan_fun_long(long x) {
    unsigned r = 0;
    long s = (x & 0x8000000000l);
    if (x == 0) return 39;
    while (((x <<= 1) & 0x8000000000l) == s)
        ++r;
    return r;
}

unsigned bitScan_fun_llong(long long x) {
    unsigned r = 0;
    long long s = (x & 0x8000000000000000ll);
    if (x == 0) return 63;
    while (((x <<= 1) & 0x8000000000000000ll) == s)
        ++r;
    return r;
}

unsigned bitScan_fun_uchar(unsigned char x) {
    unsigned r = 8;
    while (x) {
        --r;
        x >>= 1;
    }
    return r;
}

unsigned bitScan_fun_ushort(unsigned short x) {
    unsigned r = 16;
    while (x) {
        --r;
        x >>= 1;
    }
    return r;
}

unsigned bitScan_fun_uint(unsigned x) {
    unsigned r = 32;
    while (x) {
        --r;
        x >>= 1;
    }
    return r;
}

unsigned bitScan_fun_ulong(unsigned long x) {
    unsigned r = 40;
    while (x) {
        --r;
        x >>= 1;
    }
    return r;
}

unsigned bitScan_fun_ullong(unsigned long long x) {
    unsigned r = 64;
    while (x) {
        --r;
        x >>= 1;
    }
    return r;
}

unsigned bitCount_fun_char(char x) {
    unsigned r = x & 1;
    for (x = x >> 1 & 0x7f; x; x >>= 1)
        r += x & 1;
    return r;
}

unsigned bitCount_fun_short(short x) {
    unsigned r = x & 1;
    for (x = x >> 1 & 0x7fff; x; x >>= 1)
        r += x & 1;
    return r;
}

unsigned bitCount_fun_int(int x) {
    unsigned r = x & 1;
    for (x = x >> 1 & 0x7fffffff; x; x >>= 1)
        r += x & 1;
    return r;
}

unsigned bitCount_fun_long(long x) {
    unsigned r = x & 1;
    for (x = x >> 1 & 0x7fffffffffl; x; x >>= 1)
        r += x & 1;
    return r;
}

unsigned bitCount_fun_llong(long long x) {
    unsigned r = x & 1;
    for (x = x >> 1 & 0x7fffffffffffffffll; x; x >>= 1)
        r += x & 1;
    return r;
}

unsigned bitCount_fun_uchar(unsigned char x) {
    unsigned r = x & 1;
    while (x >>= 1)
        r += x & 1;
    return r;
}

unsigned bitCount_fun_ushort(unsigned short x) {
    unsigned r = x & 1;
    while (x >>= 1)
        r += x & 1;
    return r;
}

unsigned bitCount_fun_ulong(unsigned long x) {
    unsigned r = x & 1;
    while (x >>= 1)
        r += x & 1;
    return r;
}

unsigned bitCount_fun_ullong(unsigned long long x) {
    unsigned r = x & 1;
    while (x >>= 1)
        r += x & 1;
    return r;
}

/*--------------------------------------------------------------------------*
 *                 Complex numbers                                          *
 *--------------------------------------------------------------------------*/

int equal_fun_complexOf_char(complexOf_char a, complexOf_char b) {
    return a.re == b.re && a.im == b.im;
}

int equal_fun_complexOf_int(complexOf_int a, complexOf_int b) {
    return a.re == b.re && a.im == b.im;
}

int equal_fun_complexOf_long(complexOf_long a, complexOf_long b) {
    return a.re == b.re && a.im == b.im;
}

int equal_fun_complexOf_llong(complexOf_llong a, complexOf_llong b) {
    return a.re == b.re && a.im == b.im;
}

int equal_fun_complexOf_uchar(complexOf_uchar a, complexOf_uchar b) {
    return a.re == b.re && a.im == b.im;
}

int equal_fun_complexOf_uint(complexOf_uint a, complexOf_uint b) {
    return a.re == b.re && a.im == b.im;
}

int equal_fun_complexOf_ulong(complexOf_ulong a, complexOf_ulong b) {
    return a.re == b.re && a.im == b.im;
}

int equal_fun_complexOf_ullong(complexOf_ullong a, complexOf_ullong b) {
    return a.re == b.re && a.im == b.im;
}

int equal_fun_complexOf_float(complexOf_float a, complexOf_float b) {
    return a.re == b.re && a.im == b.im;
}

complexOf_char negate_fun_complexOf_char(complexOf_char a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

complexOf_int negate_fun_complexOf_int(complexOf_int a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

complexOf_long negate_fun_complexOf_long(complexOf_long a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}
 
complexOf_llong negate_fun_complexOf_llong(complexOf_llong a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

complexOf_uchar negate_fun_complexOf_uchar(complexOf_uchar a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

complexOf_uint negate_fun_complexOf_uint(complexOf_uint a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

complexOf_ulong negate_fun_complexOf_ulong(complexOf_ulong a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

complexOf_ullong negate_fun_complexOf_ullong(complexOf_ullong a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

complexOf_float negate_fun_complexOf_float(complexOf_float a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

complexOf_char abs_fun_complexOf_char(complexOf_char a) {
    a.re = magnitude_fun_complexOf_char(a);
    a.im = 0;
    return a;
}

complexOf_int abs_fun_complexOf_int(complexOf_int a) {
    a.re = magnitude_fun_complexOf_int(a);
    a.im = 0;
    return a;
}

complexOf_long abs_fun_complexOf_long(complexOf_long a) {
    a.re = magnitude_fun_complexOf_long(a);
    a.im = 0;
    return a;
}

complexOf_llong abs_fun_complexOf_llong(complexOf_llong a) {
    a.re = magnitude_fun_complexOf_llong(a);
    a.im = 0;
    return a;
}

complexOf_uchar abs_fun_complexOf_uchar(complexOf_uchar a) {
    a.re = magnitude_fun_complexOf_uchar(a);
    a.im = 0;
    return a;
}

complexOf_uint abs_fun_complexOf_uint(complexOf_uint a) {
    a.re = magnitude_fun_complexOf_uint(a);
    a.im = 0;
    return a;
}

complexOf_ulong abs_fun_complexOf_ulong(complexOf_ulong a) {
    a.re = magnitude_fun_complexOf_ulong(a);
    a.im = 0;
    return a;
}

complexOf_ullong abs_fun_complexOf_ullong(complexOf_ullong a) {
    a.re = magnitude_fun_complexOf_ullong(a);
    a.im = 0;
    return a;
}

complexOf_float abs_fun_complexOf_float(complexOf_float a) {
    a.re = magnitude_fun_complexOf_float(a);
    a.im = 0;
    return a;
}

unsigned abs_fun_complexOf_short(unsigned a) {
    return _pack2(magnitude_fun_complexOf_short(a), 0);
}

unsigned abs_fun_complexOf_ushort(unsigned a) {
    return _pack2(magnitude_fun_complexOf_ushort(a), 0);
}

complexOf_char signum_fun_complexOf_char(complexOf_char a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        char m = magnitude_fun_complexOf_char(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

complexOf_int signum_fun_complexOf_int(complexOf_int a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        int m = magnitude_fun_complexOf_int(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

complexOf_long signum_fun_complexOf_long(complexOf_long a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        long m = magnitude_fun_complexOf_long(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

complexOf_llong signum_fun_complexOf_llong(complexOf_llong a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        long long m = magnitude_fun_complexOf_llong(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

complexOf_uchar signum_fun_complexOf_uchar(complexOf_uchar a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        unsigned char m = magnitude_fun_complexOf_uchar(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

complexOf_uint signum_fun_complexOf_uint(complexOf_uint a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        unsigned m = magnitude_fun_complexOf_uint(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

complexOf_ulong signum_fun_complexOf_ulong(complexOf_ulong a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        unsigned long m = magnitude_fun_complexOf_ulong(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

complexOf_ullong signum_fun_complexOf_ullong(complexOf_ullong a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        unsigned long long m = magnitude_fun_complexOf_ullong(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

complexOf_float signum_fun_complexOf_float(complexOf_float a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        float m = magnitude_fun_complexOf_float(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

unsigned signum_fun_complexOf_short(unsigned a) {
    if (a == 0)
        return a;
    else {
        short m = magnitude_fun_complexOf_short(a);
        return _pack2((a >> 16) / m, (a & 0x0000ffff) / m);
    }
}

unsigned signum_fun_complexOf_ushort(unsigned a) {
    if (a == 0)
        return a;
    else {
        unsigned short m = magnitude_fun_complexOf_ushort(a);
        return _pack2((a >> 16) / m, (a & 0x0000ffff) / m);
    }
}

complexOf_char add_fun_complexOf_char(complexOf_char a, complexOf_char b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

complexOf_int add_fun_complexOf_int(complexOf_int a, complexOf_int b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

complexOf_long add_fun_complexOf_long(complexOf_long a, complexOf_long b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

complexOf_llong add_fun_complexOf_llong(complexOf_llong a, complexOf_llong b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

complexOf_uchar add_fun_complexOf_uchar(complexOf_uchar a, complexOf_uchar b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

complexOf_uint add_fun_complexOf_uint(complexOf_uint a, complexOf_uint b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

complexOf_ulong add_fun_complexOf_ulong(complexOf_ulong a, complexOf_ulong b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

complexOf_ullong add_fun_complexOf_ullong(complexOf_ullong a, complexOf_ullong b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

complexOf_float add_fun_complexOf_float(complexOf_float a, complexOf_float b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

complexOf_char sub_fun_complexOf_char(complexOf_char a, complexOf_char b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

complexOf_int sub_fun_complexOf_int(complexOf_int a, complexOf_int b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

complexOf_long sub_fun_complexOf_long(complexOf_long a, complexOf_long b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

complexOf_llong sub_fun_complexOf_llong(complexOf_llong a, complexOf_llong b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

complexOf_uchar sub_fun_complexOf_uchar(complexOf_uchar a, complexOf_uchar b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

complexOf_uint sub_fun_complexOf_uint(complexOf_uint a, complexOf_uint b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

complexOf_ulong sub_fun_complexOf_ulong(complexOf_ulong a, complexOf_ulong b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

complexOf_ullong sub_fun_complexOf_ullong(complexOf_ullong a, complexOf_ullong b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

complexOf_float sub_fun_complexOf_float(complexOf_float a, complexOf_float b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

complexOf_char mult_fun_complexOf_char(complexOf_char a, complexOf_char b) {
    complexOf_char r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

complexOf_int mult_fun_complexOf_int(complexOf_int a, complexOf_int b) {
    complexOf_int r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

complexOf_long mult_fun_complexOf_long(complexOf_long a, complexOf_long b) {
    complexOf_long r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

complexOf_llong mult_fun_complexOf_llong(complexOf_llong a, complexOf_llong b) {
    complexOf_llong r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

complexOf_uchar mult_fun_complexOf_uchar(complexOf_uchar a, complexOf_uchar b) {
    complexOf_uchar r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

complexOf_uint mult_fun_complexOf_uint(complexOf_uint a, complexOf_uint b) {
    complexOf_uint r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

complexOf_ulong mult_fun_complexOf_ulong(complexOf_ulong a, complexOf_ulong b) {
    complexOf_ulong r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

complexOf_ullong mult_fun_complexOf_ullong(complexOf_ullong a, complexOf_ullong b) {
    complexOf_ullong r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

complexOf_float mult_fun_complexOf_float(complexOf_float a, complexOf_float b) {
    complexOf_float r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

unsigned mult_fun_complexOf_short(unsigned a, unsigned b) {
    short are = (short)(a >> 16);
    short aim = (short)(a & 0x0000ffff);
    short bre = (short)(b >> 16);
    short bim = (short)(b & 0x0000ffff);
    return _pack2(are * bre - aim * bim, aim * bre + are * bim);
}

unsigned mult_fun_complexOf_ushort(unsigned a, unsigned b) {
    unsigned short are = (unsigned short)(a >> 16);
    unsigned short aim = (unsigned short)(a & 0x0000ffff);
    unsigned short bre = (unsigned short)(b >> 16);
    unsigned short bim = (unsigned short)(b & 0x0000ffff);
    return _pack2(are * bre - aim * bim, aim * bre + are * bim);
}

complexOf_float div_fun_complexOf_float(complexOf_float a, complexOf_float b) {
    float x = b.re * b.re + b.im * b.im;
    complexOf_float r;
    r.re = (a.re * b.re + a.im * b.im) / x;
    r.im = (a.im * b.re - a.re * b.im) / x;
    return r;
}

complexOf_float exp_fun_complexOf_float(complexOf_float a) {
    float expre = expf(a.re);
    a.re = expre * cosf(a.im);
    a.im = expre * sinf(a.im);
    return a;
}

complexOf_float sqrt_fun_complexOf_float(complexOf_float a) {
    float magare = magnitude_fun_complexOf_float(a) + a.re;
    a.re = sqrtf(magare / 2);
    a.im = a.im / sqrtf(2 * magare);
    return a;
}

complexOf_float log_fun_complexOf_float(complexOf_float a) {
    complexOf_float r;
    r.re = logf(magnitude_fun_complexOf_float(a));
    r.im = phase_fun_complexOf_float(a);
    return r;
}

complexOf_float pow_fun_complexOf_float(complexOf_float a, complexOf_float b) {
    return exp_fun_complexOf_float(mult_fun_complexOf_float(log_fun_complexOf_float(a), b));
}

complexOf_float logBase_fun_complexOf_float(complexOf_float a, complexOf_float b) {
    return div_fun_complexOf_float(log_fun_complexOf_float(b), log_fun_complexOf_float(a));
}

complexOf_float sin_fun_complexOf_float(complexOf_float a) {
    complexOf_float r;
    r.re = sinf(a.re) * coshf(a.im);
    r.im = cosf(a.re) * sinhf(a.im);
    return  r;
}

complexOf_float cos_fun_complexOf_float(complexOf_float a) {
    complexOf_float r;
    r.re = cosf(a.re) * coshf(a.im);
    r.im = - sinf(a.re) * sinhf(a.im);
    return  r;
}

complexOf_float tan_fun_complexOf_float(complexOf_float a) {
    return div_fun_complexOf_float(sin_fun_complexOf_float(a), cos_fun_complexOf_float(a));
}

complexOf_float sinh_fun_complexOf_float(complexOf_float a) {
    complexOf_float r;
    r.re = sinhf(a.re) * cosf(a.im);
    r.im = coshf(a.re) * sinf(a.im);
    return  r;
}

complexOf_float cosh_fun_complexOf_float(complexOf_float a) {
    complexOf_float r;
    r.re = cosf(a.re) * coshf(a.im);
    r.im = sinhf(a.re) * sinf(a.im);
    return  r;
}

complexOf_float tanh_fun_complexOf_float(complexOf_float a) {
    return div_fun_complexOf_float(sinh_fun_complexOf_float(a), cosh_fun_complexOf_float(a));
}

complexOf_float asin_fun_complexOf_float(complexOf_float a) {
    complexOf_float b = add_fun_complexOf_float(log_fun_complexOf_float(complex_fun_float(-a.im, a.re)), sqrt_fun_complexOf_float(sub_fun_complexOf_float(complex_fun_float(1, 0), mult_fun_complexOf_float(a, a))));
    a.re = b.im;
    a.im = -b.re;
    return a;
}

complexOf_float acos_fun_complexOf_float(complexOf_float a) {
    complexOf_float b = sqrt_fun_complexOf_float(sub_fun_complexOf_float(complex_fun_float(1, 0), mult_fun_complexOf_float(a, a)));
    complexOf_float c = log_fun_complexOf_float(add_fun_complexOf_float(a, complex_fun_float(-b.im, b.re)));
    a.re = c.im;
    a.im = -c.re;
    return a;
}

complexOf_float atan_fun_complexOf_float(complexOf_float a) {
    complexOf_float b = log_fun_complexOf_float(div_fun_complexOf_float(complex_fun_float(1 - a.im, a.re), sqrt_fun_complexOf_float(add_fun_complexOf_float(complex_fun_float(1, 0), mult_fun_complexOf_float(a, a)))));
    a.re = b.im;
    a.im = -b.re;
    return a;
}

complexOf_float asinh_fun_complexOf_float(complexOf_float a) {
    return log_fun_complexOf_float(add_fun_complexOf_float(a, sqrt_fun_complexOf_float(add_fun_complexOf_float(complex_fun_float(1, 0), mult_fun_complexOf_float(a, a)))));
}

complexOf_float acosh_fun_complexOf_float(complexOf_float a) {
    complexOf_float b = {1, 0};
    complexOf_float c = add_fun_complexOf_float(a, b);
    return log_fun_complexOf_float(add_fun_complexOf_float(a, mult_fun_complexOf_float(c, sqrt_fun_complexOf_float(div_fun_complexOf_float(sub_fun_complexOf_float(a, b), c)))));
}

complexOf_float atanh_fun_complexOf_float(complexOf_float a) {
    complexOf_float b = {1, 0};
    return log_fun_complexOf_float(div_fun_complexOf_float(add_fun_complexOf_float(b, a), sqrt_fun_complexOf_float(sub_fun_complexOf_float(b, mult_fun_complexOf_float(a, a)))));
}

complexOf_char complex_fun_char(char re, char im) {
    complexOf_char r;
    r.re = re;
    r.im = im;
    return r;
}

complexOf_int complex_fun_int(int re, int im) {
    complexOf_int r;
    r.re = re;
    r.im = im;
    return r;
}

complexOf_long complex_fun_long(long re, long im) {
    complexOf_long r;
    r.re = re;
    r.im = im;
    return r;
}

complexOf_llong complex_fun_llong(long long re, long long im) {
    complexOf_llong r;
    r.re = re;
    r.im = im;
    return r;
}

complexOf_uchar complex_fun_uchar(unsigned char re, unsigned char im) {
    complexOf_uchar r;
    r.re = re;
    r.im = im;
    return r;
}

complexOf_uint complex_fun_uint(unsigned re, unsigned im) {
    complexOf_uint r;
    r.re = re;
    r.im = im;
    return r;
}

complexOf_ulong complex_fun_ulong(unsigned long re, unsigned long im) {
    complexOf_ulong r;
    r.re = re;
    r.im = im;
    return r;
}

complexOf_ullong complex_fun_ullong(unsigned long long re, unsigned long long im) {
    complexOf_ullong r;
    r.re = re;
    r.im = im;
    return r;
}

complexOf_float complex_fun_float(float re, float im) {
    complexOf_float r;
    r.re = re;
    r.im = im;
    return r;
}

char creal_fun_complexOf_char(complexOf_char a) {
    return a.re;
}

int creal_fun_complexOf_int(complexOf_int a) {
    return a.re;
}

long creal_fun_complexOf_long(complexOf_long a) {
    return a.re;
}

long long creal_fun_complexOf_llong(complexOf_llong a) {
    return a.re;
}

unsigned char creal_fun_complexOf_uchar(complexOf_uchar a) {
    return a.re;
}

unsigned creal_fun_complexOf_uint(complexOf_uint a) {
    return a.re;
}

unsigned long creal_fun_complexOf_ulong(complexOf_ulong a) {
    return a.re;
}

unsigned long long creal_fun_complexOf_ullong(complexOf_ullong a) {
    return a.re;
}

float creal_fun_complexOf_float(complexOf_float a) {
    return a.re;
}

short creal_fun_complexOf_short(unsigned a) {
    return a >> 16;
}

unsigned short creal_fun_complexOf_ushort(unsigned a) {
    return a >> 16;
}

char cimag_fun_complexOf_char(complexOf_char a) {
    return a.im;
}

int cimag_fun_complexOf_int(complexOf_int a) {
    return a.im;
}

long cimag_fun_complexOf_long(complexOf_long a) {
    return a.im;
}

long long cimag_fun_complexOf_llong(complexOf_llong a) {
    return a.im;
}

unsigned char cimag_fun_complexOf_uchar(complexOf_uchar a) {
    return a.im;
}

unsigned cimag_fun_complexOf_uint(complexOf_uint a) {
    return a.im;
}

unsigned long cimag_fun_complexOf_ulong(complexOf_ulong a) {
    return a.im;
}

unsigned long long cimag_fun_complexOf_ullong(complexOf_ullong a) {
    return a.im;
}

float cimag_fun_complexOf_float(complexOf_float a) {
    return a.im;
}

short cimag_fun_complexOf_short(unsigned a) {
    return a & 0x0000ffff;
}

unsigned short cimag_fun_complexOf_ushort(unsigned a) {
    return a & 0x0000ffff;
}

complexOf_char conj_fun_complexOf_char(complexOf_char a) {
    a.im = -a.im;
    return a;
}

complexOf_int conj_fun_complexOf_int(complexOf_int a) {
    a.im = -a.im;
    return a;
}

complexOf_long conj_fun_complexOf_long(complexOf_long a) {
    a.im = -a.im;
    return a;
}

complexOf_llong conj_fun_complexOf_llong(complexOf_llong a) {
    a.im = -a.im;
    return a;
}

complexOf_uchar conj_fun_complexOf_uchar(complexOf_uchar a) {
    a.im = -a.im;
    return a;
}

complexOf_uint conj_fun_complexOf_uint(complexOf_uint a) {
    a.im = -a.im;
    return a;
}

complexOf_ulong conj_fun_complexOf_ulong(complexOf_ulong a) {
    a.im = -a.im;
    return a;
}

complexOf_ullong conj_fun_complexOf_ullong(complexOf_ullong a) {
    a.im = -a.im;
    return a;
}

complexOf_float conj_fun_complexOf_float(complexOf_float a) {
    a.im = -a.im;
    return a;
}

unsigned conj_fun_complexOf_short(unsigned a) {
    return _pack2(a >> 16, -(a & 0x0000ffff));
}

unsigned conj_fun_complexOf_ushort(unsigned a) {
    return _pack2(a >> 16, -(a & 0x0000ffff));
}

char magnitude_fun_complexOf_char(complexOf_char a) {
    return roundf(hypotf(a.re, a.im));
}

int magnitude_fun_complexOf_int(complexOf_int a) {
    return roundf(hypotf(a.re, a.im));
}

long magnitude_fun_complexOf_long(complexOf_long a) {
    return roundf(hypotf(a.re, a.im));
}

long long magnitude_fun_complexOf_llong(complexOf_llong a) {
    return roundf(hypotf(a.re, a.im));
}

unsigned char magnitude_fun_complexOf_uchar(complexOf_uchar a) {
    return roundf(hypotf(a.re, a.im));
}

unsigned magnitude_fun_complexOf_uint(complexOf_uint a) {
    return roundf(hypotf(a.re, a.im));
}

unsigned long magnitude_fun_complexOf_ulong(complexOf_ulong a) {
    return roundf(hypotf(a.re, a.im));
}

unsigned long long magnitude_fun_complexOf_ullong(complexOf_ullong a) {
    return roundf(hypotf(a.re, a.im));
}

float magnitude_fun_complexOf_float(complexOf_float a) {
    return (hypotf(a.re, a.im));
}

short magnitude_fun_complexOf_short(unsigned a) {
    return roundf(hypotf((a >> 16), (a & 0x0000ffff)));
}

unsigned short magnitude_fun_complexOf_ushort(unsigned a) {
    return roundf(hypotf((a >> 16), (a & 0x0000ffff)));
}

char phase_fun_complexOf_char(complexOf_char a) {
    if (a.re == 0 && a.im == 0) return 0;
    return roundf(atan2f(a.im, a.re));
}

int phase_fun_complexOf_int(complexOf_int a) {
    if (a.re == 0 && a.im == 0) return 0;
    return roundf(atan2f(a.im, a.re));
}

long phase_fun_complexOf_long(complexOf_long a) {
    if (a.re == 0 && a.im == 0) return 0;
    return roundf(atan2f(a.im, a.re));
}

long long phase_fun_complexOf_llong(complexOf_llong a) {
    if (a.re == 0 && a.im == 0) return 0;
    return roundf(atan2f(a.im, a.re));
}

unsigned char phase_fun_complexOf_uchar(complexOf_uchar a) {
    if (a.re == 0 && a.im == 0) return 0;
    return roundf(atan2f(a.im, a.re));
}

unsigned phase_fun_complexOf_uint(complexOf_uint a) {
    if (a.re == 0 && a.im == 0) return 0;
    return roundf(atan2f(a.im, a.re));
}

unsigned long phase_fun_complexOf_ulong(complexOf_ulong a) {
    if (a.re == 0 && a.im == 0) return 0;
    return roundf(atan2f(a.im, a.re));
}

unsigned long long phase_fun_complexOf_ullong(complexOf_ullong a) {
    if (a.re == 0 && a.im == 0) return 0;
    return roundf(atan2f(a.im, a.re));
}

float phase_fun_complexOf_float(complexOf_float a) {
    if (a.re == 0 && a.im == 0) return 0;
    return (atan2f(a.im, a.re));
}

short phase_fun_complexOf_short(unsigned a) {
    short re = (a >> 16);
    short im = (a & 0x0000ffff);
    if (re == 0 && im == 0) return 0;
    return roundf(atan2f(im, re));
}

unsigned short phase_fun_complexOf_ushort(unsigned a) {
    unsigned short re = (a >> 16);
    unsigned short im = (a & 0x0000ffff);
    if (re == 0 && im == 0) return 0;
    return roundf(atan2f(im, re));
}

complexOf_char mkPolar_fun_char(char r, char t) {
    complexOf_char a;
    a.re = roundf(r * cosf(t));
    a.im = roundf(r * sinf(t));
    return a;
}

complexOf_int mkPolar_fun_int(int r, int t) {
    complexOf_int a;
    a.re = roundf(r * cosf(t));
    a.im = roundf(r * sinf(t));
    return a;
}

complexOf_long mkPolar_fun_long(long r, long t) {
    complexOf_long a;
    a.re = roundf(r * cosf(t));
    a.im = roundf(r * sinf(t));
    return a;
}

complexOf_llong mkPolar_fun_llong(long long r, long long t) {
    complexOf_llong a;
    a.re = roundf(r * cosf(t));
    a.im = roundf(r * sinf(t));
    return a;
}

complexOf_uchar mkPolar_fun_uchar(unsigned char r, unsigned char t) {
    complexOf_uchar a;
    a.re = roundf(r * cosf(t));
    a.im = roundf(r * sinf(t));
    return a;
}

complexOf_uint mkPolar_fun_uint(unsigned r, unsigned t) {
    complexOf_uint a;
    a.re = roundf(r * cosf(t));
    a.im = roundf(r * sinf(t));
    return a;
}

complexOf_ulong mkPolar_fun_ulong(unsigned long r, unsigned long t) {
    complexOf_ulong a;
    a.re = roundf(r * cosf(t));
    a.im = roundf(r * sinf(t));
    return a;
}

complexOf_ullong mkPolar_fun_ullong(unsigned long long r, unsigned long long t) {
    complexOf_ullong a;
    a.re = roundf(r * cosf(t));
    a.im = roundf(r * sinf(t));
    return a;
}

complexOf_float mkPolar_fun_float(float r, float t) {
    complexOf_float a;
    a.re = (r * cosf(t));
    a.im = (r * sinf(t));
    return a;
}

unsigned mkPolar_fun_short(short r, short t) {
    return _pack2(roundf(r * cosf(t)), roundf(r * sinf(t)));
}

unsigned mkPolar_fun_ushort(unsigned short r, unsigned short t) {
    return _pack2(roundf(r * cosf(t)), roundf(r * sinf(t)));
}

complexOf_char cis_fun_char(char t) {
    complexOf_char r;
    r.re = roundf(cosf(t));
    r.im = roundf(sinf(t));
    return r;
}

complexOf_int cis_fun_int(int t) {
    complexOf_int r;
    r.re = roundf(cosf(t));
    r.im = roundf(sinf(t));
    return r;
}

complexOf_long cis_fun_long(long t) {
    complexOf_long r;
    r.re = roundf(cosf(t));
    r.im = roundf(sinf(t));
    return r;
}

complexOf_llong cis_fun_llong(long long t) {
    complexOf_llong r;
    r.re = roundf(cosf(t));
    r.im = roundf(sinf(t));
    return r;
}

complexOf_uchar cis_fun_uchar(unsigned char t) {
    complexOf_uchar r;
    r.re = roundf(cosf(t));
    r.im = roundf(sinf(t));
    return r;
}

complexOf_uint cis_fun_uint(unsigned t) {
    complexOf_uint r;
    r.re = roundf(cosf(t));
    r.im = roundf(sinf(t));
    return r;
}

complexOf_ulong cis_fun_ulong(unsigned long t) {
    complexOf_ulong r;
    r.re = roundf(cosf(t));
    r.im = roundf(sinf(t));
    return r;
}

complexOf_ullong cis_fun_ullong(unsigned long long t) {
    complexOf_ullong r;
    r.re = roundf(cosf(t));
    r.im = roundf(sinf(t));
    return r;
}

complexOf_float cis_fun_float(float t) {
    complexOf_float r;
    r.re = (cosf(t));
    r.im = (sinf(t));
    return r;
}

unsigned cis_fun_short(short t) {
    return _pack2(roundf(cosf(t)), roundf(sinf(t)));
}

unsigned cis_fun_ushort(unsigned short t) {
    return _pack2(roundf(cosf(t)), roundf(sinf(t)));
}
