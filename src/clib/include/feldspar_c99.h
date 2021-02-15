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

#ifndef FELDSPAR_C99_H
#define FELDSPAR_C99_H

#include <complex.h>
#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(WIN32)
  #include <windows.h>
#else
  #include <sys/time.h>
  #include <time.h>
#endif /* WIN32 */

#ifdef USE_WOOL
#include "wool.h"
#endif
#ifdef __TIC64X__
#include "feldspar_tic64x.h"
#endif
#include "feldspar_array.h"
#include "feldspar_future.h"
#include "ivar.h"
#include "taskpool.h"

#if (defined(__i386) || defined(__x86_64)) && !defined(OpenCL)
#define global
#define local
#endif

#define min(X, Y)  ((X) < (Y) ? (X) : (Y))
#define max(X, Y)  ((X) > (Y) ? (X) : (Y))

static inline int8_t pow_fun_int8_t(int8_t, int8_t);
static inline int16_t pow_fun_int16_t(int16_t, int16_t);
static inline int32_t pow_fun_int32_t(int32_t, int32_t);
static inline int64_t pow_fun_int64_t(int64_t, int64_t);
static inline uint8_t pow_fun_uint8_t(uint8_t, uint8_t);
static inline uint16_t pow_fun_uint16_t(uint16_t, uint16_t);
static inline uint32_t pow_fun_uint32_t(uint32_t, uint32_t);
static inline uint64_t pow_fun_uint64_t(uint64_t, uint64_t);

static inline int8_t abs_fun_int8_t(int8_t);
static inline int16_t abs_fun_int16_t(int16_t);
static inline int32_t abs_fun_int32_t(int32_t);
static inline int64_t abs_fun_int64_t(int64_t);
static inline float abs_fun_float(float);
static inline double abs_fun_double(double);

static inline int8_t signum_fun_int8_t(int8_t);
static inline int16_t signum_fun_int16_t(int16_t);
static inline int32_t signum_fun_int32_t(int32_t);
static inline int64_t signum_fun_int64_t(int64_t);
static inline uint8_t signum_fun_uint8_t(uint8_t);
static inline uint16_t signum_fun_uint16_t(uint16_t);
static inline uint32_t signum_fun_uint32_t(uint32_t);
static inline uint64_t signum_fun_uint64_t(uint64_t);
static inline float signum_fun_float(float);
static inline double signum_fun_double(double);

static inline float logBase_fun_float(float, float);
static inline double logBase_fun_double(double, double);



static inline int8_t setBit_fun_int8_t(int8_t, uint32_t);
static inline int16_t setBit_fun_int16_t(int16_t, uint32_t);
static inline int32_t setBit_fun_int32_t(int32_t, uint32_t);
static inline int64_t setBit_fun_int64_t(int64_t, uint32_t);
static inline uint8_t setBit_fun_uint8_t(uint8_t, uint32_t);
static inline uint16_t setBit_fun_uint16_t(uint16_t, uint32_t);
static inline uint32_t setBit_fun_uint32_t(uint32_t, uint32_t);
static inline uint64_t setBit_fun_uint64_t(uint64_t, uint32_t);

static inline int8_t clearBit_fun_int8_t(int8_t, uint32_t);
static inline int16_t clearBit_fun_int16_t(int16_t, uint32_t);
static inline int32_t clearBit_fun_int32_t(int32_t, uint32_t);
static inline int64_t clearBit_fun_int64_t(int64_t, uint32_t);
static inline uint8_t clearBit_fun_uint8_t(uint8_t, uint32_t);
static inline uint16_t clearBit_fun_uint16_t(uint16_t, uint32_t);
static inline uint32_t clearBit_fun_uint32_t(uint32_t, uint32_t);
static inline uint64_t clearBit_fun_uint64_t(uint64_t, uint32_t);

static inline int8_t complementBit_fun_int8_t(int8_t, uint32_t);
static inline int16_t complementBit_fun_int16_t(int16_t, uint32_t);
static inline int32_t complementBit_fun_int32_t(int32_t, uint32_t);
static inline int64_t complementBit_fun_int64_t(int64_t, uint32_t);
static inline uint8_t complementBit_fun_uint8_t(uint8_t, uint32_t);
static inline uint16_t complementBit_fun_uint16_t(uint16_t, uint32_t);
static inline uint32_t complementBit_fun_uint32_t(uint32_t, uint32_t);
static inline uint64_t complementBit_fun_uint64_t(uint64_t, uint32_t);

static inline int testBit_fun_int8_t(int8_t, uint32_t);
static inline int testBit_fun_int16_t(int16_t, uint32_t);
static inline int testBit_fun_int32_t(int32_t, uint32_t);
static inline int testBit_fun_int64_t(int64_t, uint32_t);
static inline int testBit_fun_uint8_t(uint8_t, uint32_t);
static inline int testBit_fun_uint16_t(uint16_t, uint32_t);
static inline int testBit_fun_uint32_t(uint32_t, uint32_t);
static inline int testBit_fun_uint64_t(uint64_t, uint32_t);

static inline int8_t rotateL_fun_int8_t(int8_t, int32_t);
static inline int16_t rotateL_fun_int16_t(int16_t, int32_t);
static inline int32_t rotateL_fun_int32_t(int32_t, int32_t);
static inline int64_t rotateL_fun_int64_t(int64_t, int32_t);
static inline uint8_t rotateL_fun_uint8_t(uint8_t, int32_t);
static inline uint16_t rotateL_fun_uint16_t(uint16_t, int32_t);
static inline uint32_t rotateL_fun_uint32_t(uint32_t, int32_t);
static inline uint64_t rotateL_fun_uint64_t(uint64_t, int32_t);

static inline int8_t rotateR_fun_int8_t(int8_t, int32_t);
static inline int16_t rotateR_fun_int16_t(int16_t, int32_t);
static inline int32_t rotateR_fun_int32_t(int32_t, int32_t);
static inline int64_t rotateR_fun_int64_t(int64_t, int32_t);
static inline uint8_t rotateR_fun_uint8_t(uint8_t, int32_t);
static inline uint16_t rotateR_fun_uint16_t(uint16_t, int32_t);
static inline uint32_t rotateR_fun_uint32_t(uint32_t, int32_t);
static inline uint64_t rotateR_fun_uint64_t(uint64_t, int32_t);

static inline int8_t reverseBits_fun_int8_t(int8_t);
static inline int16_t reverseBits_fun_int16_t(int16_t);
static inline int32_t reverseBits_fun_int32_t(int32_t);
static inline int64_t reverseBits_fun_int64_t(int64_t);
static inline uint8_t reverseBits_fun_uint8_t(uint8_t);
static inline uint16_t reverseBits_fun_uint16_t(uint16_t);
static inline uint32_t reverseBits_fun_uint32_t(uint32_t);
static inline uint64_t reverseBits_fun_uint64_t(uint64_t);

static inline uint32_t bitScan_fun_int8_t(int8_t);
static inline uint32_t bitScan_fun_int16_t(int16_t);
static inline uint32_t bitScan_fun_int32_t(int32_t);
static inline uint32_t bitScan_fun_int64_t(int64_t);
static inline uint32_t bitScan_fun_uint8_t(uint8_t);
static inline uint32_t bitScan_fun_uint16_t(uint16_t);
static inline uint32_t bitScan_fun_uint32_t(uint32_t);
static inline uint32_t bitScan_fun_uint64_t(uint64_t);

static inline uint32_t bitCount_fun_int8_t(int8_t);
static inline uint32_t bitCount_fun_int16_t(int16_t);
static inline uint32_t bitCount_fun_int32_t(int32_t);
static inline uint32_t bitCount_fun_int64_t(int64_t);
static inline uint32_t bitCount_fun_uint8_t(uint8_t);
static inline uint32_t bitCount_fun_uint16_t(uint16_t);
static inline uint32_t bitCount_fun_uint32_t(uint32_t);
static inline uint32_t bitCount_fun_uint64_t(uint64_t);

#define mkComplex(d1,d2) ((d1) + (d2) * I)

typedef struct {
    int8_t re;
    int8_t im;
} complexOf_int8_t;

typedef struct {
    int16_t re;
    int16_t im;
} complexOf_int16_t;

typedef struct {
    int32_t re;
    int32_t im;
} complexOf_int32_t;

typedef struct {
    int64_t re;
    int64_t im;
} complexOf_int64_t;

typedef struct {
    uint8_t re;
    uint8_t im;
} complexOf_uint8_t;

typedef struct {
    uint16_t re;
    uint16_t im;
} complexOf_uint16_t;

typedef struct {
    uint32_t re;
    uint32_t im;
} complexOf_uint32_t;

typedef struct {
    uint64_t re;
    uint64_t im;
} complexOf_uint64_t;

static inline int equal_fun_complexOf_int8_t(complexOf_int8_t, complexOf_int8_t);
static inline int equal_fun_complexOf_int16_t(complexOf_int16_t, complexOf_int16_t);
static inline int equal_fun_complexOf_int32_t(complexOf_int32_t, complexOf_int32_t);
static inline int equal_fun_complexOf_int64_t(complexOf_int64_t, complexOf_int64_t);
static inline int equal_fun_complexOf_uint8_t(complexOf_uint8_t, complexOf_uint8_t);
static inline int equal_fun_complexOf_uint16_t(complexOf_uint16_t, complexOf_uint16_t);
static inline int equal_fun_complexOf_uint32_t(complexOf_uint32_t, complexOf_uint32_t);
static inline int equal_fun_complexOf_uint64_t(complexOf_uint64_t, complexOf_uint64_t);

static inline complexOf_int8_t negate_fun_complexOf_int8_t(complexOf_int8_t);
static inline complexOf_int16_t negate_fun_complexOf_int16_t(complexOf_int16_t);
static inline complexOf_int32_t negate_fun_complexOf_int32_t(complexOf_int32_t);
static inline complexOf_int64_t negate_fun_complexOf_int64_t(complexOf_int64_t);
static inline complexOf_uint8_t negate_fun_complexOf_uint8_t(complexOf_uint8_t);
static inline complexOf_uint16_t negate_fun_complexOf_uint16_t(complexOf_uint16_t);
static inline complexOf_uint32_t negate_fun_complexOf_uint32_t(complexOf_uint32_t);
static inline complexOf_uint64_t negate_fun_complexOf_uint64_t(complexOf_uint64_t);

static inline complexOf_int8_t abs_fun_complexOf_int8_t(complexOf_int8_t);
static inline complexOf_int16_t abs_fun_complexOf_int16_t(complexOf_int16_t);
static inline complexOf_int32_t abs_fun_complexOf_int32_t(complexOf_int32_t);
static inline complexOf_int64_t abs_fun_complexOf_int64_t(complexOf_int64_t);
static inline complexOf_uint8_t abs_fun_complexOf_uint8_t(complexOf_uint8_t);
static inline complexOf_uint16_t abs_fun_complexOf_uint16_t(complexOf_uint16_t);
static inline complexOf_uint32_t abs_fun_complexOf_uint32_t(complexOf_uint32_t);
static inline complexOf_uint64_t abs_fun_complexOf_uint64_t(complexOf_uint64_t);

static inline complexOf_int8_t signum_fun_complexOf_int8_t(complexOf_int8_t);
static inline complexOf_int16_t signum_fun_complexOf_int16_t(complexOf_int16_t);
static inline complexOf_int32_t signum_fun_complexOf_int32_t(complexOf_int32_t);
static inline complexOf_int64_t signum_fun_complexOf_int64_t(complexOf_int64_t);
static inline complexOf_uint8_t signum_fun_complexOf_uint8_t(complexOf_uint8_t);
static inline complexOf_uint16_t signum_fun_complexOf_uint16_t(complexOf_uint16_t);
static inline complexOf_uint32_t signum_fun_complexOf_uint32_t(complexOf_uint32_t);
static inline complexOf_uint64_t signum_fun_complexOf_uint64_t(complexOf_uint64_t);
static inline float complex signum_fun_complexOf_float(float complex);
static inline double complex signum_fun_complexOf_double(double complex);

static inline complexOf_int8_t add_fun_complexOf_int8_t(complexOf_int8_t, complexOf_int8_t);
static inline complexOf_int16_t add_fun_complexOf_int16_t(complexOf_int16_t, complexOf_int16_t);
static inline complexOf_int32_t add_fun_complexOf_int32_t(complexOf_int32_t, complexOf_int32_t);
static inline complexOf_int64_t add_fun_complexOf_int64_t(complexOf_int64_t, complexOf_int64_t);
static inline complexOf_uint8_t add_fun_complexOf_uint8_t(complexOf_uint8_t, complexOf_uint8_t);
static inline complexOf_uint16_t add_fun_complexOf_uint16_t(complexOf_uint16_t, complexOf_uint16_t);
static inline complexOf_uint32_t add_fun_complexOf_uint32_t(complexOf_uint32_t, complexOf_uint32_t);
static inline complexOf_uint64_t add_fun_complexOf_uint64_t(complexOf_uint64_t, complexOf_uint64_t);

static inline complexOf_int8_t sub_fun_complexOf_int8_t(complexOf_int8_t, complexOf_int8_t);
static inline complexOf_int16_t sub_fun_complexOf_int16_t(complexOf_int16_t, complexOf_int16_t);
static inline complexOf_int32_t sub_fun_complexOf_int32_t(complexOf_int32_t, complexOf_int32_t);
static inline complexOf_int64_t sub_fun_complexOf_int64_t(complexOf_int64_t, complexOf_int64_t);
static inline complexOf_uint8_t sub_fun_complexOf_uint8_t(complexOf_uint8_t, complexOf_uint8_t);
static inline complexOf_uint16_t sub_fun_complexOf_uint16_t(complexOf_uint16_t, complexOf_uint16_t);
static inline complexOf_uint32_t sub_fun_complexOf_uint32_t(complexOf_uint32_t, complexOf_uint32_t);
static inline complexOf_uint64_t sub_fun_complexOf_uint64_t(complexOf_uint64_t, complexOf_uint64_t);

static inline complexOf_int8_t mult_fun_complexOf_int8_t(complexOf_int8_t, complexOf_int8_t);
static inline complexOf_int16_t mult_fun_complexOf_int16_t(complexOf_int16_t, complexOf_int16_t);
static inline complexOf_int32_t mult_fun_complexOf_int32_t(complexOf_int32_t, complexOf_int32_t);
static inline complexOf_int64_t mult_fun_complexOf_int64_t(complexOf_int64_t, complexOf_int64_t);
static inline complexOf_uint8_t mult_fun_complexOf_uint8_t(complexOf_uint8_t, complexOf_uint8_t);
static inline complexOf_uint16_t mult_fun_complexOf_uint16_t(complexOf_uint16_t, complexOf_uint16_t);
static inline complexOf_uint32_t mult_fun_complexOf_uint32_t(complexOf_uint32_t, complexOf_uint32_t);
static inline complexOf_uint64_t mult_fun_complexOf_uint64_t(complexOf_uint64_t, complexOf_uint64_t);

static inline float complex logBase_fun_complexOf_float(float complex, float complex);
static inline double complex logBase_fun_complexOf_double(double complex, double complex);

static inline complexOf_int8_t complex_fun_int8_t(int8_t, int8_t);
static inline complexOf_int16_t complex_fun_int16_t(int16_t, int16_t);
static inline complexOf_int32_t complex_fun_int32_t(int32_t, int32_t);
static inline complexOf_int64_t complex_fun_int64_t(int64_t, int64_t);
static inline complexOf_uint8_t complex_fun_uint8_t(uint8_t, uint8_t);
static inline complexOf_uint16_t complex_fun_uint16_t(uint16_t, uint16_t);
static inline complexOf_uint32_t complex_fun_uint32_t(uint32_t, uint32_t);
static inline complexOf_uint64_t complex_fun_uint64_t(uint64_t, uint64_t);
static inline float complex complex_fun_float(float, float);
static inline double complex complex_fun_double(double, double);

static inline int8_t creal_fun_complexOf_int8_t(complexOf_int8_t);
static inline int16_t creal_fun_complexOf_int16_t(complexOf_int16_t);
static inline int32_t creal_fun_complexOf_int32_t(complexOf_int32_t);
static inline int64_t creal_fun_complexOf_int64_t(complexOf_int64_t);
static inline uint8_t creal_fun_complexOf_uint8_t(complexOf_uint8_t);
static inline uint16_t creal_fun_complexOf_uint16_t(complexOf_uint16_t);
static inline uint32_t creal_fun_complexOf_uint32_t(complexOf_uint32_t);
static inline uint64_t creal_fun_complexOf_uint64_t(complexOf_uint64_t);

static inline int8_t cimag_fun_complexOf_int8_t(complexOf_int8_t);
static inline int16_t cimag_fun_complexOf_int16_t(complexOf_int16_t);
static inline int32_t cimag_fun_complexOf_int32_t(complexOf_int32_t);
static inline int64_t cimag_fun_complexOf_int64_t(complexOf_int64_t);
static inline uint8_t cimag_fun_complexOf_uint8_t(complexOf_uint8_t);
static inline uint16_t cimag_fun_complexOf_uint16_t(complexOf_uint16_t);
static inline uint32_t cimag_fun_complexOf_uint32_t(complexOf_uint32_t);
static inline uint64_t cimag_fun_complexOf_uint64_t(complexOf_uint64_t);

static inline complexOf_int8_t conj_fun_complexOf_int8_t(complexOf_int8_t);
static inline complexOf_int16_t conj_fun_complexOf_int16_t(complexOf_int16_t);
static inline complexOf_int32_t conj_fun_complexOf_int32_t(complexOf_int32_t);
static inline complexOf_int64_t conj_fun_complexOf_int64_t(complexOf_int64_t);
static inline complexOf_uint8_t conj_fun_complexOf_uint8_t(complexOf_uint8_t);
static inline complexOf_uint16_t conj_fun_complexOf_uint16_t(complexOf_uint16_t);
static inline complexOf_uint32_t conj_fun_complexOf_uint32_t(complexOf_uint32_t);
static inline complexOf_uint64_t conj_fun_complexOf_uint64_t(complexOf_uint64_t);

static inline int8_t magnitude_fun_complexOf_int8_t(complexOf_int8_t);
static inline int16_t magnitude_fun_complexOf_int16_t(complexOf_int16_t);
static inline int32_t magnitude_fun_complexOf_int32_t(complexOf_int32_t);
static inline int64_t magnitude_fun_complexOf_int64_t(complexOf_int64_t);
static inline uint8_t magnitude_fun_complexOf_uint8_t(complexOf_uint8_t);
static inline uint16_t magnitude_fun_complexOf_uint16_t(complexOf_uint16_t);
static inline uint32_t magnitude_fun_complexOf_uint32_t(complexOf_uint32_t);
static inline uint64_t magnitude_fun_complexOf_uint64_t(complexOf_uint64_t);

static inline int8_t phase_fun_complexOf_int8_t(complexOf_int8_t);
static inline int16_t phase_fun_complexOf_int16_t(complexOf_int16_t);
static inline int32_t phase_fun_complexOf_int32_t(complexOf_int32_t);
static inline int64_t phase_fun_complexOf_int64_t(complexOf_int64_t);
static inline uint8_t phase_fun_complexOf_uint8_t(complexOf_uint8_t);
static inline uint16_t phase_fun_complexOf_uint16_t(complexOf_uint16_t);
static inline uint32_t phase_fun_complexOf_uint32_t(complexOf_uint32_t);
static inline uint64_t phase_fun_complexOf_uint64_t(complexOf_uint64_t);

static inline complexOf_int8_t mkPolar_fun_int8_t(int8_t, int8_t);
static inline complexOf_int16_t mkPolar_fun_int16_t(int16_t, int16_t);
static inline complexOf_int32_t mkPolar_fun_int32_t(int32_t, int32_t);
static inline complexOf_int64_t mkPolar_fun_int64_t(int64_t, int64_t);
static inline complexOf_uint8_t mkPolar_fun_uint8_t(uint8_t, uint8_t);
static inline complexOf_uint16_t mkPolar_fun_uint16_t(uint16_t, uint16_t);
static inline complexOf_uint32_t mkPolar_fun_uint32_t(uint32_t, uint32_t);
static inline complexOf_uint64_t mkPolar_fun_uint64_t(uint64_t, uint64_t);
static inline float complex mkPolar_fun_float(float, float);
static inline double complex mkPolar_fun_double(double, double);

static inline complexOf_int8_t cis_fun_int8_t(int8_t);
static inline complexOf_int16_t cis_fun_int16_t(int16_t);
static inline complexOf_int32_t cis_fun_int32_t(int32_t);
static inline complexOf_int64_t cis_fun_int64_t(int64_t);
static inline complexOf_uint8_t cis_fun_uint8_t(uint8_t);
static inline complexOf_uint16_t cis_fun_uint16_t(uint16_t);
static inline complexOf_uint32_t cis_fun_uint32_t(uint32_t);
static inline complexOf_uint64_t cis_fun_uint64_t(uint64_t);
static inline float complex cis_fun_float(float);
static inline double complex cis_fun_double(double);

/*--------------------------------------------------------------------------*
 *                 pow(), abs(), signum(), logBase()                        *
 *--------------------------------------------------------------------------*/

static inline int8_t pow_fun_int8_t(int8_t a, int8_t b) {
    int8_t r = 1;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %" PRId8 " `pow` %" PRId8, a, b);
        exit(1);
    }
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

static inline int16_t pow_fun_int16_t(int16_t a, int16_t b) {
    int16_t r = 1;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %" PRId16 " `pow` %" PRId16, a, b);
        exit(1);
    }
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

static inline int32_t pow_fun_int32_t(int32_t a, int32_t b) {
    int32_t r = 1;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %" PRId32 " `pow` %d" PRId32, a, b);
        exit(1);
    }
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

static inline int64_t pow_fun_int64_t(int64_t a, int64_t b) {
    int64_t r = 1;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %" PRId64 " `pow` %" PRId64, a, b);
        exit(1);
    }
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

static inline uint8_t pow_fun_uint8_t(uint8_t a, uint8_t b) {
    uint8_t r = 1;
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

static inline uint16_t pow_fun_uint16_t(uint16_t a, uint16_t b) {
    uint16_t r = 1;
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

static inline uint32_t pow_fun_uint32_t(uint32_t a, uint32_t b) {
    uint32_t r = 1;
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

static inline uint64_t pow_fun_uint64_t(uint64_t a, uint64_t b) {
    uint64_t r = 1;
    for (int i = 0; i < b; ++i)
        r *= a;
    return r;
}

static inline int8_t abs_fun_int8_t(int8_t a) {
    // From Bit Twiddling Hacks:
    //    "Compute the integer absolute value (abs) without branching"
    int8_t mask = a >> 7;
    return (a + mask) ^ mask;
}

static inline int16_t abs_fun_int16_t(int16_t a) {
    // From Bit Twiddling Hacks:
    //    "Compute the integer absolute value (abs) without branching"
    int16_t mask = a >> 15;
    return (a + mask) ^ mask;
}

static inline int32_t abs_fun_int32_t(int32_t a) {
    // From Bit Twiddling Hacks:
    //    "Compute the integer absolute value (abs) without branching"
    int32_t mask = a >> 31;
    return (a + mask) ^ mask;
}

static inline int64_t abs_fun_int64_t(int64_t a) {
    // From Bit Twiddling Hacks:
    //    "Compute the integer absolute value (abs) without branching"
    int64_t mask = a >> 63;
    return (a + mask) ^ mask;
}

static inline float abs_fun_float(float a) {
    return fabsf(a);
}

static inline double abs_fun_double(double a) {
    return fabs(a);
}


static inline int8_t signum_fun_int8_t(int8_t a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 7);
}

static inline int16_t signum_fun_int16_t(int16_t a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 15);
}

static inline int32_t signum_fun_int32_t(int32_t a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 31);
}

static inline int64_t signum_fun_int64_t(int64_t a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 63);
}

static inline uint8_t signum_fun_uint8_t(uint8_t a) {
    return (a > 0);
}

static inline uint16_t signum_fun_uint16_t(uint16_t a) {
    return (a > 0);
}

static inline uint32_t signum_fun_uint32_t(uint32_t a) {
    return (a > 0);
}

static inline uint64_t signum_fun_uint64_t(uint64_t a) {
    return (a > 0);
}

static inline float signum_fun_float(float a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a > 0) - (a < 0);
}

static inline float logBase_fun_float(float a, float b) {
    return logf(b) / logf(a);
}

static inline double signum_fun_double(double a) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a > 0) - (a < 0);
}

static inline double logBase_fun_double(double a, double b) {
    return log(b) / log(a);
}


/*--------------------------------------------------------------------------*
 *                 Bit operations                                           *
 *--------------------------------------------------------------------------*/

static inline int8_t setBit_fun_int8_t(int8_t x, uint32_t i) {
    return x | 1 << i;
}

static inline int16_t setBit_fun_int16_t(int16_t x, uint32_t i) {
    return x | 1 << i;
}

static inline int32_t setBit_fun_int32_t(int32_t x, uint32_t i) {
    return x | 1 << i;
}

static inline int64_t setBit_fun_int64_t(int64_t x, uint32_t i) {
    return x | 1 << i;
}

static inline uint8_t setBit_fun_uint8_t(uint8_t x, uint32_t i) {
    return x | 1 << i;
}

static inline uint16_t setBit_fun_uint16_t(uint16_t x, uint32_t i) {
    return x | 1 << i;
}

static inline uint32_t setBit_fun_uint32_t(uint32_t x, uint32_t i) {
    return x | 1 << i;
}

static inline uint64_t setBit_fun_uint64_t(uint64_t x, uint32_t i) {
    return x | 1 << i;
}

static inline int8_t clearBit_fun_int8_t(int8_t x, uint32_t i) {
    return x & ~(1 << i);
}

static inline int16_t clearBit_fun_int16_t(int16_t x, uint32_t i) {
    return x & ~(1 << i);
}

static inline int32_t clearBit_fun_int32_t(int32_t x, uint32_t i) {
    return x & ~(1 << i);
}

static inline int64_t clearBit_fun_int64_t(int64_t x, uint32_t i) {
    return x & ~(1 << i);
}

static inline uint8_t clearBit_fun_uint8_t(uint8_t x, uint32_t i) {
    return x & ~(1 << i);
}

static inline uint16_t clearBit_fun_uint16_t(uint16_t x, uint32_t i) {
    return x & ~(1 << i);
}

static inline uint32_t clearBit_fun_uint32_t(uint32_t x, uint32_t i) {
    return x & ~(1 << i);
}

static inline uint64_t clearBit_fun_uint64_t(uint64_t x, uint32_t i) {
    return x & ~(1 << i);
}

static inline int8_t complementBit_fun_int8_t(int8_t x, uint32_t i) {
    return x ^ 1 << i;
}

static inline int16_t complementBit_fun_int16_t(int16_t x, uint32_t i) {
    return x ^ 1 << i;
}

static inline int32_t complementBit_fun_int32_t(int32_t x, uint32_t i) {
    return x ^ 1 << i;
}

static inline int64_t complementBit_fun_int64_t(int64_t x, uint32_t i) {
    return x ^ 1 << i;
}

static inline uint8_t complementBit_fun_uint8_t(uint8_t x, uint32_t i) {
    return x ^ 1 << i;
}

static inline uint16_t complementBit_fun_uint16_t(uint16_t x, uint32_t i) {
    return x ^ 1 << i;
}

static inline uint32_t complementBit_fun_uint32_t(uint32_t x, uint32_t i) {
    return x ^ 1 << i;
}

static inline uint64_t complementBit_fun_uint64_t(uint64_t x, uint32_t i) {
    return x ^ 1 << i;
}

static inline int testBit_fun_int8_t(int8_t x, uint32_t i) {
    return (x & 1 << i) != 0;
}

static inline int testBit_fun_int16_t(int16_t x, uint32_t i) {
    return (x & 1 << i) != 0;
}

static inline int testBit_fun_int32_t(int32_t x, uint32_t i) {
    return (x & 1 << i) != 0;
}

static inline int testBit_fun_int64_t(int64_t x, uint32_t i) {
    return (x & 1 << i) != 0;
}

static inline int testBit_fun_uint8_t(uint8_t x, uint32_t i) {
    return (x & 1 << i) != 0;
}

static inline int testBit_fun_uint16_t(uint16_t x, uint32_t i) {
    return (x & 1 << i) != 0;
}

static inline int testBit_fun_uint32_t(uint32_t x, uint32_t i) {
    return (x & 1 << i) != 0;
}

static inline int testBit_fun_uint64_t(uint64_t x, uint32_t i) {
    return (x & 1 << i) != 0;
}

static inline int8_t rotateL_fun_int8_t(int8_t x, int32_t i) {
    if ((i %= 8) == 0) return x;
    return (x << i) | ((0x7f >> (7 - i)) & (x >> (8 - i)));
}

static inline int16_t rotateL_fun_int16_t(int16_t x, int32_t i) {
    if ((i %= 16) == 0) return x;
    return (x << i) | ((0x7fff >> (15 - i)) & (x >> (16 - i)));
}

static inline int32_t rotateL_fun_int32_t(int32_t x, int32_t i) {
    if ((i %= 32) == 0) return x;
    return (x << i) | ((0x7fffffff >> (31 - i)) & (x >> (32 - i)));
}

static inline int64_t rotateL_fun_int64_t(int64_t x, int32_t i) {
    if ((i %= 64) == 0) return x;
    return (x << i) | ((0x7fffffffffffffffll >> (63 - i)) & (x >> (64 - i)));
}

static inline uint8_t rotateL_fun_uint8_t(uint8_t x, int32_t i) {
    if ((i %= 8) == 0) return x;
    return (x << i) | (x >> (8 - i));
}

static inline uint16_t rotateL_fun_uint16_t(uint16_t x, int32_t i) {
    if ((i %= 16) == 0) return x;
    return (x << i) | (x >> (16 - i));
}

static inline uint32_t rotateL_fun_uint32_t(uint32_t x, int32_t i) {
    if ((i %= 32) == 0) return x;
    return (x << i) | (x >> (32 - i));
}

static inline uint64_t rotateL_fun_uint64_t(uint64_t x, int32_t i) {
    if ((i %= 64) == 0) return x;
    return (x << i) | (x >> (64 - i));
}

static inline int8_t rotateR_fun_int8_t(int8_t x, int32_t i) {
    if ((i %= 8) == 0) return x;
    return (x << (8 - i)) | ((0x7f >> (i - 1)) & (x >> i));
}

static inline int16_t rotateR_fun_int16_t(int16_t x, int32_t i) {
    if ((i %= 16) == 0) return x;
    return (x << (16 - i)) | ((0x7fff >> (i - 1)) & (x >> i));
}

static inline int32_t rotateR_fun_int32_t(int32_t x, int32_t i) {
    if ((i %= 32) == 0) return x;
    return (x << (32 - i)) | ((0x7fffffff >> (i - 1)) & (x >> i));
}

static inline int64_t rotateR_fun_int64_t(int64_t x, int32_t i) {
    if ((i %= 64) == 0) return x;
    return (x << (64 - i)) | ((0x7fffffffffffffffll >> (i - 1)) & (x >> i));
}

static inline uint8_t rotateR_fun_uint8_t(uint8_t x, int32_t i) {
    if ((i %= 8) == 0) return x;
    return (x << (8 - i)) | (x >> i);
}

static inline uint16_t rotateR_fun_uint16_t(uint16_t x, int32_t i) {
    if ((i %= 16) == 0) return x;
    return (x << (16 - i)) | (x >> i);
}

static inline uint32_t rotateR_fun_uint32_t(uint32_t x, int32_t i) {
    if ((i %= 32) == 0) return x;
    return (x << (32 - i)) | (x >> i);
}

static inline uint64_t rotateR_fun_uint64_t(uint64_t x, int32_t i) {
    if ((i %= 64) == 0) return x;
    return (x << (64 - i)) | (x >> i);
}

static inline int8_t reverseBits_fun_int8_t(int8_t x) {
    int8_t r = x;
    int i = 7;
    for (x = x >> 1 & 0x7f; x; x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

static inline int16_t reverseBits_fun_int16_t(int16_t x) {
    int16_t r = x;
    int i = 15;
    for (x = x >> 1 & 0x7fff; x; x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

static inline int32_t reverseBits_fun_int32_t(int32_t x) {
    int32_t r = x;
    int i = 31;
    for (x = x >> 1 & 0x7fffffff; x; x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

static inline int64_t reverseBits_fun_int64_t(int64_t x) {
    int64_t r = x;
    int i = 63;
    for (x = x >> 1 & 0x7fffffffffffffffll; x; x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

static inline uint8_t reverseBits_fun_uint8_t(uint8_t x) {
    uint8_t r = x;
    int i = 7;
    while (x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

static inline uint16_t reverseBits_fun_uint16_t(uint16_t x) {
    uint16_t r = x;
    int i = 15;
    while (x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

static inline uint32_t reverseBits_fun_uint32_t(uint32_t x) {
    uint32_t r = x;
    int i = 31;
    while (x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

static inline uint64_t reverseBits_fun_uint64_t(uint64_t x) {
    uint64_t r = x;
    int i = 63;
    while (x >>= 1) {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}

static inline uint32_t bitScan_fun_int8_t(int8_t x) {
    uint32_t r = 0;
    int8_t s = (x & 0x80);
    if (x == 0) return 7;
    while ((int8_t)((x <<= 1) & 0x80) == s)
        ++r;
    return r;
}

static inline uint32_t bitScan_fun_int16_t(int16_t x) {
    uint32_t r = 0;
    int16_t s = (x & 0x8000);
    if (x == 0) return 15;
    while ((int16_t)((x <<= 1) & 0x8000) == s)
        ++r;
    return r;
}

static inline uint32_t bitScan_fun_int32_t(int32_t x) {
    uint32_t r = 0;
    int32_t s = (x & 0x80000000);
    if (x == 0) return 31;
    while ((int32_t)((x <<= 1) & 0x80000000) == s)
        ++r;
    return r;
}

static inline uint32_t bitScan_fun_int64_t(int64_t x) {
    uint32_t r = 0;
    int64_t s = (x & 0x8000000000000000ll);
    if (x == 0) return 63;
    while ((int64_t)((x <<= 1) & 0x8000000000000000ll) == s)
        ++r;
    return r;
}

static inline uint32_t bitScan_fun_uint8_t(uint8_t x) {
    uint32_t r = 8;
    while (x) {
        --r;
        x >>= 1;
    }
    return r;
}

static inline uint32_t bitScan_fun_uint16_t(uint16_t x) {
    uint32_t r = 16;
    while (x) {
        --r;
        x >>= 1;
    }
    return r;
}

static inline uint32_t bitScan_fun_uint32_t(uint32_t x) {
    uint32_t r = 32;
    while (x) {
        --r;
        x >>= 1;
    }
    return r;
}

static inline uint32_t bitScan_fun_uint64_t(uint64_t x) {
    uint32_t r = 64;
    while (x) {
        --r;
        x >>= 1;
    }
    return r;
}

static inline uint32_t bitCount_fun_int8_t(int8_t x) {
    uint32_t r = x & 1;
    for (x = x >> 1 & 0x7f; x; x >>= 1)
        r += x & 1;
    return r;
}

static inline uint32_t bitCount_fun_int16_t(int16_t x) {
    uint32_t r = x & 1;
    for (x = x >> 1 & 0x7fff; x; x >>= 1)
        r += x & 1;
    return r;
}

static inline uint32_t bitCount_fun_int32_t(int32_t x) {
    uint32_t r = x & 1;
    for (x = x >> 1 & 0x7fffffff; x; x >>= 1)
        r += x & 1;
    return r;
}

static inline uint32_t bitCount_fun_int64_t(int64_t x) {
    uint32_t r = x & 1;
    for (x = x >> 1 & 0x7fffffffffffffffll; x; x >>= 1)
        r += x & 1;
    return r;
}

static inline uint32_t bitCount_fun_uint8_t(uint8_t x) {
    uint32_t r = x & 1;
    while (x >>= 1)
        r += x & 1;
    return r;
}

static inline uint32_t bitCount_fun_uint16_t(uint16_t x) {
    uint32_t r = x & 1;
    while (x >>= 1)
        r += x & 1;
    return r;
}

static inline uint32_t bitCount_fun_uint32_t(uint32_t x) {
    uint32_t r = x & 1;
    while (x >>= 1)
        r += x & 1;
    return r;
}

static inline uint32_t bitCount_fun_uint64_t(uint64_t x) {
    uint32_t r = x & 1;
    while (x >>= 1)
        r += x & 1;
    return r;
}

/*--------------------------------------------------------------------------*
 *                 Complex numbers                                          *
 *--------------------------------------------------------------------------*/

static inline int equal_fun_complexOf_int8_t(complexOf_int8_t a, complexOf_int8_t b) {
    return a.re == b.re && a.im == b.im;
}

static inline int equal_fun_complexOf_int16_t(complexOf_int16_t a, complexOf_int16_t b) {
    return a.re == b.re && a.im == b.im;
}

static inline int equal_fun_complexOf_int32_t(complexOf_int32_t a, complexOf_int32_t b) {
    return a.re == b.re && a.im == b.im;
}

static inline int equal_fun_complexOf_int64_t(complexOf_int64_t a, complexOf_int64_t b) {
    return a.re == b.re && a.im == b.im;
}

static inline int equal_fun_complexOf_uint8_t(complexOf_uint8_t a, complexOf_uint8_t b) {
    return a.re == b.re && a.im == b.im;
}

static inline int equal_fun_complexOf_uint16_t(complexOf_uint16_t a, complexOf_uint16_t b) {
    return a.re == b.re && a.im == b.im;
}

static inline int equal_fun_complexOf_uint32_t(complexOf_uint32_t a, complexOf_uint32_t b) {
    return a.re == b.re && a.im == b.im;
}

static inline int equal_fun_complexOf_uint64_t(complexOf_uint64_t a, complexOf_uint64_t b) {
    return a.re == b.re && a.im == b.im;
}

static inline complexOf_int8_t negate_fun_complexOf_int8_t(complexOf_int8_t a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

static inline complexOf_int16_t negate_fun_complexOf_int16_t(complexOf_int16_t a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

static inline complexOf_int32_t negate_fun_complexOf_int32_t(complexOf_int32_t a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

static inline complexOf_int64_t negate_fun_complexOf_int64_t(complexOf_int64_t a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

static inline complexOf_uint8_t negate_fun_complexOf_uint8_t(complexOf_uint8_t a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

static inline complexOf_uint16_t negate_fun_complexOf_uint16_t(complexOf_uint16_t a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

static inline complexOf_uint32_t negate_fun_complexOf_uint32_t(complexOf_uint32_t a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

static inline complexOf_uint64_t negate_fun_complexOf_uint64_t(complexOf_uint64_t a) {
    a.re = -a.re;
    a.im = -a.im;
    return a;
}

static inline complexOf_int8_t abs_fun_complexOf_int8_t(complexOf_int8_t a) {
    a.re = magnitude_fun_complexOf_int8_t(a);
    a.im = 0;
    return a;
}

static inline complexOf_int16_t abs_fun_complexOf_int16_t(complexOf_int16_t a) {
    a.re = magnitude_fun_complexOf_int16_t(a);
    a.im = 0;
    return a;
}

static inline complexOf_int32_t abs_fun_complexOf_int32_t(complexOf_int32_t a) {
    a.re = magnitude_fun_complexOf_int32_t(a);
    a.im = 0;
    return a;
}

static inline complexOf_int64_t abs_fun_complexOf_int64_t(complexOf_int64_t a) {
    a.re = magnitude_fun_complexOf_int64_t(a);
    a.im = 0;
    return a;
}

static inline complexOf_uint8_t abs_fun_complexOf_uint8_t(complexOf_uint8_t a) {
    a.re = magnitude_fun_complexOf_uint8_t(a);
    a.im = 0;
    return a;
}

static inline complexOf_uint16_t abs_fun_complexOf_uint16_t(complexOf_uint16_t a) {
    a.re = magnitude_fun_complexOf_uint16_t(a);
    a.im = 0;
    return a;
}

static inline complexOf_uint32_t abs_fun_complexOf_uint32_t(complexOf_uint32_t a) {
    a.re = magnitude_fun_complexOf_uint32_t(a);
    a.im = 0;
    return a;
}

static inline complexOf_uint64_t abs_fun_complexOf_uint64_t(complexOf_uint64_t a) {
    a.re = magnitude_fun_complexOf_uint64_t(a);
    a.im = 0;
    return a;
}

static inline complexOf_int8_t signum_fun_complexOf_int8_t(complexOf_int8_t a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        int8_t m = magnitude_fun_complexOf_int8_t(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

static inline complexOf_int16_t signum_fun_complexOf_int16_t(complexOf_int16_t a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        int16_t m = magnitude_fun_complexOf_int16_t(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

static inline complexOf_int32_t signum_fun_complexOf_int32_t(complexOf_int32_t a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        int32_t m = magnitude_fun_complexOf_int32_t(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

static inline complexOf_int64_t signum_fun_complexOf_int64_t(complexOf_int64_t a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        int64_t m = magnitude_fun_complexOf_int64_t(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

static inline complexOf_uint8_t signum_fun_complexOf_uint8_t(complexOf_uint8_t a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        uint8_t m = magnitude_fun_complexOf_uint8_t(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

static inline complexOf_uint16_t signum_fun_complexOf_uint16_t(complexOf_uint16_t a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        uint16_t m = magnitude_fun_complexOf_uint16_t(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

static inline complexOf_uint32_t signum_fun_complexOf_uint32_t(complexOf_uint32_t a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        uint32_t m = magnitude_fun_complexOf_uint32_t(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

static inline complexOf_uint64_t signum_fun_complexOf_uint64_t(complexOf_uint64_t a) {
    if (a.re == 0 && a.im == 0)
        return a;
    else {
        uint64_t m = magnitude_fun_complexOf_uint64_t(a);
        a.re = a.re / m;
        a.im = a.im / m;
        return a;
    }
}

static inline float complex signum_fun_complexOf_float(float complex a) {
    if (a == 0)
        return a;
    else {
        float m = cabsf(a);
        return crealf(a) / m + cimagf(a) / m * I;
    }
}

static inline double complex signum_fun_complexOf_double(double complex a) {
    if (a == 0)
        return a;
    else {
        double m = cabs(a);
        return creal(a) / m + cimag(a) / m * I;
    }
}

static inline complexOf_int8_t add_fun_complexOf_int8_t(complexOf_int8_t a, complexOf_int8_t b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

static inline complexOf_int16_t add_fun_complexOf_int16_t(complexOf_int16_t a, complexOf_int16_t b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

static inline complexOf_int32_t add_fun_complexOf_int32_t(complexOf_int32_t a, complexOf_int32_t b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

static inline complexOf_int64_t add_fun_complexOf_int64_t(complexOf_int64_t a, complexOf_int64_t b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

static inline complexOf_uint8_t add_fun_complexOf_uint8_t(complexOf_uint8_t a, complexOf_uint8_t b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

static inline complexOf_uint16_t add_fun_complexOf_uint16_t(complexOf_uint16_t a, complexOf_uint16_t b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

static inline complexOf_uint32_t add_fun_complexOf_uint32_t(complexOf_uint32_t a, complexOf_uint32_t b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

static inline complexOf_uint64_t add_fun_complexOf_uint64_t(complexOf_uint64_t a, complexOf_uint64_t b) {
    a.re = a.re + b.re;
    a.im = a.im + b.im;
    return a;
}

static inline complexOf_int8_t sub_fun_complexOf_int8_t(complexOf_int8_t a, complexOf_int8_t b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

static inline complexOf_int16_t sub_fun_complexOf_int16_t(complexOf_int16_t a, complexOf_int16_t b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

static inline complexOf_int32_t sub_fun_complexOf_int32_t(complexOf_int32_t a, complexOf_int32_t b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

static inline complexOf_int64_t sub_fun_complexOf_int64_t(complexOf_int64_t a, complexOf_int64_t b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

static inline complexOf_uint8_t sub_fun_complexOf_uint8_t(complexOf_uint8_t a, complexOf_uint8_t b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

static inline complexOf_uint16_t sub_fun_complexOf_uint16_t(complexOf_uint16_t a, complexOf_uint16_t b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

static inline complexOf_uint32_t sub_fun_complexOf_uint32_t(complexOf_uint32_t a, complexOf_uint32_t b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

static inline complexOf_uint64_t sub_fun_complexOf_uint64_t(complexOf_uint64_t a, complexOf_uint64_t b) {
    a.re = a.re - b.re;
    a.im = a.im - b.im;
    return a;
}

static inline complexOf_int8_t mult_fun_complexOf_int8_t(complexOf_int8_t a, complexOf_int8_t b) {
    complexOf_int8_t r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

static inline complexOf_int16_t mult_fun_complexOf_int16_t(complexOf_int16_t a, complexOf_int16_t b) {
    complexOf_int16_t r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

static inline complexOf_int32_t mult_fun_complexOf_int32_t(complexOf_int32_t a, complexOf_int32_t b) {
    complexOf_int32_t r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

static inline complexOf_int64_t mult_fun_complexOf_int64_t(complexOf_int64_t a, complexOf_int64_t b) {
    complexOf_int64_t r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

static inline complexOf_uint8_t mult_fun_complexOf_uint8_t(complexOf_uint8_t a, complexOf_uint8_t b) {
    complexOf_uint8_t r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

static inline complexOf_uint16_t mult_fun_complexOf_uint16_t(complexOf_uint16_t a, complexOf_uint16_t b) {
    complexOf_uint16_t r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

static inline complexOf_uint32_t mult_fun_complexOf_uint32_t(complexOf_uint32_t a, complexOf_uint32_t b) {
    complexOf_uint32_t r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

static inline complexOf_uint64_t mult_fun_complexOf_uint64_t(complexOf_uint64_t a, complexOf_uint64_t b) {
    complexOf_uint64_t r;
    r.re = a.re * b.re - a.im * b.im;
    r.im = a.im * b.re + a.re * b.im;
    return r;
}

#ifdef __FreeBSD__
static inline float complex clogf(float complex x) {
    return (I * atan2f(cimagf(x), crealf(x))) + logf(cabsf(x));
}
#endif


static inline float complex logBase_fun_complexOf_float(float complex a, float complex b) {
    return clogf(b) / clogf(a);
}

static inline double complex logBase_fun_complexOf_double(double complex a, double complex b) {
    return clog(b) / clog(a);
}

static inline complexOf_int8_t complex_fun_int8_t(int8_t re, int8_t im) {
    complexOf_int8_t r;
    r.re = re;
    r.im = im;
    return r;
}

static inline complexOf_int16_t complex_fun_int16_t(int16_t re, int16_t im) {
    complexOf_int16_t r;
    r.re = re;
    r.im = im;
    return r;
}

static inline complexOf_int32_t complex_fun_int32_t(int32_t re, int32_t im) {
    complexOf_int32_t r;
    r.re = re;
    r.im = im;
    return r;
}

static inline complexOf_int64_t complex_fun_int64_t(int64_t re, int64_t im) {
    complexOf_int64_t r;
    r.re = re;
    r.im = im;
    return r;
}

static inline complexOf_uint8_t complex_fun_uint8_t(uint8_t re, uint8_t im) {
    complexOf_uint8_t r;
    r.re = re;
    r.im = im;
    return r;
}

static inline complexOf_uint16_t complex_fun_uint16_t(uint16_t re, uint16_t im) {
    complexOf_uint16_t r;
    r.re = re;
    r.im = im;
    return r;
}

static inline complexOf_uint32_t complex_fun_uint32_t(uint32_t re, uint32_t im) {
    complexOf_uint32_t r;
    r.re = re;
    r.im = im;
    return r;
}

static inline complexOf_uint64_t complex_fun_uint64_t(uint64_t re, uint64_t im) {
    complexOf_uint64_t r;
    r.re = re;
    r.im = im;
    return r;
}

static inline float complex complex_fun_float(float re, float im) {
    return re + im * I;
}

static inline double complex complex_fun_double(double re, double im) {
    return re + im * I;
}

static inline int8_t creal_fun_complexOf_int8_t(complexOf_int8_t a) {
    return a.re;
}

static inline int16_t creal_fun_complexOf_int16_t(complexOf_int16_t a) {
    return a.re;
}

static inline int32_t creal_fun_complexOf_int32_t(complexOf_int32_t a) {
    return a.re;
}

static inline int64_t creal_fun_complexOf_int64_t(complexOf_int64_t a) {
    return a.re;
}

static inline uint8_t creal_fun_complexOf_uint8_t(complexOf_uint8_t a) {
    return a.re;
}

static inline uint16_t creal_fun_complexOf_uint16_t(complexOf_uint16_t a) {
    return a.re;
}

static inline uint32_t creal_fun_complexOf_uint32_t(complexOf_uint32_t a) {
    return a.re;
}

static inline uint64_t creal_fun_complexOf_uint64_t(complexOf_uint64_t a) {
    return a.re;
}

static inline int8_t cimag_fun_complexOf_int8_t(complexOf_int8_t a) {
    return a.im;
}

static inline int16_t cimag_fun_complexOf_int16_t(complexOf_int16_t a) {
    return a.im;
}

static inline int32_t cimag_fun_complexOf_int32_t(complexOf_int32_t a) {
    return a.im;
}

static inline int64_t cimag_fun_complexOf_int64_t(complexOf_int64_t a) {
    return a.im;
}

static inline uint8_t cimag_fun_complexOf_uint8_t(complexOf_uint8_t a) {
    return a.im;
}

static inline uint16_t cimag_fun_complexOf_uint16_t(complexOf_uint16_t a) {
    return a.im;
}

static inline uint32_t cimag_fun_complexOf_uint32_t(complexOf_uint32_t a) {
    return a.im;
}

static inline uint64_t cimag_fun_complexOf_uint64_t(complexOf_uint64_t a) {
    return a.im;
}

static inline complexOf_int8_t conj_fun_complexOf_int8_t(complexOf_int8_t a) {
    a.im = -a.im;
    return a;
}

static inline complexOf_int16_t conj_fun_complexOf_int16_t(complexOf_int16_t a) {
    a.im = -a.im;
    return a;
}

static inline complexOf_int32_t conj_fun_complexOf_int32_t(complexOf_int32_t a) {
    a.im = -a.im;
    return a;
}

static inline complexOf_int64_t conj_fun_complexOf_int64_t(complexOf_int64_t a) {
    a.im = -a.im;
    return a;
}

static inline complexOf_uint8_t conj_fun_complexOf_uint8_t(complexOf_uint8_t a) {
    a.im = -a.im;
    return a;
}

static inline complexOf_uint16_t conj_fun_complexOf_uint16_t(complexOf_uint16_t a) {
    a.im = -a.im;
    return a;
}

static inline complexOf_uint32_t conj_fun_complexOf_uint32_t(complexOf_uint32_t a) {
    a.im = -a.im;
    return a;
}

static inline complexOf_uint64_t conj_fun_complexOf_uint64_t(complexOf_uint64_t a) {
    a.im = -a.im;
    return a;
}

static inline int8_t magnitude_fun_complexOf_int8_t(complexOf_int8_t a) {
    return lroundf(hypotf(a.re, a.im));
}

static inline int16_t magnitude_fun_complexOf_int16_t(complexOf_int16_t a) {
    return lroundf(hypotf(a.re, a.im));
}

static inline int32_t magnitude_fun_complexOf_int32_t(complexOf_int32_t a) {
    return lroundf(hypotf(a.re, a.im));
}

static inline int64_t magnitude_fun_complexOf_int64_t(complexOf_int64_t a) {
    return llroundf(hypotf(a.re, a.im));
}

static inline uint8_t magnitude_fun_complexOf_uint8_t(complexOf_uint8_t a) {
    return lroundf(hypotf(a.re, a.im));
}

static inline uint16_t magnitude_fun_complexOf_uint16_t(complexOf_uint16_t a) {
    return lroundf(hypotf(a.re, a.im));
}

static inline uint32_t magnitude_fun_complexOf_uint32_t(complexOf_uint32_t a) {
    return lroundf(hypotf(a.re, a.im));
}

static inline uint64_t magnitude_fun_complexOf_uint64_t(complexOf_uint64_t a) {
    return llroundf(hypotf(a.re, a.im));
}

static inline int8_t phase_fun_complexOf_int8_t(complexOf_int8_t a) {
    if (a.re == 0 && a.im == 0) return 0;
    return lroundf(atan2f(a.im, a.re));
}

static inline int16_t phase_fun_complexOf_int16_t(complexOf_int16_t a) {
    if (a.re == 0 && a.im == 0) return 0;
    return lroundf(atan2f(a.im, a.re));
}

static inline int32_t phase_fun_complexOf_int32_t(complexOf_int32_t a) {
    if (a.re == 0 && a.im == 0) return 0;
    return lroundf(atan2f(a.im, a.re));
}

static inline int64_t phase_fun_complexOf_int64_t(complexOf_int64_t a) {
    if (a.re == 0 && a.im == 0) return 0;
    return llroundf(atan2f(a.im, a.re));
}

static inline uint8_t phase_fun_complexOf_uint8_t(complexOf_uint8_t a) {
    if (a.re == 0 && a.im == 0) return 0;
    return lroundf(atan2f(a.im, a.re));
}

static inline uint16_t phase_fun_complexOf_uint16_t(complexOf_uint16_t a) {
    if (a.re == 0 && a.im == 0) return 0;
    return lroundf(atan2f(a.im, a.re));
}

static inline uint32_t phase_fun_complexOf_uint32_t(complexOf_uint32_t a) {
    if (a.re == 0 && a.im == 0) return 0;
    return lroundf(atan2f(a.im, a.re));
}

static inline uint64_t phase_fun_complexOf_uint64_t(complexOf_uint64_t a) {
    if (a.re == 0 && a.im == 0) return 0;
    return llroundf(atan2f(a.im, a.re));
}

static inline complexOf_int8_t mkPolar_fun_int8_t(int8_t r, int8_t t) {
    complexOf_int8_t a;
    a.re = lroundf(r * cosf(t));
    a.im = lroundf(r * sinf(t));
    return a;
}

static inline complexOf_int16_t mkPolar_fun_int16_t(int16_t r, int16_t t) {
    complexOf_int16_t a;
    a.re = lroundf(r * cosf(t));
    a.im = lroundf(r * sinf(t));
    return a;
}

static inline complexOf_int32_t mkPolar_fun_int32_t(int32_t r, int32_t t) {
    complexOf_int32_t a;
    a.re = lroundf(r * cosf(t));
    a.im = lroundf(r * sinf(t));
    return a;
}

static inline complexOf_int64_t mkPolar_fun_int64_t(int64_t r, int64_t t) {
    complexOf_int64_t a;
    a.re = llroundf(r * cosf(t));
    a.im = llroundf(r * sinf(t));
    return a;
}

static inline complexOf_uint8_t mkPolar_fun_uint8_t(uint8_t r, uint8_t t) {
    complexOf_uint8_t a;
    a.re = lroundf(r * cosf(t));
    a.im = lroundf(r * sinf(t));
    return a;
}

static inline complexOf_uint16_t mkPolar_fun_uint16_t(uint16_t r, uint16_t t) {
    complexOf_uint16_t a;
    a.re = lroundf(r * cosf(t));
    a.im = lroundf(r * sinf(t));
    return a;
}

static inline complexOf_uint32_t mkPolar_fun_uint32_t(uint32_t r, uint32_t t) {
    complexOf_uint32_t a;
    a.re = lroundf(r * cosf(t));
    a.im = lroundf(r * sinf(t));
    return a;
}

static inline complexOf_uint64_t mkPolar_fun_uint64_t(uint64_t r, uint64_t t) {
    complexOf_uint64_t a;
    a.re = llroundf(r * cosf(t));
    a.im = llroundf(r * sinf(t));
    return a;
}

static inline float complex mkPolar_fun_float(float r, float t) {
    return r * cosf(t) + r * sinf(t) * I;
}

static inline double complex mkPolar_fun_double(double r, double t) {
    return r * cos(t) + r * sin(t) * I;
}

static inline complexOf_int8_t cis_fun_int8_t(int8_t t) {
    complexOf_int8_t r;
    r.re = lroundf(cosf(t));
    r.im = lroundf(sinf(t));
    return r;
}

static inline complexOf_int16_t cis_fun_int16_t(int16_t t) {
    complexOf_int16_t r;
    r.re = lroundf(cosf(t));
    r.im = lroundf(sinf(t));
    return r;
}

static inline complexOf_int32_t cis_fun_int32_t(int32_t t) {
    complexOf_int32_t r;
    r.re = lroundf(cosf(t));
    r.im = lroundf(sinf(t));
    return r;
}

static inline complexOf_int64_t cis_fun_int64_t(int64_t t) {
    complexOf_int64_t r;
    r.re = llroundf(cosf(t));
    r.im = llroundf(sinf(t));
    return r;
}

static inline complexOf_uint8_t cis_fun_uint8_t(uint8_t t) {
    complexOf_uint8_t r;
    r.re = lroundf(cosf(t));
    r.im = lroundf(sinf(t));
    return r;
}

static inline complexOf_uint16_t cis_fun_uint16_t(uint16_t t) {
    complexOf_uint16_t r;
    r.re = lroundf(cosf(t));
    r.im = lroundf(sinf(t));
    return r;
}

static inline complexOf_uint32_t cis_fun_uint32_t(uint32_t t) {
    complexOf_uint32_t r;
    r.re = lroundf(cosf(t));
    r.im = lroundf(sinf(t));
    return r;
}

static inline complexOf_uint64_t cis_fun_uint64_t(uint64_t t) {
    complexOf_uint64_t r;
    r.re = llroundf(cosf(t));
    r.im = llroundf(sinf(t));
    return r;
}

static inline float complex cis_fun_float(float t) {
    return cosf(t) + sinf(t) * I;
}

static inline double complex cis_fun_double(double t) {
    return cos(t) + sin(t) * I;
}

#endif /* FELDSPAR_C99_H */
