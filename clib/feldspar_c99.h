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

#include <stdint.h>
#include <complex.h>


int feldspar_c99_hook(void);

#define min(X, Y)  ((X) < (Y) ? (X) : (Y))
#define max(X, Y)  ((X) > (Y) ? (X) : (Y))

int8_t pow_fun_int8_t(int8_t, int8_t);
int16_t pow_fun_int16_t(int16_t, int16_t);
int32_t pow_fun_int32_t(int32_t, int32_t);
int64_t pow_fun_int64_t(int64_t, int64_t);
uint8_t pow_fun_uint8_t(uint8_t, uint8_t);
uint16_t pow_fun_uint16_t(uint16_t, uint16_t);
uint32_t pow_fun_uint32_t(uint32_t, uint32_t);
uint64_t pow_fun_uint64_t(uint64_t, uint64_t);

int8_t abs_fun_int8_t(int8_t);
int16_t abs_fun_int16_t(int16_t);
int32_t abs_fun_int32_t(int32_t);
int64_t abs_fun_int64_t(int64_t);
float abs_fun_float(float);
double abs_fun_double(double);

int8_t signum_fun_int8_t(int8_t);
int16_t signum_fun_int16_t(int16_t);
int32_t signum_fun_int32_t(int32_t);
int64_t signum_fun_int64_t(int64_t);
uint8_t signum_fun_uint8_t(uint8_t);
uint16_t signum_fun_uint16_t(uint16_t);
uint32_t signum_fun_uint32_t(uint32_t);
uint64_t signum_fun_uint64_t(uint64_t);
float signum_fun_float(float);
double signum_fun_double(double);

float logBase_fun_float(float, float);
double logBase_fun_double(double, double);



int8_t setBit_fun_int8_t(int8_t, uint32_t);
int16_t setBit_fun_int16_t(int16_t, uint32_t);
int32_t setBit_fun_int32_t(int32_t, uint32_t);
int64_t setBit_fun_int64_t(int64_t, uint32_t);
uint8_t setBit_fun_uint8_t(uint8_t, uint32_t);
uint16_t setBit_fun_uint16_t(uint16_t, uint32_t);
uint32_t setBit_fun_uint32_t(uint32_t, uint32_t);
uint64_t setBit_fun_uint64_t(uint64_t, uint32_t);

int8_t clearBit_fun_int8_t(int8_t, uint32_t);
int16_t clearBit_fun_int16_t(int16_t, uint32_t);
int32_t clearBit_fun_int32_t(int32_t, uint32_t);
int64_t clearBit_fun_int64_t(int64_t, uint32_t);
uint8_t clearBit_fun_uint8_t(uint8_t, uint32_t);
uint16_t clearBit_fun_uint16_t(uint16_t, uint32_t);
uint32_t clearBit_fun_uint32_t(uint32_t, uint32_t);
uint64_t clearBit_fun_uint64_t(uint64_t, uint32_t);

int8_t complementBit_fun_int8_t(int8_t, uint32_t);
int16_t complementBit_fun_int16_t(int16_t, uint32_t);
int32_t complementBit_fun_int32_t(int32_t, uint32_t);
int64_t complementBit_fun_int64_t(int64_t, uint32_t);
uint8_t complementBit_fun_uint8_t(uint8_t, uint32_t);
uint16_t complementBit_fun_uint16_t(uint16_t, uint32_t);
uint32_t complementBit_fun_uint32_t(uint32_t, uint32_t);
uint64_t complementBit_fun_uint64_t(uint64_t, uint32_t);

int testBit_fun_int8_t(int8_t, uint32_t);
int testBit_fun_int16_t(int16_t, uint32_t);
int testBit_fun_int32_t(int32_t, uint32_t);
int testBit_fun_int64_t(int64_t, uint32_t);
int testBit_fun_uint8_t(uint8_t, uint32_t);
int testBit_fun_uint16_t(uint16_t, uint32_t);
int testBit_fun_uint32_t(uint32_t, uint32_t);
int testBit_fun_uint64_t(uint64_t, uint32_t);

int8_t rotateL_fun_int8_t(int8_t, int32_t);
int16_t rotateL_fun_int16_t(int16_t, int32_t);
int32_t rotateL_fun_int32_t(int32_t, int32_t);
int64_t rotateL_fun_int64_t(int64_t, int32_t);
uint8_t rotateL_fun_uint8_t(uint8_t, int32_t);
uint16_t rotateL_fun_uint16_t(uint16_t, int32_t);
uint32_t rotateL_fun_uint32_t(uint32_t, int32_t);
uint64_t rotateL_fun_uint64_t(uint64_t, int32_t);

int8_t rotateR_fun_int8_t(int8_t, int32_t);
int16_t rotateR_fun_int16_t(int16_t, int32_t);
int32_t rotateR_fun_int32_t(int32_t, int32_t);
int64_t rotateR_fun_int64_t(int64_t, int32_t);
uint8_t rotateR_fun_uint8_t(uint8_t, int32_t);
uint16_t rotateR_fun_uint16_t(uint16_t, int32_t);
uint32_t rotateR_fun_uint32_t(uint32_t, int32_t);
uint64_t rotateR_fun_uint64_t(uint64_t, int32_t);

int8_t reverseBits_fun_int8_t(int8_t);
int16_t reverseBits_fun_int16_t(int16_t);
int32_t reverseBits_fun_int32_t(int32_t);
int64_t reverseBits_fun_int64_t(int64_t);
uint8_t reverseBits_fun_uint8_t(uint8_t);
uint16_t reverseBits_fun_uint16_t(uint16_t);
uint32_t reverseBits_fun_uint32_t(uint32_t);
uint64_t reverseBits_fun_uint64_t(uint64_t);

uint32_t bitScan_fun_int8_t(int8_t);
uint32_t bitScan_fun_int16_t(int16_t);
uint32_t bitScan_fun_int32_t(int32_t);
uint32_t bitScan_fun_int64_t(int64_t);
uint32_t bitScan_fun_uint8_t(uint8_t);
uint32_t bitScan_fun_uint16_t(uint16_t);
uint32_t bitScan_fun_uint32_t(uint32_t);
uint32_t bitScan_fun_uint64_t(uint64_t);

uint32_t bitCount_fun_int8_t(int8_t);
uint32_t bitCount_fun_int16_t(int16_t);
uint32_t bitCount_fun_int32_t(int32_t);
uint32_t bitCount_fun_int64_t(int64_t);
uint32_t bitCount_fun_uint8_t(uint8_t);
uint32_t bitCount_fun_uint16_t(uint16_t);
uint32_t bitCount_fun_uint32_t(uint32_t);
uint32_t bitCount_fun_uint64_t(uint64_t);

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

int equal_fun_complexOf_int8_t(complexOf_int8_t, complexOf_int8_t);
int equal_fun_complexOf_int16_t(complexOf_int16_t, complexOf_int16_t);
int equal_fun_complexOf_int32_t(complexOf_int32_t, complexOf_int32_t);
int equal_fun_complexOf_int64_t(complexOf_int64_t, complexOf_int64_t);
int equal_fun_complexOf_uint8_t(complexOf_uint8_t, complexOf_uint8_t);
int equal_fun_complexOf_uint16_t(complexOf_uint16_t, complexOf_uint16_t);
int equal_fun_complexOf_uint32_t(complexOf_uint32_t, complexOf_uint32_t);
int equal_fun_complexOf_uint64_t(complexOf_uint64_t, complexOf_uint64_t);

complexOf_int8_t negate_fun_complexOf_int8_t(complexOf_int8_t);
complexOf_int16_t negate_fun_complexOf_int16_t(complexOf_int16_t);
complexOf_int32_t negate_fun_complexOf_int32_t(complexOf_int32_t);
complexOf_int64_t negate_fun_complexOf_int64_t(complexOf_int64_t);
complexOf_uint8_t negate_fun_complexOf_uint8_t(complexOf_uint8_t);
complexOf_uint16_t negate_fun_complexOf_uint16_t(complexOf_uint16_t);
complexOf_uint32_t negate_fun_complexOf_uint32_t(complexOf_uint32_t);
complexOf_uint64_t negate_fun_complexOf_uint64_t(complexOf_uint64_t);

complexOf_int8_t abs_fun_complexOf_int8_t(complexOf_int8_t);
complexOf_int16_t abs_fun_complexOf_int16_t(complexOf_int16_t);
complexOf_int32_t abs_fun_complexOf_int32_t(complexOf_int32_t);
complexOf_int64_t abs_fun_complexOf_int64_t(complexOf_int64_t);
complexOf_uint8_t abs_fun_complexOf_uint8_t(complexOf_uint8_t);
complexOf_uint16_t abs_fun_complexOf_uint16_t(complexOf_uint16_t);
complexOf_uint32_t abs_fun_complexOf_uint32_t(complexOf_uint32_t);
complexOf_uint64_t abs_fun_complexOf_uint64_t(complexOf_uint64_t);

complexOf_int8_t signum_fun_complexOf_int8_t(complexOf_int8_t);
complexOf_int16_t signum_fun_complexOf_int16_t(complexOf_int16_t);
complexOf_int32_t signum_fun_complexOf_int32_t(complexOf_int32_t);
complexOf_int64_t signum_fun_complexOf_int64_t(complexOf_int64_t);
complexOf_uint8_t signum_fun_complexOf_uint8_t(complexOf_uint8_t);
complexOf_uint16_t signum_fun_complexOf_uint16_t(complexOf_uint16_t);
complexOf_uint32_t signum_fun_complexOf_uint32_t(complexOf_uint32_t);
complexOf_uint64_t signum_fun_complexOf_uint64_t(complexOf_uint64_t);
float complex signum_fun_complexOf_float(float complex);
double complex signum_fun_complexOf_double(double complex);

complexOf_int8_t add_fun_complexOf_int8_t(complexOf_int8_t, complexOf_int8_t);
complexOf_int16_t add_fun_complexOf_int16_t(complexOf_int16_t, complexOf_int16_t);
complexOf_int32_t add_fun_complexOf_int32_t(complexOf_int32_t, complexOf_int32_t);
complexOf_int64_t add_fun_complexOf_int64_t(complexOf_int64_t, complexOf_int64_t);
complexOf_uint8_t add_fun_complexOf_uint8_t(complexOf_uint8_t, complexOf_uint8_t);
complexOf_uint16_t add_fun_complexOf_uint16_t(complexOf_uint16_t, complexOf_uint16_t);
complexOf_uint32_t add_fun_complexOf_uint32_t(complexOf_uint32_t, complexOf_uint32_t);
complexOf_uint64_t add_fun_complexOf_uint64_t(complexOf_uint64_t, complexOf_uint64_t);

complexOf_int8_t sub_fun_complexOf_int8_t(complexOf_int8_t, complexOf_int8_t);
complexOf_int16_t sub_fun_complexOf_int16_t(complexOf_int16_t, complexOf_int16_t);
complexOf_int32_t sub_fun_complexOf_int32_t(complexOf_int32_t, complexOf_int32_t);
complexOf_int64_t sub_fun_complexOf_int64_t(complexOf_int64_t, complexOf_int64_t);
complexOf_uint8_t sub_fun_complexOf_uint8_t(complexOf_uint8_t, complexOf_uint8_t);
complexOf_uint16_t sub_fun_complexOf_uint16_t(complexOf_uint16_t, complexOf_uint16_t);
complexOf_uint32_t sub_fun_complexOf_uint32_t(complexOf_uint32_t, complexOf_uint32_t);
complexOf_uint64_t sub_fun_complexOf_uint64_t(complexOf_uint64_t, complexOf_uint64_t);

complexOf_int8_t mult_fun_complexOf_int8_t(complexOf_int8_t, complexOf_int8_t);
complexOf_int16_t mult_fun_complexOf_int16_t(complexOf_int16_t, complexOf_int16_t);
complexOf_int32_t mult_fun_complexOf_int32_t(complexOf_int32_t, complexOf_int32_t);
complexOf_int64_t mult_fun_complexOf_int64_t(complexOf_int64_t, complexOf_int64_t);
complexOf_uint8_t mult_fun_complexOf_uint8_t(complexOf_uint8_t, complexOf_uint8_t);
complexOf_uint16_t mult_fun_complexOf_uint16_t(complexOf_uint16_t, complexOf_uint16_t);
complexOf_uint32_t mult_fun_complexOf_uint32_t(complexOf_uint32_t, complexOf_uint32_t);
complexOf_uint64_t mult_fun_complexOf_uint64_t(complexOf_uint64_t, complexOf_uint64_t);

float complex logBase_fun_complexOf_float(float complex, float complex);
double complex logBase_fun_complexOf_double(double complex, double complex);

complexOf_int8_t complex_fun_int8_t(int8_t, int8_t);
complexOf_int16_t complex_fun_int16_t(int16_t, int16_t);
complexOf_int32_t complex_fun_int32_t(int32_t, int32_t);
complexOf_int64_t complex_fun_int64_t(int64_t, int64_t);
complexOf_uint8_t complex_fun_uint8_t(uint8_t, uint8_t);
complexOf_uint16_t complex_fun_uint16_t(uint16_t, uint16_t);
complexOf_uint32_t complex_fun_uint32_t(uint32_t, uint32_t);
complexOf_uint64_t complex_fun_uint64_t(uint64_t, uint64_t);
float complex complex_fun_float(float, float);
double complex complex_fun_double(double, double);

int8_t creal_fun_complexOf_int8_t(complexOf_int8_t);
int16_t creal_fun_complexOf_int16_t(complexOf_int16_t);
int32_t creal_fun_complexOf_int32_t(complexOf_int32_t);
int64_t creal_fun_complexOf_int64_t(complexOf_int64_t);
uint8_t creal_fun_complexOf_uint8_t(complexOf_uint8_t);
uint16_t creal_fun_complexOf_uint16_t(complexOf_uint16_t);
uint32_t creal_fun_complexOf_uint32_t(complexOf_uint32_t);
uint64_t creal_fun_complexOf_uint64_t(complexOf_uint64_t);

int8_t cimag_fun_complexOf_int8_t(complexOf_int8_t);
int16_t cimag_fun_complexOf_int16_t(complexOf_int16_t);
int32_t cimag_fun_complexOf_int32_t(complexOf_int32_t);
int64_t cimag_fun_complexOf_int64_t(complexOf_int64_t);
uint8_t cimag_fun_complexOf_uint8_t(complexOf_uint8_t);
uint16_t cimag_fun_complexOf_uint16_t(complexOf_uint16_t);
uint32_t cimag_fun_complexOf_uint32_t(complexOf_uint32_t);
uint64_t cimag_fun_complexOf_uint64_t(complexOf_uint64_t);

complexOf_int8_t conj_fun_complexOf_int8_t(complexOf_int8_t);
complexOf_int16_t conj_fun_complexOf_int16_t(complexOf_int16_t);
complexOf_int32_t conj_fun_complexOf_int32_t(complexOf_int32_t);
complexOf_int64_t conj_fun_complexOf_int64_t(complexOf_int64_t);
complexOf_uint8_t conj_fun_complexOf_uint8_t(complexOf_uint8_t);
complexOf_uint16_t conj_fun_complexOf_uint16_t(complexOf_uint16_t);
complexOf_uint32_t conj_fun_complexOf_uint32_t(complexOf_uint32_t);
complexOf_uint64_t conj_fun_complexOf_uint64_t(complexOf_uint64_t);

int8_t magnitude_fun_complexOf_int8_t(complexOf_int8_t);
int16_t magnitude_fun_complexOf_int16_t(complexOf_int16_t);
int32_t magnitude_fun_complexOf_int32_t(complexOf_int32_t);
int64_t magnitude_fun_complexOf_int64_t(complexOf_int64_t);
uint8_t magnitude_fun_complexOf_uint8_t(complexOf_uint8_t);
uint16_t magnitude_fun_complexOf_uint16_t(complexOf_uint16_t);
uint32_t magnitude_fun_complexOf_uint32_t(complexOf_uint32_t);
uint64_t magnitude_fun_complexOf_uint64_t(complexOf_uint64_t);

int8_t phase_fun_complexOf_int8_t(complexOf_int8_t);
int16_t phase_fun_complexOf_int16_t(complexOf_int16_t);
int32_t phase_fun_complexOf_int32_t(complexOf_int32_t);
int64_t phase_fun_complexOf_int64_t(complexOf_int64_t);
uint8_t phase_fun_complexOf_uint8_t(complexOf_uint8_t);
uint16_t phase_fun_complexOf_uint16_t(complexOf_uint16_t);
uint32_t phase_fun_complexOf_uint32_t(complexOf_uint32_t);
uint64_t phase_fun_complexOf_uint64_t(complexOf_uint64_t);

complexOf_int8_t mkPolar_fun_int8_t(int8_t, int8_t);
complexOf_int16_t mkPolar_fun_int16_t(int16_t, int16_t);
complexOf_int32_t mkPolar_fun_int32_t(int32_t, int32_t);
complexOf_int64_t mkPolar_fun_int64_t(int64_t, int64_t);
complexOf_uint8_t mkPolar_fun_uint8_t(uint8_t, uint8_t);
complexOf_uint16_t mkPolar_fun_uint16_t(uint16_t, uint16_t);
complexOf_uint32_t mkPolar_fun_uint32_t(uint32_t, uint32_t);
complexOf_uint64_t mkPolar_fun_uint64_t(uint64_t, uint64_t);
float complex mkPolar_fun_float(float, float);
double complex mkPolar_fun_double(double, double);

complexOf_int8_t cis_fun_int8_t(int8_t);
complexOf_int16_t cis_fun_int16_t(int16_t);
complexOf_int32_t cis_fun_int32_t(int32_t);
complexOf_int64_t cis_fun_int64_t(int64_t);
complexOf_uint8_t cis_fun_uint8_t(uint8_t);
complexOf_uint16_t cis_fun_uint16_t(uint16_t);
complexOf_uint32_t cis_fun_uint32_t(uint32_t);
complexOf_uint64_t cis_fun_uint64_t(uint64_t);
float complex cis_fun_float(float);
double complex cis_fun_double(double);

#endif /* FELDSPAR_C99_H */
