#ifndef TESTS_DEEPARRAYCOPY_H
#define TESTS_DEEPARRAYCOPY_H

#include "feldspar_c99.h"
#include "feldspar_array.h"
#include "feldspar_future.h"
#include "ivar.h"
#include "taskpool.h"
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>


struct awl_unsignedS32
{
  uint32_t * buffer;
  uint32_t length;
};

struct awl_awl_unsignedS32
{
  struct awl_unsignedS32 * buffer;
  uint32_t length;
};

struct awl_awl_awl_unsignedS32
{
  struct awl_awl_unsignedS32 * buffer;
  uint32_t length;
};

struct s_2_awl_awl_awl_unsignedS32_awl_awl_awl_unsignedS32
{
  struct awl_awl_awl_unsignedS32 member1;
  struct awl_awl_awl_unsignedS32 member2;
};

struct awl_awl_unsignedS32 * copyArrayPos_awl_awl_unsignedS32(struct awl_awl_unsignedS32 * dst, int32_t dstLen, struct awl_awl_unsignedS32 * src, int32_t srcLen, int32_t pos);

struct awl_awl_unsignedS32 * copyArray_awl_awl_unsignedS32(struct awl_awl_unsignedS32 * dst, int32_t dstLen, struct awl_awl_unsignedS32 * src, int32_t srcLen);

struct awl_awl_unsignedS32 * initCopyArray_awl_awl_unsignedS32(struct awl_awl_unsignedS32 * dst, int32_t dstLen, struct awl_awl_unsignedS32 * src, int32_t srcLen);

struct awl_unsignedS32 * copyArrayPos_awl_unsignedS32(struct awl_unsignedS32 * dst, int32_t dstLen, struct awl_unsignedS32 * src, int32_t srcLen, int32_t pos);

struct awl_unsignedS32 * copyArray_awl_unsignedS32(struct awl_unsignedS32 * dst, int32_t dstLen, struct awl_unsignedS32 * src, int32_t srcLen);

struct awl_unsignedS32 * initCopyArray_awl_unsignedS32(struct awl_unsignedS32 * dst, int32_t dstLen, struct awl_unsignedS32 * src, int32_t srcLen);

struct awl_awl_unsignedS32 * initArray_awl_awl_unsignedS32(struct awl_awl_unsignedS32 * dst, uint32_t oldLen, uint32_t newLen);

void freeArray_awl_awl_unsignedS32(struct awl_awl_unsignedS32 * src, int32_t srcLen);

struct awl_unsignedS32 * initArray_awl_unsignedS32(struct awl_unsignedS32 * dst, uint32_t oldLen, uint32_t newLen);

void freeArray_awl_unsignedS32(struct awl_unsignedS32 * src, int32_t srcLen);

void deepArrayCopy(struct awl_awl_awl_unsignedS32 * v0, struct s_2_awl_awl_awl_unsignedS32_awl_awl_awl_unsignedS32 * out);

#endif // TESTS_DEEPARRAYCOPY_H
