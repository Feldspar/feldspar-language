#ifndef TESTS_SCANLPUSH_H
#define TESTS_SCANLPUSH_H

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

struct awl_unsignedS32 * initArray_awl_unsignedS32(struct awl_unsignedS32 * dst, uint32_t oldLen, uint32_t newLen);

void freeArray_awl_unsignedS32(struct awl_unsignedS32 * src, int32_t srcLen);

void scanlPush(struct awl_unsignedS32 * v0, struct awl_unsignedS32 * v1, struct awl_awl_unsignedS32 * out);

#endif // TESTS_SCANLPUSH_H
