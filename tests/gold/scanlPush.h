#ifndef TMP2_SCANLPUSH_H
#define TMP2_SCANLPUSH_H

#include "feldspar_c99.h"

struct awl_unsignedS32
{
  global uint32_t * buffer;
  uint32_t length;
};

struct awl_awl_unsignedS32
{
  global struct awl_unsignedS32 * buffer;
  uint32_t length;
};

global struct awl_unsignedS32 * initArray_awl_unsignedS32(global struct awl_unsignedS32 * dst, uint32_t oldLen, uint32_t newLen);

void freeArray_awl_unsignedS32(global struct awl_unsignedS32 * src, int32_t srcLen);

void scanlPush(struct awl_unsignedS32 * v0, struct awl_unsignedS32 * v1, struct awl_awl_unsignedS32 * out);

#endif // TMP2_SCANLPUSH_H
