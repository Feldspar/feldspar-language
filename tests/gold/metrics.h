#ifndef TESTS_METRICS_H
#define TESTS_METRICS_H

#include "feldspar_c99.h"

struct awl_signedS32
{
  int32_t * buffer;
  uint32_t length;
};

struct awl_awl_signedS32
{
  struct awl_signedS32 * buffer;
  uint32_t length;
};

struct s_2_unsignedS32_unsignedS32
{
  uint32_t member1;
  uint32_t member2;
};

struct awl_s_2_unsignedS32_unsignedS32
{
  struct s_2_unsignedS32_unsignedS32 * buffer;
  uint32_t length;
};

struct awl_awl_s_2_unsignedS32_unsignedS32
{
  struct awl_s_2_unsignedS32_unsignedS32 * buffer;
  uint32_t length;
};

struct awl_signedS32 * initArray_awl_signedS32(struct awl_signedS32 * dst, uint32_t oldLen, uint32_t newLen);

void freeArray_awl_signedS32(struct awl_signedS32 * src, int32_t srcLen);

void metrics(struct awl_signedS32 * v1, struct awl_signedS32 * v2, struct awl_awl_s_2_unsignedS32_unsignedS32 * v3, struct awl_awl_signedS32 * out);

#endif // TESTS_METRICS_H
