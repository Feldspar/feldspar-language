#ifndef TESTS_CONCATV_H
#define TESTS_CONCATV_H

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

void concatV(struct awl_awl_signedS32 * v1, struct awl_signedS32 * out);

#endif // TESTS_CONCATV_H
