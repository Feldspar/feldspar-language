#ifndef TMP2_CONCATVM_H
#define TMP2_CONCATVM_H

#include "feldspar_c99.h"

struct awl_signedS32
{
  global int32_t * buffer;
  uint32_t length;
};

struct awl_awl_signedS32
{
  global struct awl_signedS32 * buffer;
  uint32_t length;
};

void concatVM(struct awl_awl_signedS32 * v1, struct awl_signedS32 * out);

#endif // TMP2_CONCATVM_H
