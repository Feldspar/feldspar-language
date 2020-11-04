#ifndef TMP2_ARRAYINSTRUCT_OPENMP_H
#define TMP2_ARRAYINSTRUCT_OPENMP_H

#include "feldspar_c99.h"

struct awl_unsignedS32
{
  global uint32_t * buffer;
  uint32_t length;
};

struct s_2_1xunsignedS32_1xawl_unsignedS32
{
  uint32_t member1;
  struct awl_unsignedS32 member2;
};

void arrayInStruct__openMP(struct awl_unsignedS32 * v0, struct awl_unsignedS32 * out);

#endif // TMP2_ARRAYINSTRUCT_OPENMP_H
