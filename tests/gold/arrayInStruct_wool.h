#ifndef TMP2_ARRAYINSTRUCT_WOOL_H
#define TMP2_ARRAYINSTRUCT_WOOL_H

#include "feldspar_c99.h"

struct awl_unsignedS32
{
  uint32_t * buffer;
  uint32_t length;
};

struct s_2_unsignedS32_awl_unsignedS32
{
  uint32_t member1;
  struct awl_unsignedS32 member2;
};

void arrayInStruct__wool(struct awl_unsignedS32 * v0, struct awl_unsignedS32 * out);

#endif // TMP2_ARRAYINSTRUCT_WOOL_H
