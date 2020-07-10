#ifndef TESTS_ARRAYINSTRUCTINSTRUCT_H
#define TESTS_ARRAYINSTRUCTINSTRUCT_H

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

struct s_2_unsignedS32_s_2_unsignedS32_awl_unsignedS32
{
  uint32_t member1;
  struct s_2_unsignedS32_awl_unsignedS32 member2;
};

void arrayInStructInStruct(struct s_2_unsignedS32_s_2_unsignedS32_awl_unsignedS32 * v0, struct s_2_unsignedS32_s_2_unsignedS32_awl_unsignedS32 * out);

#endif // TESTS_ARRAYINSTRUCTINSTRUCT_H
