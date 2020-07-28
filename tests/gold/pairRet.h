#ifndef TESTS_PAIRRET_H
#define TESTS_PAIRRET_H

#include "feldspar_c99.h"

struct s_2_unsignedS32_unsignedS32
{
  uint32_t member1;
  uint32_t member2;
};

void pairRet(uint32_t v1, struct s_2_unsignedS32_unsignedS32 * out);

#endif // TESTS_PAIRRET_H
