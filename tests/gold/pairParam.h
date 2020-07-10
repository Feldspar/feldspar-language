#ifndef TESTS_PAIRPARAM_H
#define TESTS_PAIRPARAM_H

#include "feldspar_c99.h"


struct s_2_unsignedS32_unsignedS32
{
  uint32_t member1;
  uint32_t member2;
};

void pairParam(struct s_2_unsignedS32_unsignedS32 * v0, uint32_t * out);

#endif // TESTS_PAIRPARAM_H
