#ifndef TMP2_PAIRRET_H
#define TMP2_PAIRRET_H

#include "feldspar_c99.h"

struct s_2_2xunsignedS32
{
  uint32_t member1;
  uint32_t member2;
};

void pairRet(uint32_t v1, struct s_2_2xunsignedS32 * out);

#endif // TMP2_PAIRRET_H
