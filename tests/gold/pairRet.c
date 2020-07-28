#include "pairRet.h"


void pairRet(uint32_t v1, struct s_2_unsignedS32_unsignedS32 * out)
{
  if ((v1 > 3))
  {
    (*out).member1 = 3;
    (*out).member2 = 9;
  }
  else
  {
    (*out).member1 = 7;
    (*out).member2 = 5;
  }
}
