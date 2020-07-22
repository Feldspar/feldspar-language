#include "complexWhileCond.h"


void complexWhileCond(uint32_t v0, struct s_2_unsignedS32_unsignedS32 * out)
{
  struct s_2_unsignedS32_unsignedS32 e0 = { 0 };
  struct s_2_unsignedS32_unsignedS32 v9 = { 0 };
  uint32_t v4;
  uint32_t v6;
  bool v2;
  
  (e0).member1 = 0;
  (e0).member2 = v0;
  v4 = (e0).member1;
  v6 = ((e0).member2 - v4);
  v2 = ((v4 * v4) < (v6 * v6));
  while (v2)
  {
    (v9).member1 = ((e0).member1 + 1);
    (v9).member2 = (e0).member2;
    e0 = v9;
    v4 = (e0).member1;
    v6 = ((e0).member2 - v4);
    v2 = ((v4 * v4) < (v6 * v6));
  }
  *out = e0;
}
