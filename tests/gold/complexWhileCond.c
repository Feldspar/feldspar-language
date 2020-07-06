#include "complexWhileCond.h"


void complexWhileCond(int32_t v0, struct s_2_signedS32_signedS32 * out)
{
  struct s_2_signedS32_signedS32 e0 = { 0 };
  struct s_2_signedS32_signedS32 v9 = { 0 };
  int32_t v3;
  int32_t v5;
  bool v2;
  
  (e0).member1 = 0;
  (e0).member2 = v0;
  v3 = (e0).member1;
  v5 = ((e0).member2 - v3);
  v2 = ((v3 * v3) != (v5 * v5));
  while (v2)
  {
    (v9).member1 = ((e0).member1 + 1);
    (v9).member2 = (e0).member2;
    e0 = v9;
    v3 = (e0).member1;
    v5 = ((e0).member2 - v3);
    v2 = ((v3 * v3) != (v5 * v5));
  }
  *out = e0;
}
