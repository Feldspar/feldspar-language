#include "pairParam2.h"


void pairParam2(struct s_2_2xsignedS16 * v0, struct s_2_2xs_2_2xsignedS16 * out)
{
  ((*out).member1).member1 = (*v0).member1;
  ((*out).member1).member2 = (*v0).member2;
  ((*out).member2).member1 = (*v0).member1;
  ((*out).member2).member2 = (*v0).member2;
}
