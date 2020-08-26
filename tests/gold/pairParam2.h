#ifndef TMP2_PAIRPARAM2_H
#define TMP2_PAIRPARAM2_H

#include "feldspar_c99.h"

struct s_2_2xsignedS16
{
  int16_t member1;
  int16_t member2;
};

struct s_2_2xs_2_2xsignedS16
{
  struct s_2_2xsignedS16 member1;
  struct s_2_2xsignedS16 member2;
};

void pairParam2(struct s_2_2xsignedS16 * v0, struct s_2_2xs_2_2xsignedS16 * out);

#endif // TMP2_PAIRPARAM2_H
