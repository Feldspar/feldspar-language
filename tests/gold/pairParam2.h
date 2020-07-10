#ifndef TESTS_PAIRPARAM2_H
#define TESTS_PAIRPARAM2_H

#include "feldspar_c99.h"

struct s_2_signedS16_signedS16
{
  int16_t member1;
  int16_t member2;
};

struct s_2_s_2_signedS16_signedS16_s_2_signedS16_signedS16
{
  struct s_2_signedS16_signedS16 member1;
  struct s_2_signedS16_signedS16 member2;
};

void pairParam2(struct s_2_signedS16_signedS16 * v0, struct s_2_s_2_signedS16_signedS16_s_2_signedS16_signedS16 * out);

#endif // TESTS_PAIRPARAM2_H
