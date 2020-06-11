#ifndef TESTS_COMPLEXWHILECOND_H
#define TESTS_COMPLEXWHILECOND_H

#include "feldspar_c99.h"
#include "feldspar_array.h"
#include "feldspar_future.h"
#include "ivar.h"
#include "taskpool.h"
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>


struct s_2_signedS32_signedS32
{
  int32_t member1;
  int32_t member2;
};

void complexWhileCond(int32_t v0, struct s_2_signedS32_signedS32 * out);

#endif // TESTS_COMPLEXWHILECOND_H
