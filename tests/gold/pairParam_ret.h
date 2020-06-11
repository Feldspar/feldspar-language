#ifndef TESTS_PAIRPARAM_RET_H
#define TESTS_PAIRPARAM_RET_H

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


struct s_2_unsignedS32_unsignedS32
{
  uint32_t member1;
  uint32_t member2;
};

uint32_t pairParam__ret(struct s_2_unsignedS32_unsignedS32 * v0);

#endif // TESTS_PAIRPARAM_RET_H
