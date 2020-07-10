#ifndef TESTS_IVARTEST2_H
#define TESTS_IVARTEST2_H

#include "feldspar_c99.h"


struct s_2_unsignedS32_unsignedS32
{
  uint32_t member1;
  uint32_t member2;
};

void task_core0(struct s_2_unsignedS32_unsignedS32 * v0, struct ivar e0);

void task0(void * params);

void ivartest2(struct s_2_unsignedS32_unsignedS32 * v0, struct s_2_unsignedS32_unsignedS32 * out);

#endif // TESTS_IVARTEST2_H
