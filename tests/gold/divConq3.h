#ifndef TMP2_DIVCONQ3_H
#define TMP2_DIVCONQ3_H

#include "feldspar_c99.h"

struct awl_i_awl_signedS32
{
  global struct ivar * buffer;
  uint32_t length;
};

struct awl_signedS32
{
  global int32_t * buffer;
  uint32_t length;
};

void task_core0(uint32_t v8, uint32_t v3, struct awl_signedS32 * v1, struct awl_i_awl_signedS32 v24);

void task0(void * params);

void divConq3(struct awl_signedS32 * v1, struct awl_signedS32 * out);

#endif // TMP2_DIVCONQ3_H
