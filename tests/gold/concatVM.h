#ifndef TESTS_CONCATVM_H
#define TESTS_CONCATVM_H

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


struct awl_signedS32
{
  int32_t * buffer;
  uint32_t length;
};

struct awl_awl_signedS32
{
  struct awl_signedS32 * buffer;
  uint32_t length;
};

void concatVM(struct awl_awl_signedS32 * v1, struct awl_signedS32 * out);

#endif // TESTS_CONCATVM_H
