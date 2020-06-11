#ifndef TESTS_IVARTEST_H
#define TESTS_IVARTEST_H

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


void task_core0(uint32_t v0, struct ivar e2);

void task0(void * params);

void ivartest(uint32_t v0, uint32_t * out);

#endif // TESTS_IVARTEST_H
