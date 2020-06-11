#include "ivartest.h"


void task_core0(uint32_t v0, struct ivar e2)
{
  uint32_t e3;
  
  e3 = (v0 + 1);
  ivar_put(uint32_t, e2, &e3);
}

void task0(void * params)
{
  run2(task_core0, uint32_t, struct ivar);
}

void ivartest(uint32_t v0, uint32_t * out)
{
  uint32_t e1;
  struct ivar e2;
  
  taskpool_init(4, 4, 4);
  ivar_init(&e2);
  spawn2(task0, uint32_t, v0, struct ivar, e2);
  ivar_get_nontask(uint32_t, &e1, e2);
  *out = (e1 << 1);
  taskpool_shutdown();
  ivar_destroy(&e2);
}
