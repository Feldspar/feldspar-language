#include "fut1.h"


void task_core0(struct ivar e3)
{
  int32_t e4;
  
  ivar_get(int32_t, &e4, e3);
  ivar_put(int32_t, e3, &e4);
}

void task0(void * params)
{
  run1(task_core0, struct ivar);
}

void fut1(struct ivar v0, struct ivar * out)
{
  struct ivar e3;
  
  taskpool_init(4, 4, 4);
  e3 = *out;
  e3 = v0;
  for (uint32_t v1 = 0; v1 < 20; v1 += 1)
  {
    ivar_init(&e3);
    spawn1(task0, struct ivar, e3);
  }
  *out = e3;
  taskpool_shutdown();
}
