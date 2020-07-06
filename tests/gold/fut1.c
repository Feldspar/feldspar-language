#include "fut1.h"


void task_core0(struct ivar e0)
{
  int32_t e1;
  
  ivar_get(int32_t, &e1, e0);
  ivar_put(int32_t, e0, &e1);
}

void task0(void * params)
{
  run1(task_core0, struct ivar);
}

void fut1(struct ivar v0, struct ivar * out)
{
  struct ivar e0;
  
  taskpool_init(4, 4, 4);
  e0 = *out;
  e0 = v0;
  for (uint32_t v1 = 0; v1 < 20; v1 += 1)
  {
    ivar_init(&e0);
    spawn1(task0, struct ivar, e0);
  }
  *out = e0;
  taskpool_shutdown();
}
