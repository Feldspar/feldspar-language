#include "ivartest2.h"


void task_core0(struct s_2_unsignedS32_unsignedS32 * v0, struct ivar e1)
{
  ivar_put(struct s_2_unsignedS32_unsignedS32, e1, &*v0);
}

void task0(void * params)
{
  run2(task_core0, struct s_2_unsignedS32_unsignedS32 *, struct ivar);
}

void ivartest2(struct s_2_unsignedS32_unsignedS32 * v0, struct s_2_unsignedS32_unsignedS32 * out)
{
  struct ivar e1;
  
  taskpool_init(4, 4, 4);
  ivar_init(&e1);
  spawn2(task0, struct s_2_unsignedS32_unsignedS32 *, v0, struct ivar, e1);
  ivar_get_nontask(struct s_2_unsignedS32_unsignedS32, &*out, e1);
  taskpool_shutdown();
  ivar_destroy(&e1);
}
