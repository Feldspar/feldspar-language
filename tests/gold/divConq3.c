#include "divConq3.h"


void task_core0(uint32_t v8, uint32_t v3, struct awl_signedS32 * v1, struct awl_i_awl_signedS32 v24)
{
  uint32_t v9;
  uint32_t v12;
  struct awl_signedS32 e54 = { 0 };
  
  v9 = (v8 << 10);
  v12 = min(1024, (v3 - v9));
  (e54).buffer = initArray((e54).buffer, (e54).length, sizeof(int32_t), v12);
  (e54).length = v12;
  for (uint32_t v15 = 0; v15 < v12; v15 += 1)
  {
    (e54).buffer[v15] = ((*v1).buffer[(v15 + v9)] + 1);
  }
  ivar_put_array_shallow((v24).buffer[v8], &e54, sizeof(int32_t));
}

void task0(void * params)
{
  run4(task_core0, uint32_t, uint32_t, struct awl_signedS32 *, struct awl_i_awl_signedS32);
}

void divConq3(struct awl_signedS32 * v1, struct awl_signedS32 * out)
{
  uint32_t v3;
  uint32_t v4;
  struct awl_i_awl_signedS32 v24 = { 0 };
  struct awl_signedS32 v49 = { 0 };
  uint32_t len55;
  struct awl_signedS32 v28 = { 0 };
  uint32_t v34;
  struct awl_signedS32 v31 = { 0 };
  struct ivar e56;
  uint32_t v32;
  uint32_t len57;
  struct awl_signedS32 e58 = { 0 };
  uint32_t v50;
  
  taskpool_init(4, 4, 4);
  v3 = (*v1).length;
  v4 = (v3 >> 10);
  (v24).buffer = initArray((v24).buffer, (v24).length, sizeof(struct ivar), v4);
  (v24).length = v4;
  for (uint32_t v8 = 0; v8 < v4; v8 += 1)
  {
    ivar_init(&(v24).buffer[v8]);
    spawn4(task0, uint32_t, v8, uint32_t, v3, struct awl_signedS32 *, v1, struct awl_i_awl_signedS32, v24);
  }
  len55 = (v24).length;
  (v49).buffer = initArray((v49).buffer, (v49).length, sizeof(int32_t), 0);
  (v49).length = 0;
  for (uint32_t v27 = 0; v27 < len55; v27 += 1)
  {
    v34 = (v49).length;
    e56 = (v24).buffer[v27];
    ivar_get_array_shallow_nontask(&v31, e56, sizeof(int32_t));
    v32 = (v31).length;
    len57 = (v34 + v32);
    (v28).buffer = initArray((v28).buffer, (v28).length, sizeof(int32_t), len57);
    (v28).length = len57;
    for (uint32_t v39 = 0; v39 < v34; v39 += 1)
    {
      (v28).buffer[v39] = (v49).buffer[v39];
    }
    for (uint32_t v43 = 0; v43 < v32; v43 += 1)
    {
      (v28).buffer[(v43 + v34)] = (v31).buffer[v43];
    }
    e58 = v49;
    v49 = v28;
    v28 = e58;
  }
  v50 = (v49).length;
  (*out).buffer = initArray((*out).buffer, (*out).length, sizeof(int32_t), v50);
  (*out).length = v50;
  for (uint32_t v53 = 0; v53 < v50; v53 += 1)
  {
    (*out).buffer[v53] = (v49).buffer[v53];
  }
  taskpool_shutdown();
  freeArray((v24).buffer);
  freeArray((v49).buffer);
  freeArray((v28).buffer);
  freeArray((v31).buffer);
  ivar_destroy(&e56);
}
