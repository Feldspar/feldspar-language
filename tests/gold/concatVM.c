#include "concatVM.h"


void concatVM(struct awl_awl_signedS32 * v1, struct awl_signedS32 * out)
{
  uint32_t len22;
  struct awl_signedS32 e23 = { 0 };
  struct awl_signedS32 v7 = { 0 };
  uint32_t v12;
  uint32_t v10;
  uint32_t len24;
  struct awl_signedS32 e25 = { 0 };
  
  len22 = (*v1).length;
  e23 = *out;
  (e23).buffer = initArray((e23).buffer, (e23).length, sizeof(int32_t), 0);
  (e23).length = 0;
  for (uint32_t v6 = 0; v6 < len22; v6 += 1)
  {
    v12 = (e23).length;
    v10 = ((*v1).buffer[v6]).length;
    len24 = (v12 + v10);
    (v7).buffer = initArray((v7).buffer, (v7).length, sizeof(int32_t), len24);
    (v7).length = len24;
    for (uint32_t v17 = 0; v17 < v12; v17 += 1)
    {
      (v7).buffer[v17] = (e23).buffer[v17];
    }
    for (uint32_t v21 = 0; v21 < v10; v21 += 1)
    {
      (v7).buffer[(v21 + v12)] = ((*v1).buffer[v6]).buffer[v21];
    }
    e25 = e23;
    e23 = v7;
    v7 = e25;
  }
  *out = e23;
  freeArray((v7).buffer);
}
