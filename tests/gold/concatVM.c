#include "concatVM.h"


void concatVM(struct awl_awl_signedS32 * v1, struct awl_signedS32 * out)
{
  uint32_t len0;
  struct awl_signedS32 e1 = { 0 };
  struct awl_signedS32 v7 = { 0 };
  uint32_t v12;
  uint32_t v10;
  uint32_t len2;
  struct awl_signedS32 e3 = { 0 };
  
  len0 = (*v1).length;
  e1 = *out;
  (e1).buffer = initArray((e1).buffer, (e1).length, sizeof(int32_t), 0);
  (e1).length = 0;
  for (uint32_t v6 = 0; v6 < len0; v6 += 1)
  {
    v12 = (e1).length;
    v10 = ((*v1).buffer[v6]).length;
    len2 = (v12 + v10);
    (v7).buffer = initArray((v7).buffer, (v7).length, sizeof(int32_t), len2);
    (v7).length = len2;
    for (uint32_t v17 = 0; v17 < v12; v17 += 1)
    {
      (v7).buffer[v17] = (e1).buffer[v17];
    }
    for (uint32_t v21 = 0; v21 < v10; v21 += 1)
    {
      (v7).buffer[(v21 + v12)] = ((*v1).buffer[v6]).buffer[v21];
    }
    e3 = e1;
    e1 = v7;
    v7 = e3;
  }
  *out = e1;
  freeArray((v7).buffer);
}
