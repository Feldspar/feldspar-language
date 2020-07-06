#include "concatV.h"


void concatV(struct awl_awl_signedS32 * v1, struct awl_signedS32 * out)
{
  struct awl_signedS32 v26 = { 0 };
  uint32_t len0;
  struct awl_signedS32 v6 = { 0 };
  uint32_t v11;
  uint32_t v9;
  uint32_t len1;
  struct awl_signedS32 e2 = { 0 };
  uint32_t v27;
  
  len0 = (*v1).length;
  (v26).buffer = initArray((v26).buffer, (v26).length, sizeof(int32_t), 0);
  (v26).length = 0;
  for (uint32_t v5 = 0; v5 < len0; v5 += 1)
  {
    v11 = (v26).length;
    v9 = ((*v1).buffer[v5]).length;
    len1 = (v11 + v9);
    (v6).buffer = initArray((v6).buffer, (v6).length, sizeof(int32_t), len1);
    (v6).length = len1;
    for (uint32_t v16 = 0; v16 < v11; v16 += 1)
    {
      (v6).buffer[v16] = (v26).buffer[v16];
    }
    for (uint32_t v20 = 0; v20 < v9; v20 += 1)
    {
      (v6).buffer[(v20 + v11)] = ((*v1).buffer[v5]).buffer[v20];
    }
    e2 = v26;
    v26 = v6;
    v6 = e2;
  }
  v27 = (v26).length;
  (*out).buffer = initArray((*out).buffer, (*out).length, sizeof(int32_t), v27);
  (*out).length = v27;
  for (uint32_t v30 = 0; v30 < v27; v30 += 1)
  {
    (*out).buffer[v30] = (v26).buffer[v30];
  }
  freeArray((v26).buffer);
  freeArray((v6).buffer);
}
