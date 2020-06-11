#include "scanlPush.h"


struct awl_unsignedS32 * initArray_awl_unsignedS32(struct awl_unsignedS32 * dst, uint32_t oldLen, uint32_t newLen)
{
  if ((oldLen != newLen))
  {
    if ((oldLen < newLen))
    {
      dst = resizeArray(dst, sizeof(struct awl_unsignedS32), newLen);
      for (int32_t i = oldLen; i < newLen; i += 1)
      {
        struct awl_unsignedS32 null_arr_0 = { 0 };
        
        dst[i] = null_arr_0;
      }
    }
    else
    {
      for (int32_t i = newLen; i < oldLen; i += 1)
      {
        freeArray((dst[i]).buffer);
      }
      dst = resizeArray(dst, sizeof(struct awl_unsignedS32), newLen);
    }
  }
  return(dst);
}

void freeArray_awl_unsignedS32(struct awl_unsignedS32 * src, int32_t srcLen)
{
  for (int32_t i = 0; i < srcLen; i += 1)
  {
    freeArray((src[i]).buffer);
  }
  freeArray(src);
}

void scanlPush(struct awl_unsignedS32 * v0, struct awl_unsignedS32 * v1, struct awl_awl_unsignedS32 * out)
{
  uint32_t v9;
  uint32_t v2;
  struct awl_unsignedS32 v12 = { 0 };
  uint32_t v15;
  struct awl_unsignedS32 v23 = { 0 };
  uint32_t v24;
  struct awl_unsignedS32 e27 = { 0 };
  
  v9 = (*v1).length;
  (*out).buffer = initArray_awl_unsignedS32((*out).buffer, (*out).length, v9);
  (*out).length = v9;
  v2 = (*v0).length;
  (v12).buffer = initArray((v12).buffer, (v12).length, sizeof(uint32_t), v2);
  (v12).length = v2;
  for (uint32_t v4 = 0; v4 < v2; v4 += 1)
  {
    (v12).buffer[v4] = (*v0).buffer[v4];
  }
  for (uint32_t v13 = 0; v13 < v9; v13 += 1)
  {
    v15 = (v12).length;
    (v12).buffer = initArray((v12).buffer, (v12).length, sizeof(uint32_t), v15);
    (v12).length = v15;
    (v23).buffer = initCopyArray((v23).buffer, (v23).length, sizeof(uint32_t), (v12).buffer, (v12).length);
    (v23).length = (v12).length;
    v24 = (v23).length;
    (e27).buffer = initArray((e27).buffer, (e27).length, sizeof(uint32_t), v24);
    (e27).length = v24;
    for (uint32_t v26 = 0; v26 < v24; v26 += 1)
    {
      (e27).buffer[v26] = (v23).buffer[v26];
    }
    ((*out).buffer[v13]).buffer = initCopyArray(((*out).buffer[v13]).buffer, ((*out).buffer[v13]).length, sizeof(uint32_t), (e27).buffer, (e27).length);
    ((*out).buffer[v13]).length = (e27).length;
  }
  freeArray((v12).buffer);
  freeArray((v23).buffer);
  freeArray((e27).buffer);
}
