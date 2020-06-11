#include "metrics.h"


struct awl_signedS32 * initArray_awl_signedS32(struct awl_signedS32 * dst, uint32_t oldLen, uint32_t newLen)
{
  if ((oldLen != newLen))
  {
    if ((oldLen < newLen))
    {
      dst = resizeArray(dst, sizeof(struct awl_signedS32), newLen);
      for (int32_t i = oldLen; i < newLen; i += 1)
      {
        struct awl_signedS32 null_arr_0 = { 0 };
        
        dst[i] = null_arr_0;
      }
    }
    else
    {
      for (int32_t i = newLen; i < oldLen; i += 1)
      {
        freeArray((dst[i]).buffer);
      }
      dst = resizeArray(dst, sizeof(struct awl_signedS32), newLen);
    }
  }
  return(dst);
}

void freeArray_awl_signedS32(struct awl_signedS32 * src, int32_t srcLen)
{
  for (int32_t i = 0; i < srcLen; i += 1)
  {
    freeArray((src[i]).buffer);
  }
  freeArray(src);
}

void metrics(struct awl_signedS32 * v1, struct awl_signedS32 * v2, struct awl_awl_s_2_unsignedS32_unsignedS32 * v3, struct awl_awl_signedS32 * out)
{
  uint32_t v10;
  uint32_t v9;
  struct awl_awl_signedS32 v33 = { 0 };
  uint32_t v19;
  struct awl_signedS32 st41 = { 0 };
  struct awl_signedS32 * v14 = NULL;
  uint32_t v37;
  
  v10 = (*v3).length;
  v9 = (*v1).length;
  (st41).buffer = initArray((st41).buffer, (st41).length, sizeof(int32_t), 8);
  (st41).length = 8;
  for (uint32_t v6 = 0; v6 < 8; v6 += 1)
  {
    (st41).buffer[v6] = -32678;
  }
  v14 = &st41;
  (v33).buffer = initArray_awl_signedS32((v33).buffer, (v33).length, v10);
  (v33).length = v10;
  for (uint32_t v13 = 0; v13 < v10; v13 += 1)
  {
    v19 = min(((*v3).buffer[v13]).length, v9);
    ((v33).buffer[v13]).buffer = initArray(((v33).buffer[v13]).buffer, ((v33).buffer[v13]).length, sizeof(int32_t), v19);
    ((v33).buffer[v13]).length = v19;
    for (uint32_t v24 = 0; v24 < v19; v24 += 1)
    {
      ((v33).buffer[v13]).buffer[v24] = (*v14).buffer[(((*v3).buffer[v13]).buffer[v24]).member1];
    }
    v14 = &(v33).buffer[v13];
  }
  (*out).buffer = initArray_awl_signedS32((*out).buffer, (*out).length, v10);
  (*out).length = v10;
  for (uint32_t v34 = 0; v34 < v10; v34 += 1)
  {
    v37 = ((v33).buffer[v34]).length;
    ((*out).buffer[v34]).buffer = initArray(((*out).buffer[v34]).buffer, ((*out).buffer[v34]).length, sizeof(int32_t), v37);
    ((*out).buffer[v34]).length = v37;
    for (uint32_t v40 = 0; v40 < v37; v40 += 1)
    {
      ((*out).buffer[v34]).buffer[v40] = ((v33).buffer[v34]).buffer[v40];
    }
  }
  freeArray_awl_signedS32((v33).buffer, (v33).length);
  freeArray((st41).buffer);
}
