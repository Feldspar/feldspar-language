#include "deepArrayCopy.h"


struct awl_awl_unsignedS32 * copyArrayPos_awl_awl_unsignedS32(struct awl_awl_unsignedS32 * dst, int32_t dstLen, struct awl_awl_unsignedS32 * src, int32_t srcLen, int32_t pos)
{
  for (int32_t i = 0; i < srcLen; i += 1)
  {
    (dst[(pos + i)]).buffer = initCopyArray_awl_unsignedS32((dst[(pos + i)]).buffer, (dst[(pos + i)]).length, (src[i]).buffer, (src[i]).length);
    (dst[(pos + i)]).length = (src[i]).length;
  }
  return(dst);
}

struct awl_awl_unsignedS32 * copyArray_awl_awl_unsignedS32(struct awl_awl_unsignedS32 * dst, int32_t dstLen, struct awl_awl_unsignedS32 * src, int32_t srcLen)
{
  dst = copyArrayPos_awl_awl_unsignedS32(dst, dstLen, src, srcLen, 0);
  return(dst);
}

struct awl_awl_unsignedS32 * initCopyArray_awl_awl_unsignedS32(struct awl_awl_unsignedS32 * dst, int32_t dstLen, struct awl_awl_unsignedS32 * src, int32_t srcLen)
{
  dst = initArray_awl_awl_unsignedS32(dst, dstLen, srcLen);
  dstLen = srcLen;
  dst = copyArrayPos_awl_awl_unsignedS32(dst, dstLen, src, srcLen, 0);
  return(dst);
}

struct awl_unsignedS32 * copyArrayPos_awl_unsignedS32(struct awl_unsignedS32 * dst, int32_t dstLen, struct awl_unsignedS32 * src, int32_t srcLen, int32_t pos)
{
  for (int32_t i = 0; i < srcLen; i += 1)
  {
    (dst[(pos + i)]).buffer = initCopyArray((dst[(pos + i)]).buffer, (dst[(pos + i)]).length, sizeof(uint32_t), (src[i]).buffer, (src[i]).length);
    (dst[(pos + i)]).length = (src[i]).length;
  }
  return(dst);
}

struct awl_unsignedS32 * copyArray_awl_unsignedS32(struct awl_unsignedS32 * dst, int32_t dstLen, struct awl_unsignedS32 * src, int32_t srcLen)
{
  dst = copyArrayPos_awl_unsignedS32(dst, dstLen, src, srcLen, 0);
  return(dst);
}

struct awl_unsignedS32 * initCopyArray_awl_unsignedS32(struct awl_unsignedS32 * dst, int32_t dstLen, struct awl_unsignedS32 * src, int32_t srcLen)
{
  dst = initArray_awl_unsignedS32(dst, dstLen, srcLen);
  dstLen = srcLen;
  dst = copyArrayPos_awl_unsignedS32(dst, dstLen, src, srcLen, 0);
  return(dst);
}

struct awl_awl_unsignedS32 * initArray_awl_awl_unsignedS32(struct awl_awl_unsignedS32 * dst, uint32_t oldLen, uint32_t newLen)
{
  if ((oldLen != newLen))
  {
    if ((oldLen < newLen))
    {
      dst = resizeArray(dst, sizeof(struct awl_awl_unsignedS32), newLen);
      for (int32_t i = oldLen; i < newLen; i += 1)
      {
        struct awl_awl_unsignedS32 null_arr_0 = { 0 };
        
        dst[i] = null_arr_0;
      }
    }
    else
    {
      for (int32_t i = newLen; i < oldLen; i += 1)
      {
        freeArray_awl_unsignedS32((dst[i]).buffer, (dst[i]).length);
      }
      dst = resizeArray(dst, sizeof(struct awl_awl_unsignedS32), newLen);
    }
  }
  return(dst);
}

void freeArray_awl_awl_unsignedS32(struct awl_awl_unsignedS32 * src, int32_t srcLen)
{
  for (int32_t i = 0; i < srcLen; i += 1)
  {
    freeArray_awl_unsignedS32((src[i]).buffer, (src[i]).length);
  }
  freeArray(src);
}

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

void deepArrayCopy(struct awl_awl_awl_unsignedS32 * v0, struct s_2_awl_awl_awl_unsignedS32_awl_awl_awl_unsignedS32 * out)
{
  ((*out).member1).buffer = initCopyArray_awl_awl_unsignedS32(((*out).member1).buffer, ((*out).member1).length, (*v0).buffer, (*v0).length);
  ((*out).member1).length = (*v0).length;
  ((*out).member2).buffer = initCopyArray_awl_awl_unsignedS32(((*out).member2).buffer, ((*out).member2).length, (*v0).buffer, (*v0).length);
  ((*out).member2).length = (*v0).length;
}
