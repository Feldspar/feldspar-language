#include "arrayInStructInStruct.h"


void arrayInStructInStruct(struct s_2_unsignedS32_s_2_unsignedS32_awl_unsignedS32 * v0, struct s_2_unsignedS32_s_2_unsignedS32_awl_unsignedS32 * out)
{
  (*out).member1 = (*v0).member1;
  ((*out).member2).member1 = ((*v0).member2).member1;
  (((*out).member2).member2).buffer = initCopyArray((((*out).member2).member2).buffer, (((*out).member2).member2).length, sizeof(uint32_t), (((*v0).member2).member2).buffer, (((*v0).member2).member2).length);
  (((*out).member2).member2).length = (((*v0).member2).member2).length;
}
