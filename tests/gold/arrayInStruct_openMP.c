#include "arrayInStruct_openMP.h"


void arrayInStruct__openMP(struct awl_unsignedS32 * v0, struct awl_unsignedS32 * out)
{
  struct s_2_unsignedS32_awl_unsignedS32 e0 = { 0 };
  struct s_2_unsignedS32_awl_unsignedS32 v6 = { 0 };
  bool v3;
  
  (e0).member1 = (*v0).length;
  ((e0).member2).buffer = initCopyArray(((e0).member2).buffer, ((e0).member2).length, sizeof(uint32_t), (*v0).buffer, (*v0).length);
  ((e0).member2).length = (*v0).length;
  v3 = ((e0).member1 > 0);
  while (v3)
  {
    uint32_t len1;
    struct awl_unsignedS32 e2 = { 0 };
    
    (v6).member1 = ((e0).member1 - 1);
    len1 = ((e0).member2).length;
    ((v6).member2).buffer = initArray(((v6).member2).buffer, ((v6).member2).length, sizeof(uint32_t), len1);
    ((v6).member2).length = len1;
    #pragma omp parallel for
    for (uint32_t v10 = 0; v10 < len1; v10 += 1)
    {
      ((v6).member2).buffer[v10] = (((e0).member2).buffer[v10] + 5);
    }
    e2 = (e0).member2;
    e0 = v6;
    (v6).member2 = e2;
    v3 = ((e0).member1 > 0);
  }
  (*out).buffer = initCopyArray((*out).buffer, (*out).length, sizeof(uint32_t), ((e0).member2).buffer, ((e0).member2).length);
  (*out).length = ((e0).member2).length;
  freeArray(((e0).member2).buffer);
  freeArray(((v6).member2).buffer);
}
