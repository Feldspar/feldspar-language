#include "arrayInStruct_wool.h"


LOOP_BODY_2(wool0,
            LARGE_BODY,
            uint32_t,
            v9,
            struct s_2_unsignedS32_awl_unsignedS32,
            e10,
            struct s_2_unsignedS32_awl_unsignedS32,
            v6)
{
  ((v6).member2).buffer[v9] = (((e10).member2).buffer[v9] + 5);
}

void arrayInStruct__wool(struct awl_unsignedS32 * v0, struct awl_unsignedS32 * out)
{
  struct s_2_unsignedS32_awl_unsignedS32 e10 = { 0 };
  struct s_2_unsignedS32_awl_unsignedS32 v6 = { 0 };
  bool v3;
  
  (e10).member1 = (*v0).length;
  ((e10).member2).buffer = initCopyArray(((e10).member2).buffer, ((e10).member2).length, sizeof(uint32_t), (*v0).buffer, (*v0).length);
  ((e10).member2).length = (*v0).length;
  v3 = ((e10).member1 > 0);
  while (v3)
  {
    uint32_t len11;
    struct awl_unsignedS32 e12 = { 0 };
    
    (v6).member1 = ((e10).member1 - 1);
    len11 = ((e10).member2).length;
    ((v6).member2).buffer = initArray(((v6).member2).buffer, ((v6).member2).length, sizeof(uint32_t), len11);
    ((v6).member2).length = len11;
    FOR(wool0, 0, len11, e10, v6);
    e12 = (e10).member2;
    e10 = v6;
    (v6).member2 = e12;
    v3 = ((e10).member1 > 0);
  }
  (*out).buffer = initCopyArray((*out).buffer, (*out).length, sizeof(uint32_t), ((e10).member2).buffer, ((e10).member2).length);
  (*out).length = ((e10).member2).length;
  freeArray(((e10).member2).buffer);
  freeArray(((v6).member2).buffer);
}
