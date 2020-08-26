#ifndef TMP2_ARRAYINSTRUCTINSTRUCT_H
#define TMP2_ARRAYINSTRUCTINSTRUCT_H

#include "feldspar_c99.h"

struct awl_unsignedS32
{
  uint32_t * buffer;
  uint32_t length;
};

struct s_2_1xunsignedS32_1xawl_unsignedS32
{
  uint32_t member1;
  struct awl_unsignedS32 member2;
};

struct s_2_1xunsignedS32_1xs_2_1xunsignedS32_1xawl_unsignedS32
{
  uint32_t member1;
  struct s_2_1xunsignedS32_1xawl_unsignedS32 member2;
};

void arrayInStructInStruct(struct s_2_1xunsignedS32_1xs_2_1xunsignedS32_1xawl_unsignedS32 * v0, struct s_2_1xunsignedS32_1xs_2_1xunsignedS32_1xawl_unsignedS32 * out);

#endif // TMP2_ARRAYINSTRUCTINSTRUCT_H
