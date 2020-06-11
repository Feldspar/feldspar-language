#include "topLevelConsts_native.h"


void topLevelConsts__native(uint32_t v1, uint32_t v2, uint32_t * out)
{
  uint32_t v5;
  
  v5 = (v2 + 5);
  if ((v1 < 5))
  {
    *out = ((uint32_t[]){2, 3, 4, 5, 6})[v5];
  }
  else
  {
    *out = ((uint32_t[]){1, 2, 3, 4, 5})[v5];
  }
}
