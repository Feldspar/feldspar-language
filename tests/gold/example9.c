#include "example9.h"


void example9(int32_t v0, int32_t * out)
{
  int32_t v2;
  
  v2 = (v0 + 20);
  if ((v0 < 5))
  {
    *out = (3 * v2);
  }
  else
  {
    *out = (30 * v2);
  }
}
