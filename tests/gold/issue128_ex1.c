#include "issue128_ex1.h"


void issue128__ex1(uint32_t v0, uint32_t * out)
{
  bool v1;
  
  v1 = (1 == v0);
  if (v1)
  {
    if (v1)
    {
      *out = 10;
    }
    else
    {
      *out = 45;
    }
  }
  else
  {
    *out = v0;
  }
}
