#include "issue128_ex3.h"


void issue128__ex3(uint32_t v0, uint32_t * out)
{
  uint32_t e0;
  uint32_t e1;
  
  switch (v0)
  {
    case 1:
      e0 = 10;
      break;
    default:
      e0 = 45;
      break;
  }
  if ((2 == v0))
  {
    e1 = 2;
  }
  else
  {
    e1 = v0;
  }
  *out = (e0 + e1);
}
