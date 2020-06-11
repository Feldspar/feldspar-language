#include "issue128_ex3.h"


void issue128__ex3(uint32_t v0, uint32_t * out)
{
  uint32_t e1;
  uint32_t e2;
  
  switch (v0)
  {
    case 1:
      e1 = 10;
      break;
    default:
      e1 = 45;
      break;
  }
  if ((2 == v0))
  {
    e2 = 2;
  }
  else
  {
    e2 = v0;
  }
  *out = (e1 + e2);
}
