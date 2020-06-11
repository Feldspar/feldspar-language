#include "issue128_ex2.h"


void issue128__ex2(uint32_t v0, uint32_t * out)
{
  if ((2 == v0))
  {
    switch (v0)
    {
      case 1:
        *out = 20;
        break;
      default:
        *out = 45;
        break;
    }
  }
  else
  {
    *out = v0;
  }
}
