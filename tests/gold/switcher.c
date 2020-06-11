#include "switcher.h"


void switcher(uint8_t v0, bool v1, uint8_t * out)
{
  switch (v1)
  {
    case true:
      *out = v0;
      break;
    case false:
      *out = 2;
      break;
    default:
      *out = 0;
      break;
  }
}
