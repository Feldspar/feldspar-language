#include "noinline1.h"


void noinline0(bool v0, bool * out)
{
  *out = !(v0);
}

void noinline1(bool v0, bool * out)
{
  noinline0(v0, out);
}
