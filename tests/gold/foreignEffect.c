#include "foreignEffect.h"


void foreignEffect(void * out)
{
  float v0;
  
  alert();
  v0 = getPos();
  launchMissiles(v0);
  *out = cleanUp();
}
