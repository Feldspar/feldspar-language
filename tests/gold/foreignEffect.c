#include "foreignEffect.h"


void foreignEffect(void * out)
{
  float v77;
  
  alert();
  v77 = getPos();
  launchMissiles(v77);
  *out = cleanUp();
}
