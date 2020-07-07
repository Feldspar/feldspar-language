#include "tuples.h"


void tuples(int32_t v0, int32_t * out)
{
  int32_t v1;
  int32_t v7;
  int32_t v6;
  int32_t v14;
  int32_t v24;
  int32_t v36;
  int32_t v15;
  int32_t v22;
  int32_t v34;
  int32_t v48;
  int32_t v46;
  int32_t v68;
  int32_t v13;
  int32_t v23;
  int32_t v37;
  int32_t v51;
  int32_t v67;
  int32_t v25;
  int32_t v35;
  int32_t v50;
  int32_t v66;
  int32_t v33;
  int32_t v49;
  int32_t v47;
  int32_t v65;
  int32_t v63;
  int32_t v62;
  int32_t v61;
  
  v1 = (v0 * 3);
  v7 = (v1 + v0);
  v6 = (v0 + v1);
  v14 = (v6 + v7);
  v24 = (v1 + v14);
  v36 = (v24 + v1);
  v15 = (v7 + v1);
  v22 = (v14 + v15);
  v34 = (v1 + v22);
  v48 = (v36 + v34);
  v46 = (v34 + v1);
  v68 = (v48 + v46);
  v13 = (v1 + v6);
  v23 = (v13 + v1);
  v37 = (v23 + v24);
  v51 = (v37 + v36);
  v67 = (v51 + v48);
  v25 = (v15 + v13);
  v35 = (v22 + v25);
  v50 = (v1 + v35);
  v66 = (v50 + v1);
  v33 = (v25 + v23);
  v49 = (v35 + v33);
  v47 = (v33 + v37);
  v65 = (v49 + v47);
  v63 = (v1 + v49);
  v62 = (v47 + v51);
  v61 = (v46 + v50);
  *out = ((((((((((((((v65 + v62) + v67) + v68) + v61) + v66) + v63) + v65) + v62) + v67) + v68) + v61) + v66) + v63) + (v1 * v49));
}
