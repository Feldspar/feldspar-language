#include "tuples.h"


void tuples(int32_t v0, int32_t * out)
{
  int32_t v1;
  int32_t v7;
  int32_t v13;
  int32_t v23;
  int32_t v6;
  int32_t v15;
  int32_t v22;
  int32_t v36;
  int32_t v14;
  int32_t v24;
  int32_t v33;
  int32_t v51;
  int32_t v25;
  int32_t v35;
  int32_t v49;
  int32_t v68;
  int32_t v37;
  int32_t v48;
  int32_t v34;
  int32_t v46;
  int32_t v67;
  int32_t v47;
  int32_t v66;
  int32_t v64;
  int32_t v50;
  int32_t v63;
  int32_t v62;
  int32_t v61;
  
  v1 = (v0 * 3);
  v7 = (v0 + v1);
  v13 = (v1 + v7);
  v23 = (v13 + v1);
  v6 = (v1 + v0);
  v15 = (v6 + v1);
  v22 = (v15 + v13);
  v36 = (v22 + v23);
  v14 = (v7 + v6);
  v24 = (v1 + v14);
  v33 = (v23 + v24);
  v51 = (v36 + v33);
  v25 = (v14 + v15);
  v35 = (v25 + v22);
  v49 = (v35 + v36);
  v68 = (v49 + v51);
  v37 = (v1 + v25);
  v48 = (v37 + v1);
  v34 = (v24 + v1);
  v46 = (v34 + v37);
  v67 = (v46 + v48);
  v47 = (v1 + v35);
  v66 = (v47 + v1);
  v64 = (v1 + v49);
  v50 = (v33 + v34);
  v63 = (v51 + v50);
  v62 = (v50 + v46);
  v61 = (v48 + v47);
  *out = ((((((((((((((v68 + v63) + v62) + v67) + v61) + v66) + v64) + v68) + v63) + v62) + v67) + v61) + v66) + v64) + (v1 * v49));
}
