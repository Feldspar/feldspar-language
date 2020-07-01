#include <stdlib.h>
#include "MatMulC.h"

void MatMulC(int rows, int len, double *a, double *bin, double *c) {
  double *b = malloc(len * sizeof(double));

  // Transpose bin with result in b.
  for (int n = 0; n < len; n++) {
    b[n] = bin[rows * (n % rows) + (n / rows)];
  }

  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < rows; j++) {
      double sum = 0.0;
      for (int k = 0; k < rows; k++) {
        sum += a[i*rows+k] * b[j*rows + k];
      }
      c[i*rows + j] = sum;
    }
  }

  free(b);
}

/**
 * Same code as above with middle loop unrolled once to improve the balance
 * between computation and memory reads.
 */
void MatMulCopt(int rows, int len, double *a, double *bin, double *c) {
  double *b = malloc(len * sizeof(double));

  // Transpose bin with result in b.
  for(int n = 0; n < len; n++ ) {
    b[n] = bin[rows * (n % rows) + (n / rows)];
  }

  for (int i = 0; i < rows; i++)  {
    for (int j = 0; j < rows; j += 2) {
      double sum0 = 0.0;
      double sum1 = 0.0;
      for (int k = 0; k < rows; k++) {
        sum0 += a[i*rows+k] * b[j*rows + k];
        sum1 += a[i*rows+k] * b[(j + 1)*rows + k];
      }
      c[i*rows + j] = sum0;
      c[i*rows + j+1] = sum1;
    }
  }

  free(b);
}
