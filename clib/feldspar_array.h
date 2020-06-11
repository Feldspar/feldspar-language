//
// Copyright (c) 2009-2011, ERICSSON AB
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//     * Redistributions of source code must retain the above copyright notice,
//       this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of the ERICSSON AB nor the names of its contributors
//       may be used to endorse or promote products derived from this software
//       without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

#ifndef FELDSPAR_ARRAY_H
#define FELDSPAR_ARRAY_H

#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>
//#define LOG
#include "log.h"

/* This library contains operations on flat arrays, arrays that do not contain
   (pointers to) other arrays. For non-flat arrays, these array operations are
   implemented by code generated from the program by the ArrayOps module.
   The size argument is always the size in bytes of each element.
*/

/* TODO qualify the names to avoid clashes with Haskell names */

struct array {
    void*    buffer;   /* pointer to the buffer of elements */
    int32_t  length;   /* number of elements in the array */
};

/// Allocate and initialize a struct array if we did not have one already
static inline struct array *allocArray(struct array* src) {
  log_1("allocArray %p\n", src);
  if (src == NULL) {
    src = malloc(sizeof(struct array));
    src->buffer = NULL;
    src->length = 0;
  }
  return src;
}

/// Resizing an existing array.
static inline void* resizeArray(void* arr, int32_t size, int32_t len) {
  log_3("resize %p with size %d and len %d\n", arr, size, len);
  return realloc(arr, len*size);
}

/// Array (re)initialization for flat arrays.
static inline void* initArray(void* arr, int32_t arrLen, int32_t size, int32_t newLen) {
  log_4("initArray %p with arrlen %d size %d newLen %d\n", arr, arrLen, size, newLen);
  if (newLen != arrLen)
    arr = resizeArray(arr, size, newLen);
  return arr;
}

/// Free a flat array or an array where all the arrays it contains have been free'd already.
// TODO: Think about arrays escaping from their scope.
static inline void freeArray(void* arr) {
  log_1("freeArray %p\n", arr);
  free(arr);
}

/// Deep array copy to a given position for flat arrays.
static inline void* copyArrayPos(void* dst, int32_t dstLen, int32_t size, void* src, int32_t srcLen, int32_t pos) {
  if (srcLen > 0)
    memcpy(dst + pos * size, src, srcLen * size);
  return dst;
}

/// Deep array copy for flat arrays.
static inline void* copyArray(void* dst, int32_t dstLen, int32_t size, void* src, int32_t srcLen) {
  return copyArrayPos(dst, dstLen, size, src, srcLen, 0);
}

/// Combined init and copy for flat arrays.
static inline void* initCopyArray(void* dst, int32_t dstLen, int32_t size, void* src, int32_t srcLen) {
  assert((src || !srcLen) && "source array not initialized");
  assert((src != dst || srcLen == dstLen) && "same source as destination but with different lengths");

  dst = initArray(dst, dstLen, size, srcLen);
  return copyArrayPos(dst, dstLen, size, src, srcLen, 0);
}
#endif
