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

#ifndef IVAR_H
#define IVAR_H

#include <pthread.h>
#include "feldspar_array.h"

struct ivar_internals {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    int full;
    void *data;
};

struct ivar {
    struct ivar_internals *internals;
    struct ivar *self;
};

/* Initializes 'iv'. */
void ivar_init(struct ivar *iv);

/* Deinitializes ivar 'iv'. */
void ivar_destroy(struct ivar *iv);

/* Copies the data at 'd' of size 'size' into the ivar 'iv'. Ivars are 
 * allowed to be written only once! */
void ivar_put_with_size(struct ivar iv, void *d, int size);

/* Wrapper to 'ivar_put_with_size'. */
#define ivar_put(typ,iv,d) ivar_put_with_size(iv,d,sizeof(typ))

/* Specialized version for arrays. */
void ivar_put_array(struct ivar iv, void *d, void* cf);
void ivar_put_array_shallow(struct ivar iv, void *d, int32_t size);

/* Copies the data of size 'size' of the ivar 'iv' to 'var'. Ivars are
 * allowed to be read any number of times. Reading an empty ivar blocks
 * the thread, but a new worker thread is started instead.
 * Use this function only inside tasks! */
void ivar_get_with_size(void *var, struct ivar iv, int size);

/* Wrapper to 'ivar_get_with_size'. */
#define ivar_get(typ,var,iv) ivar_get_with_size(var,iv,sizeof(typ))

/* Specialized version for arrays. */
void ivar_get_array(void *var, struct ivar iv, void* cf);
void ivar_get_array_shallow(void *var, struct ivar iv, int32_t size);

/* Copies the data of size 'size' of the ivar 'iv' to 'var'. Ivars are
 * allowed to be read any number of times. Reading an empty ivar blocks
 * the thread.
 * Use this function only outside tasks, eg. the main thread or similar! */
void ivar_get_nontask_with_size(void *var, struct ivar iv, int size);

/* Wrapper to 'ivar_get_nontask_with_size'. */
#define ivar_get_nontask(typ,var,iv) ivar_get_nontask_with_size(var,iv,sizeof(typ))

/* Specialized version for arrays. */
void ivar_get_array_nontask(void *var, struct ivar iv, void* vcf);
void ivar_get_array_shallow_nontask(void *var, struct ivar iv, int32_t size);

#endif /* IVAR_H */
