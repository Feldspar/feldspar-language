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

#include "ivar.h"
#include "taskpool.h"
#include <stdlib.h>
#include <string.h>
//#define LOG
#include "log.h"

/* Type of function pointer */
typedef void* array_copy_t(void* dst, int32_t dstLen, void* src, int32_t srcLen);

void *worker(void *p);

void ivar_init(struct ivar *iv) {
    struct ivar_internals *ivi;
    log_1("ivar_init %p - enter\n", iv);
    ivi = iv->internals = (struct ivar_internals*)malloc(sizeof(struct ivar_internals));
    int err = pthread_mutex_init(&(ivi->mutex), NULL);
    if (err) exit(err);
    err = pthread_cond_init(&(ivi->cond), NULL);
    if (err) exit(err);
    ivi->full = 0;
    iv->self = iv;
    log_1("ivar_init %p - leave\n", iv);
}

void ivar_destroy(struct ivar *iv) {    // TODO: Think about ivars escaping from their scope...
    log_1("ivar_destroy %p - enter\n", iv);
    if (iv->self == iv) {   // This is true iff this iVar is not a copy.
        struct ivar_internals *ivi = iv->internals;
        pthread_mutex_destroy(&(ivi->mutex));
        pthread_cond_destroy(&(ivi->cond));
        if (ivi->full)
            free(ivi->data); // TODO: Destroy deep?
        free(ivi);
    }
    log_1("ivar_destroy %p - leave\n", iv);
}

void ivar_put_with_size(struct ivar iv, void *d, int size) {
    struct ivar_internals *ivi = iv.internals;
    log_3("ivar_put_with_size %p %p %d - enter\n", &iv, d, size);
    pthread_mutex_lock(&(ivi->mutex));
    ivi->data = malloc(size);
    memcpy(ivi->data, d, size);
    ivi->full = 1;
    pthread_cond_broadcast(&(ivi->cond));
    pthread_mutex_unlock(&(ivi->mutex));
    log_3("ivar_put_with_size %p %p %d - leave\n", &iv, d, size);
}

void ivar_put_array(struct ivar iv, void *dv, void* vcf) {
    struct ivar_internals *ivi = iv.internals;
    log_2("ivar_put_array %p %p - enter\n", &iv, d);
    pthread_mutex_lock(&(ivi->mutex));
    array_copy_t *cf = (array_copy_t*) vcf;
    struct array *str = allocArray(ivi->data);
    struct array *d = dv;
    str->buffer = cf(str->buffer, str->length, d->buffer, d->length);
    str->length = d->length;
    ivi->data = str;
    ivi->full = 1;
    pthread_cond_broadcast(&(ivi->cond));
    pthread_mutex_unlock(&(ivi->mutex));
    log_2("ivar_put_array %p %p - leave\n", &iv, d);
}

void ivar_put_array_shallow(struct ivar iv, void *dv, int32_t size) {
    struct ivar_internals *ivi = iv.internals;
    log_2("ivar_put_array_shallow %p %p - enter\n", &iv, d);
    pthread_mutex_lock(&(ivi->mutex));
    struct array *str = allocArray(ivi->data);
    struct array *d = dv;
    str->buffer = initCopyArray(str->buffer, str->length, size, d->buffer, d->length);
    str->length = d->length;
    ivi->data = str;
    ivi->full = 1;
    pthread_cond_broadcast(&(ivi->cond));
    pthread_mutex_unlock(&(ivi->mutex));
    log_2("ivar_put_array_shallow %p %p - leave\n", &iv, d);
}

void ivar_get_helper(struct ivar_internals *iv) {
    log_1("ivar_get_helper %p - enter\n", iv);
    pthread_mutex_lock(&(iv->mutex));
    if (!iv->full) {
        log_1("ivar_get_helper %p - ivar is empty\n", iv);
        taskpool_spawn_worker();
        log_1("ivar_get_helper %p - blocking while waiting for data\n", iv);
        pthread_cond_wait(&(iv->cond), &(iv->mutex));
        log_1("ivar_get_helper %p - data arrived\n" , iv);
    }
    pthread_mutex_unlock(&(iv->mutex));
    log_1("ivar_get_helper %p - leave\n", iv);
}

void ivar_get_with_size(void *var, struct ivar iv, int size) {
    log_3("ivar_get_with_size %p %p %d - enter\n", var, &iv, size);
    ivar_get_helper(iv.internals);
    memcpy(var, iv.internals->data, size);
    log_3("ivar_get_with_size %p %p %d - leave\n", var, &iv, size);
}

void ivar_get_array(void *vvar, struct ivar iv, void* vcf) {
    struct array *ptr;
    log_2("ivar_get_array %p %p - enter\n", var, &iv);
    ivar_get_helper(iv.internals);
    ptr = (struct array*)iv.internals->data;
    assert(ptr);
    struct array *var = vvar;
    array_copy_t* cf = (array_copy_t*) vcf;
    var->buffer = cf(var->buffer, var->length, ptr->buffer, ptr->length);
    var->length = ptr->length;
    log_2("ivar_get_array %p %p - leave\n", var, &iv);
}

void ivar_get_array_shallow(void *vvar, struct ivar iv, int32_t size) {
    struct array *ptr;
    log_2("ivar_get_array_shallow %p %p - enter\n", var, &iv);
    ivar_get_helper(iv.internals);
    ptr = (struct array*)iv.internals->data;
    assert(ptr);
    struct array *var = vvar;
    var->buffer = initCopyArray(var->buffer, var->length, size, ptr->buffer, ptr->length);
    var->length = ptr->length;
    log_2("ivar_get_arra_shallowy %p %p - leave\n", var, &iv);
}

void ivar_get_nontask_with_size(void *var, struct ivar iv, int size) {
    struct ivar_internals *ivi = iv.internals;
    log_3("ivar_get_nontask_with_size %p %p %d - enter\n", var, &iv, size);
    pthread_mutex_lock(&(ivi->mutex));
    if (!ivi->full)
        log_3("ivar_get_nontask_with_size %p %p %d -> waiting for data\n"
             , var, &iv, size);
    while (!ivi->full) {
        int err = pthread_cond_wait(&(ivi->cond), &(ivi->mutex));
        if (err) exit(err);
    }
    pthread_mutex_unlock(&(ivi->mutex));
    assert(ivi->data);
    memcpy(var, ivi->data, size);
    log_3("ivar_get_nontask_with_size %p %p %d - leave\n", var, &iv, size);
}

void ivar_get_array_nontask(void *vvar, struct ivar iv, void* vcf) {
    struct ivar_internals *ivi = iv.internals;
    struct array *ptr;
    log_2("ivar_get_array_nontask %p %p - enter\n", var, &iv);
    pthread_mutex_lock(&(ivi->mutex));
    if (!ivi->full)
        log_2("ivar_get_array_nontask %p %p - waiting for data\n", var, &iv);
    while (!ivi->full) {
        int err = pthread_cond_wait(&(ivi->cond), &(ivi->mutex));
        if (err) { exit(err); }
    }
    assert(ivi->full);
    pthread_mutex_unlock(&(ivi->mutex));
    if (NULL == ivi->data) {
        log_2("ivar_get_array_nontask %p %p - data uninitialized\n", var, &iv);
    } else {
        ptr = (struct array*)ivi->data;
        struct array *var = vvar;
        array_copy_t* cf = (array_copy_t*) vcf;
        var->buffer = cf(var->buffer, var->length, ptr->buffer, ptr->length);
        var->length = ptr->length;
    }
    log_2("ivar_get_array_nontask %p %p - leave\n", var, &iv);
}

void ivar_get_array_shallow_nontask(void *vvar, struct ivar iv, int32_t size) {
    struct ivar_internals *ivi = iv.internals;
    struct array *ptr;
    log_2("ivar_get_array_shallow_nontask %p %p - enter\n", var, &iv);
    pthread_mutex_lock(&(ivi->mutex));
    if (!ivi->full)
        log_2("ivar_get_array_shallow_nontask %p %p - waiting for data\n", var, &iv);
    while (!ivi->full) {
        int err = pthread_cond_wait(&(ivi->cond), &(ivi->mutex));
        if (err) exit(err);
    }
    assert(ivi->full);
    pthread_mutex_unlock(&(ivi->mutex));
    if (NULL == ivi->data) {
        log_2("ivar_get_array_shallow_nontask %p %p - data uninitialized\n", var, &iv);
    } else {
        ptr = (struct array*)ivi->data;
        struct array *var = vvar;
        var->buffer = initCopyArray(var->buffer, var->length, size, ptr->buffer, ptr->length);
        var->length = ptr->length;
    }
    log_2("ivar_get_array_shallow_nontask %p %p - leave\n", var, &iv);
}
