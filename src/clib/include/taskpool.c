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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include "taskpool.h"
//#define LOG
#include "log.h"

/* Definition of the Feldspar application's global taskpool. */
typedef struct {
    int capacity;
    int num_threads, act_threads, min_threads, max_threads;
    int head, tail;
    void **closures;
    int shutdown;
    pthread_mutex_t mutex;
} taskpool;

static taskpool *feldspar_taskpool = 0;

void *worker();

void taskpool_init(int c, int n, int m)  {
    log_3("taskpool_init %d %d %d - enter\n",c,n,m);
    log_0("taskpool_init - allocating taskpool\n");
    feldspar_taskpool = calloc(1, sizeof(taskpool));
    log_1("taskpool_init - allocating %d closures\n",c);
    feldspar_taskpool->closures = malloc(c * sizeof(void *));
    feldspar_taskpool->capacity = c;
    feldspar_taskpool->min_threads = m;
    feldspar_taskpool->max_threads = n;
    pthread_mutex_init(&(feldspar_taskpool->mutex), NULL);
    log_1("taskpool_init - starting %d threads\n",n);
    for (; n > 0; --n)
      taskpool_spawn_worker();
    log_0("taskpool_init - leave\n");
}

void taskpool_shutdown() {
    log_0("taskpool_shutdown - enter\n");
    feldspar_taskpool->shutdown = 1;
    log_0("taskpool_shutdown - shutdown signalled, waiting for workers\n");
    while (1) {
      pthread_mutex_lock(&feldspar_taskpool->mutex);
      int ths = feldspar_taskpool->num_threads;
      pthread_mutex_unlock(&feldspar_taskpool->mutex);
      if (0 == ths) break;
    }
    log_0("taskpool_shutdown - all threads have stopped\n");
    pthread_mutex_destroy (&feldspar_taskpool->mutex);
    log_0("taskpool_shutdown - leave\n");
}

void taskpool_spawn_worker() {
  log_0("taskpool_spawn_worker - enter\n");
  pthread_mutex_lock(&feldspar_taskpool->mutex);
  if (!feldspar_taskpool->shutdown) {
    pthread_t th;
    pthread_create(&th, NULL, &worker, NULL);
    pthread_detach(th);
    log_1("taskpool_spawn_worker - create thread %d\n", (unsigned)th);
    ++feldspar_taskpool->num_threads;
    ++feldspar_taskpool->act_threads;
  }
  pthread_mutex_unlock(&feldspar_taskpool->mutex);
  log_0("taskpool_spawn_worker - leave\n");
}

void spawn(void *closure) {
    log_1("spawn %p - enter\n", closure);
    assert(feldspar_taskpool);
    pthread_mutex_lock(&(feldspar_taskpool->mutex));
    feldspar_taskpool->closures[feldspar_taskpool->tail] = closure;
    log_3("spawn %p - saved as task %d at %p\n"
         , closure, feldspar_taskpool->tail
         , &feldspar_taskpool->closures[feldspar_taskpool->tail]);
    ++feldspar_taskpool->tail;
    if (feldspar_taskpool->tail == feldspar_taskpool->capacity)
        feldspar_taskpool->tail = 0;
    pthread_mutex_unlock(&(feldspar_taskpool->mutex));
    log_1("spawn %p - leave\n", closure);
}

void *worker() {
#ifdef LOG
    unsigned int self = (unsigned long)pthread_self();
#endif
    log_1("worker %d - enter\n", self);
    taskpool *pool = feldspar_taskpool;
    void (*fun)();
    void *closure;
    int awake = 1;
    log_1("worker %d - entering the loop\n", self);
    while (1) {
        if (pool->shutdown && pool->head == pool->tail) {
            log_1("worker %d - shutdown detected, going to terminate\n", self);
            break;
        }
        if (pool->act_threads > pool->max_threads) {
            log_1("worker %d - too many active threads, going to terminate\n", self);
            break;
        }
        fun = NULL;
        closure = NULL;
        pthread_mutex_lock(&(pool->mutex));
        if (pool->head != pool->tail) {
            log_2("worker %d - pop task %d\n", self, pool->head);
            closure = pool->closures[pool->head];
            ++pool->head;
            if (pool->head == pool->capacity)
                pool->head = 0;
        }
        pthread_mutex_unlock(&(pool->mutex));
        if (closure == NULL) {
            if (1 == awake) {
                log_1("worker %d - sleep\n", self);
                awake = 0;
            }
        } else {
            awake = 1;
            fun = *((void(**)())closure);
            log_2("worker %d - closure %p enter\n", self, fun);
            fun(closure + sizeof(void(*)())); /* TODO: sizeof(void*) == sizeof(void(**)()) is assumed here */
            log_2("worker %d - closure %p leave\n", self, fun);
        }
    }
    /* Cleanup before exit: */
    log_1("worker %d - cleanup\n", self);
    pthread_mutex_lock(&(pool->mutex));
    --pool->num_threads;
    --pool->act_threads;
    log_3("worker %d - cleanup done; active: %d, all: %d\n"
         , self, pool->act_threads, pool->num_threads);
    pthread_mutex_unlock(&(pool->mutex));
    log_1("worker %d - leave\n", self);
    return NULL;
}
