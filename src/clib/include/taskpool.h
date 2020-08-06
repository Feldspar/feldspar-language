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

#ifndef TASKPOOL_H
#define TASKPOOL_H

#include <pthread.h>

void taskpool_init(int c, int num, int min);

void taskpool_shutdown();

void taskpool_spawn_worker();

void spawn(void *closure);

/* Helper macro: */
/* TODO: Replace runtime check with a compile time one! */
#define check_array(t, temp)   \
    if (!strcmp(#t,"struct array *")) { \
        void *m = malloc(sizeof(struct array)); \
        memcpy(m, *(void**)(&(temp)), sizeof(struct array));    \
        memcpy((void*)&(temp), (void*)&m, sizeof(void*));   \
    }   \

/* Wrappers for spawn with different number of arguments: */

#define spawn0(task)  \
    {   \
        void *buffer = malloc(sizeof(void(*)()));   \
        char *p = buffer;   \
        void *t0 = &task;    \
        memcpy(p, &t0, sizeof(void(*)()));   \
        spawn(buffer);    \
    }   \

#define spawn1(task, t1, p1)  \
    {   \
        void *buffer = malloc(sizeof(void(*)()) + sizeof(t1));   \
        char *p = buffer;   \
        void *t0 = &task;    \
        t1 temp1 = (p1);    \
        memcpy(p, &t0, sizeof(void(*)()));   \
        p += sizeof(void(*)()); \
        check_array(t1, temp1);   \
        memcpy(p, &temp1, sizeof(t1));    \
        spawn(buffer);    \
    }   \

#define spawn2(task, t1, p1, t2, p2)  \
    {   \
        void *buffer = malloc(sizeof(void(*)()) + sizeof(t1) + sizeof(t2));   \
        char *p = buffer;   \
        void *t0 = &task;    \
        t1 temp1 = (p1);    \
        t2 temp2 = (p2);    \
        memcpy(p, &t0, sizeof(void(*)()));   \
        p += sizeof(void(*)()); \
        check_array(t1, temp1);   \
        memcpy(p, &temp1, sizeof(t1));    \
        p += sizeof(t1);    \
        check_array(t2, temp2);   \
        memcpy(p, &temp2, sizeof(t2));    \
        spawn(buffer);    \
    }   \

#define spawn3(task, t1, p1, t2, p2, t3, p3)  \
    {   \
        void *buffer = malloc(sizeof(void(*)()) + sizeof(t1) + sizeof(t2) + sizeof(t3));   \
        char *p = buffer;   \
        void *t0 = &task;    \
        t1 temp1 = (p1);    \
        t2 temp2 = (p2);    \
        t3 temp3 = (p3);    \
        memcpy(p, &t0, sizeof(void(*)()));   \
        p += sizeof(void(*)()); \
        check_array(t1, temp1);   \
        memcpy(p, &temp1, sizeof(t1));    \
        p += sizeof(t1);    \
        check_array(t2, temp2);   \
        memcpy(p, &temp2, sizeof(t2));    \
        p += sizeof(t2);    \
        check_array(t3, temp3);   \
        memcpy(p, &temp3, sizeof(t3));    \
        spawn(buffer);    \
    }   \

#define spawn4(task, t1, p1, t2, p2, t3, p3, t4, p4)  \
    {   \
        void *buffer = malloc(sizeof(void(*)()) + sizeof(t1) + sizeof(t2) + sizeof(t3) + sizeof(t4));   \
        char *p = buffer;   \
        void *t0 = &task;    \
        t1 temp1 = (p1);    \
        t2 temp2 = (p2);    \
        t3 temp3 = (p3);    \
        t4 temp4 = (p4);    \
        memcpy(p, &t0, sizeof(void(*)()));   \
        p += sizeof(void(*)()); \
        check_array(t1, temp1);   \
        memcpy(p, &temp1, sizeof(t1));    \
        p += sizeof(t1);    \
        check_array(t2, temp2);   \
        memcpy(p, &temp2, sizeof(t2));    \
        p += sizeof(t2);    \
        check_array(t3, temp3);   \
        memcpy(p, &temp3, sizeof(t3));    \
        p += sizeof(t3);    \
        check_array(t4, temp4);   \
        memcpy(p, &temp4, sizeof(t4));    \
        spawn(buffer);    \
    }   \

#define spawn5(task, t1, p1, t2, p2, t3, p3, t4, p4, t5, p5)  \
    {   \
        void *buffer = malloc(sizeof(void(*)()) + sizeof(t1) + sizeof(t2) + sizeof(t3) + sizeof(t4) + sizeof(t5));   \
        char *p = buffer;   \
        void *t0 = &task;    \
        t1 temp1 = (p1);    \
        t2 temp2 = (p2);    \
        t3 temp3 = (p3);    \
        t4 temp4 = (p4);    \
        t5 temp5 = (p5);    \
        memcpy(p, &t0, sizeof(void(*)()));   \
        p += sizeof(void(*)()); \
        check_array(t1, temp1);   \
        memcpy(p, &temp1, sizeof(t1));    \
        p += sizeof(t1);    \
        check_array(t2, temp2);   \
        memcpy(p, &temp2, sizeof(t2));    \
        p += sizeof(t2);    \
        check_array(t3, temp3);   \
        memcpy(p, &temp3, sizeof(t3));    \
        p += sizeof(t3);    \
        check_array(t4, temp4);   \
        memcpy(p, &temp4, sizeof(t4));    \
        p += sizeof(t4);    \
        check_array(t5, temp5);   \
        memcpy(p, &temp5, sizeof(t5));    \
        spawn(buffer);    \
    }   \

#define spawn6(task, t1, p1, t2, p2, t3, p3, t4, p4, t5, p5, t6, p6)  \
    {   \
        void *buffer = malloc(sizeof(void(*)()) + sizeof(t1) + sizeof(t2) + sizeof(t3) + sizeof(t4) + sizeof(t5) + sizeof(t6));   \
        char *p = buffer;   \
        void *t0 = &task;    \
        t1 temp1 = (p1);    \
        t2 temp2 = (p2);    \
        t3 temp3 = (p3);    \
        t4 temp4 = (p4);    \
        t5 temp5 = (p5);    \
        t6 temp6 = (p6);    \
        memcpy(p, &t0, sizeof(void(*)()));   \
        p += sizeof(void(*)()); \
        check_array(t1, temp1);   \
        memcpy(p, &temp1, sizeof(t1));    \
        p += sizeof(t1);    \
        check_array(t2, temp2);   \
        memcpy(p, &temp2, sizeof(t2));    \
        p += sizeof(t2);    \
        check_array(t3, temp3);   \
        memcpy(p, &temp3, sizeof(t3));    \
        p += sizeof(t3);    \
        check_array(t4, temp4);   \
        memcpy(p, &temp4, sizeof(t4));    \
        p += sizeof(t4);    \
        check_array(t5, temp5);   \
        memcpy(p, &temp5, sizeof(t5));    \
        p += sizeof(t5);    \
        check_array(t6, temp6);   \
        memcpy(p, &temp6, sizeof(t6));    \
        spawn(buffer);    \
    }   \

#define spawn7(task, t1, p1, t2, p2, t3, p3, t4, p4, t5, p5, t6, p6 , t7, p7)  \
    {   \
        void *buffer = malloc(sizeof(void(*)()) + sizeof(t1) + sizeof(t2) + sizeof(t3) + sizeof(t4) + sizeof(t5) + sizeof(t6) + sizeof(t7));   \
        char *p = buffer;   \
        void *t0 = &task;    \
        t1 temp1 = (p1);    \
        t2 temp2 = (p2);    \
        t3 temp3 = (p3);    \
        t4 temp4 = (p4);    \
        t5 temp5 = (p5);    \
        t6 temp6 = (p6);    \
        t7 temp7 = (p7);    \
        memcpy(p, &t0, sizeof(void(*)()));   \
        p += sizeof(void(*)()); \
        check_array(t1, temp1);   \
        memcpy(p, &temp1, sizeof(t1));    \
        p += sizeof(t1);    \
        check_array(t2, temp2);   \
        memcpy(p, &temp2, sizeof(t2));    \
        p += sizeof(t2);    \
        check_array(t3, temp3);   \
        memcpy(p, &temp3, sizeof(t3));    \
        p += sizeof(t3);    \
        check_array(t4, temp4);   \
        memcpy(p, &temp4, sizeof(t4));    \
        p += sizeof(t4);    \
        check_array(t5, temp5);   \
        memcpy(p, &temp5, sizeof(t5));    \
        p += sizeof(t5);    \
        check_array(t6, temp6);   \
        memcpy(p, &temp6, sizeof(t6));    \
        p += sizeof(t6);    \
        check_array(t7, temp7);   \
        memcpy(p, &temp7, sizeof(t7));    \
        spawn(buffer);    \
    }   \

/* Wrappers for task cores with different number of arguments: */

#define run0(task_core)   \
    {   \
        (task_core)();   \
    }   \

#define run1(task_core, t1)   \
    {   \
        t1 p1;  \
        char *p = params;   \
        memcpy(&p1, p, sizeof(t1));  \
        (task_core)(p1);   \
    }   \

#define run2(task_core, t1, t2)   \
    {   \
        t1 p1;  \
        t2 p2;  \
        char *p = params;   \
        memcpy(&p1, p, sizeof(t1));  \
        p += sizeof(t1);    \
        memcpy(&p2, p, sizeof(t2));  \
        (task_core)(p1,p2);   \
    }   \

#define run3(task_core, t1, t2, t3)   \
    {   \
        t1 p1;  \
        t2 p2;  \
        t3 p3;  \
        char *p = params;   \
        memcpy(&p1, p, sizeof(t1));  \
        p += sizeof(t1);    \
        memcpy(&p2, p, sizeof(t2));  \
        p += sizeof(t2);    \
        memcpy(&p3, p, sizeof(t3));  \
        (task_core)(p1,p2,p3);   \
    }   \

#define run4(task_core, t1, t2, t3, t4)   \
    {   \
        t1 p1;  \
        t2 p2;  \
        t3 p3;  \
        t4 p4;  \
        char *p = params;   \
        memcpy(&p1, p, sizeof(t1));  \
        p += sizeof(t1);    \
        memcpy(&p2, p, sizeof(t2));  \
        p += sizeof(t2);    \
        memcpy(&p3, p, sizeof(t3));  \
        p += sizeof(t3);    \
        memcpy(&p4, p, sizeof(t4));  \
        (task_core)(p1,p2,p3,p4);   \
    }   \

#define run5(task_core, t1, t2, t3, t4, t5)   \
    {   \
        t1 p1;  \
        t2 p2;  \
        t3 p3;  \
        t4 p4;  \
        t5 p5;  \
        char *p = params;   \
        memcpy(&p1, p, sizeof(t1));  \
        p += sizeof(t1);    \
        memcpy(&p2, p, sizeof(t2));  \
        p += sizeof(t2);    \
        memcpy(&p3, p, sizeof(t3));  \
        p += sizeof(t3);    \
        memcpy(&p4, p, sizeof(t4));  \
        p += sizeof(t4);    \
        memcpy(&p5, p, sizeof(t5));  \
        (task_core)(p1,p2,p3,p4,p5);   \
    }   \

#define run6(task_core, t1, t2, t3, t4, t5, t6)   \
    {   \
        t1 p1;  \
        t2 p2;  \
        t3 p3;  \
        t4 p4;  \
        t5 p5;  \
        t6 p6;  \
        char *p = params;   \
        memcpy(&p1, p, sizeof(t1));  \
        p += sizeof(t1);    \
        memcpy(&p2, p, sizeof(t2));  \
        p += sizeof(t2);    \
        memcpy(&p3, p, sizeof(t3));  \
        p += sizeof(t3);    \
        memcpy(&p4, p, sizeof(t4));  \
        p += sizeof(t4);    \
        memcpy(&p5, p, sizeof(t5));  \
        p += sizeof(t5);    \
        memcpy(&p6, p, sizeof(t6));  \
        (task_core)(p1,p2,p3,p4,p5,p6);   \
    }   \

#define run7(task_core, t1, t2, t3, t4, t5, t6, t7)   \
    {   \
        t1 p1;  \
        t2 p2;  \
        t3 p3;  \
        t4 p4;  \
        t5 p5;  \
        t6 p6;  \
        t7 p7;  \
        char *p = params;   \
        memcpy(&p1, p, sizeof(t1));  \
        p += sizeof(t1);    \
        memcpy(&p2, p, sizeof(t2));  \
        p += sizeof(t2);    \
        memcpy(&p3, p, sizeof(t3));  \
        p += sizeof(t3);    \
        memcpy(&p4, p, sizeof(t4));  \
        p += sizeof(t4);    \
        memcpy(&p5, p, sizeof(t5));  \
        p += sizeof(t5);    \
        memcpy(&p6, p, sizeof(t6));  \
        p += sizeof(t6);    \
        memcpy(&p7, p, sizeof(t7));  \
        (task_core)(p1,p2,p3,p4,p5,p6,p7);   \
    }   \

#endif /* TASKPOOL_H */
