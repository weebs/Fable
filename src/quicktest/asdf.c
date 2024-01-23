#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct ref {
    void* data;
    int context;
    void* f;
} ref;
typedef struct Runtime_pool {
    int size;
    int n;
    ref* data;
} Runtime_pool;

static Runtime_pool pool = { .size = 0, .n = 0, .data = 0 };

void Runtime_clear_pool() {
    for (int i = 0; i < pool.n; i++) {
        ref r = pool.data[i];
        unsigned char count = *(unsigned char*)r.data;
        if (count <= 0 && r.context > __thread_context) {
            void (*f)(void*) = r.f;
            f(r.data);
        }
    }
    pool.size = 0;
    pool.n = 0;
    free(pool.data);
    pool.data = 0;
}

void Runtime_pool_track (void* addr, void* f) {
    if (pool.n >= pool.size) {
        pool.size = pool.size * 2;
        ref* copy = pool.data;
        //int* copyctx = pool.context_data;
        pool.data = malloc(sizeof(void*) * pool.size);
        //pool.context_data = malloc(sizeof(int) * pool.size);
        for (int i = 0; i < pool.n; i++) {
            pool.data[i] = copy[i];
            //pool.context_data[i] = copyctx[i];
        }
    }
    pool.data[pool.n] = { .data = addr, .n = __thread_context, .f = f };
    //pool.context_data[pool.n] = __thread_context;
    pool.n++;
}

typedef struct foo {
    int a;
    int b;
    bool c;
} foo;

int main() {
    foo f = { .a = 1, .b = 2, .c = true };
    foo* p = &f;
    printf("%p\n%p", &f, &(p->b));
}