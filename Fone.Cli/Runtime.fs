module Fable.C.Runtime

let code = """
typedef struct ref {
    void* data;
    int context;
    void* f;
    bool checked;
} ref;
typedef struct Runtime_pool {
    int size;
    int n;
    ref* data;
} Runtime_pool;

static Runtime_pool pool = { .size = 0, .n = 0, .data = 0 };

void Runtime_pool_track (void* addr, void* f) {
    (*(unsigned char*)addr)++;
    if (pool.n >= pool.size) {
        pool.size = pool.size == 0 ? 1 : pool.size * 2;
        ref* copy = pool.data;
        //int* copyctx = pool.context_data;
        pool.data = malloc(sizeof(ref) * pool.size);
        //pool.context_data = malloc(sizeof(int) * pool.size);
        for (int i = 0; i < pool.n; i++) {
            pool.data[i] = copy[i];
            //pool.context_data[i] = copyctx[i];
        }
        free(copy);
    }
    ref value = { .data = addr, .context = __thread_context + 1, .f = f, .checked = false };
    pool.data[pool.n] = value;
    //pool.context_data[pool.n] = __thread_context;
    pool.n++;
}

void Runtime_clear_pool() {
    int free_count = 0;
    for (int i = 0; i < pool.n; i++) {
        ref r = pool.data[i];
        if (r.checked || r.data == 0) {
            free_count++;
            continue;
        }
        //printf("Checking address %p\n", r.data);
        if (r.checked == false && r.context > __thread_context) {
            unsigned char* p = r.data;
            *p = *p - 1;
            pool.data[i].checked = true;
        }
        unsigned char count = *(unsigned char *)r.data;
        // todo: r.checked == true ?
        if (count <= 0) {
            void (*f)(void*) = r.f;
            //printf("Autorelease freeing %p\n", r.data);
            f(r.data);
            free_count++;
            pool.data[i].data = 0;
        }
    }
    // todo: Only free when pool is empty
    if (free_count == pool.n) {
        pool.size = 0;
        pool.n = 0;
        free(pool.data);
        pool.data = 0;
    }
}
void Runtime_pool_end() {
    // todo: Run destructors ?
    pool.size = 0;
    pool.n = 0;
    free(pool.data);
    pool.data = 0;
}

void* Runtime_autorelease(void* ptr, void* destructor) {
    //printf("Autorelease %p\n", ptr);
    unsigned char* p = ptr;
    *p = *p - 1;
    //void (*f)(void*) = destructor;
    // todo: Only track when *p > 0 ?
    Runtime_pool_track(ptr, destructor);
    //if (*p == 0) {
        //f(ptr);
    //}
    return ptr;
}
void Runtime_end_var_scope(void* ptr, void* destructor) {
    unsigned char* p = ptr;
    void (*f)(void*) = destructor;
    *p = *p - 1;
    if (*p <= 0) {{
        //printf("Freeing %p\n", ptr);
        f(ptr);
    }}
}
void Runtime_swap_value(void** location, void* value, void* destructor) {
    if (*location == value) { return; }
    unsigned char* p = value;
    *p = *p + 1;
    void* oldValue = *location;
    *location = value;
    if (oldValue != NULL)
        Runtime_end_var_scope(oldValue, destructor);
}
"""
