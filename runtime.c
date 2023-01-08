#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define BULTIN_PREFIX(x) __ ## x
#define USER_DEF_PREFIX(x) ___ ## x

void USER_DEF_PREFIX(error) () {
    puts("runtime error");
    exit(1);
}

void BULTIN_PREFIX(dividing_by_zero) () {
    puts("runtime error: dividing by zero");
    exit(1);
}

void USER_DEF_PREFIX(printInt)(int i) {
    printf("%d\n", i);
}

int USER_DEF_PREFIX(readInt)() {
    int i;
    scanf("%d", &i);
    return i;
}

void USER_DEF_PREFIX(printString)(char* s) {
    printf("%s\n", s);
}

typedef int64_t counterType;

char* BULTIN_PREFIX(copyString)(char*);

void* USER_DEF_PREFIX(readString)() {
    char* s = NULL;
    size_t len = 0;
    ssize_t read = getline(&s, &len, stdin);
    if (read == -1) {
        USER_DEF_PREFIX(error)();
    }
    s[read - 1] = '\0';
    void* string = BULTIN_PREFIX(copyString)(s);
    free(s);
    return string;
}

void* BULTIN_PREFIX(alloc)(int size) {
    void *p = malloc(size + sizeof(counterType));
    if (p == NULL) {
        puts("out of memory");
        exit(1);
    }
    counterType *counter = p;
    *counter = 0;
    return p + sizeof(counterType);
}

void BULTIN_PREFIX(addRef)(void* p) {
    if (p != NULL) {
        counterType *counter = p - sizeof(counterType);
        *counter += 1;
    }
}

void BULTIN_PREFIX(removeRef)(void* p) {
    if (p != NULL) {
        counterType *counter = p - sizeof(counterType);
        *counter -= 1;
        assert(*counter >= 0);
        if (*counter == 0) {
            free(counter);
        }
    }
}

char* BULTIN_PREFIX(concat)(char* a, char* b) {
    char *s = BULTIN_PREFIX(alloc)(strlen(a) + strlen(b) + 1);
    strcpy(s, a);
    strcat(s, b);
    return s;
}

long BULTIN_PREFIX(strEqual)(char* a, char* b) {
    return strcmp(a, b) == 0;
}

long BULTIN_PREFIX(strNotEqual)(char* a, char* b) {
    return strcmp(a, b) != 0;
}

char* BULTIN_PREFIX(copyString)(char* s) {
    char *t = BULTIN_PREFIX(alloc)(strlen(s) + 1);
    strcpy(t, s);
    return t;
}

void* BULTIN_PREFIX(allocArray)(int64_t length) {
    void *p = BULTIN_PREFIX(alloc)(8 * length + sizeof(counterType));
    memset(p + sizeof(counterType), 0, 8 * length);
    *(counterType*)p = length;
    return p ;
}

int64_t BULTIN_PREFIX(loadArray)(void* p, int64_t index) {
    if (p == NULL) {
        puts("null pointer");
        exit(1);
    }

    counterType length = *(counterType*)p;
    if (index < 0 || index >= length) {
        puts("array index out of bounds");
        exit(1);
    }

    return *((int64_t*)(p + 8 + 8 * index));
}

void BULTIN_PREFIX(storeArray)(void* p, int64_t index, int64_t value) {
    if (p == NULL) {
        puts("null pointer");
        exit(1);
    }

    counterType length = *(counterType*)p;
    if (index < 0 || index >= length) {
        puts("array index out of bounds");
        exit(1);
    }

    *((int64_t*)(p + 8 + 8 * index)) = value;
}

void* BULTIN_PREFIX(allocObject)(int size, int64_t* vtable) {
    void *p = BULTIN_PREFIX(alloc)(size + 8);
    memset(p + sizeof(int64_t), 0, size);
    *(int64_t**)p = vtable;
    return p;
}
