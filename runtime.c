#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


void error () {
    puts("runtime error");
    exit(1);
}

void printInt(int i) {
    printf("%d\n", i);
}

int readInt() {
    int i;
    scanf("%d", &i);
    return i;
}

void printString(char* s) {
    printf("%s\n", s);
}

typedef long counterType;

char* __copyString(char*);

void* readString() {
    char* s = NULL;
    size_t len = 0;
    ssize_t read = getline(&s, &len, stdin);
    if (read == -1) {
        error();
    }
    s[read - 1] = '\0';
    void* string = __copyString(s);
    free(s);
    return string;
}

void* __alloc(int size) {
    void *p = malloc(size + sizeof(counterType));
    if (p == NULL) {
        puts("out of memory");
        exit(1);
    }
    counterType *counter = p;
    *counter = 0;
    return p + sizeof(counterType);
}

void __addRef(void* p) {
    if (p != NULL) {
        counterType *counter = p - sizeof(counterType);
        *counter += 1;
    }
}

void __removeRef(void* p) {
    if (p != NULL) {
        counterType *counter = p - sizeof(counterType);
        *counter -= 1;
        assert(*counter >= 0);
        if (*counter == 0) {
            free(counter);
        }
    }
}

char* __concat(char* a, char* b) {
    char *s = __alloc(strlen(a) + strlen(b) + 1);
    strcpy(s, a);
    strcat(s, b);
    return s;
}

char* __copyString(char* s) {
    char *t = __alloc(strlen(s) + 1);
    strcpy(t, s);
    return t;
}

void* __allocArray(long length) {
    void *p = __alloc(8 * length + sizeof(counterType));
    memset(p + sizeof(counterType), 0, 8 * length);
    *(counterType*)p = length;
    return p ;
}

long __loadArray(void* p, long index) {
    if (p == NULL) {
        puts("null pointer");
        exit(1);
    }

    counterType length = *(counterType*)p;
    if (index < 0 || index >= length) {
        puts("array index out of bounds");
        exit(1);
    }

    return *((long*)(p + 8 + 8 * index));
}

void __storeArray(void* p, long index, long value) {
    if (p == NULL) {
        puts("null pointer");
        exit(1);
    }

    counterType length = *(counterType*)p;
    if (index < 0 || index >= length) {
        puts("array index out of bounds");
        exit(1);
    }

    *((long*)(p + 8 + 8 * index)) = value;
}

void* __allocObject(int size, long* vtable) {
    void *p = __alloc(size + 8);
    memset(p + sizeof(long), 0, size);
    *(long**)p = vtable;
    return p;
}
