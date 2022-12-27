#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEBUG 1

#define debug(...) do { if (DEBUG) { printf(__VA_ARGS__); } } while (0)

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

char* readString() {
    char* s = NULL;
    size_t len = 0;
    ssize_t read = getline(&s, &len, stdin);
    if (read == -1) {
        error();
    }
    s[read - 1] = '\0';
    return __copyString(s);
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
