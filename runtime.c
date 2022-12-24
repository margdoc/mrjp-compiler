#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error () {
    printf("runtime error\n");
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

char* readString() {
    char* s = NULL;
    size_t len = 0;
    ssize_t read = getline(&s, &len, stdin);
    if (read == -1) {
        error();
    }
    s[read - 1] = '\0';
    return s;
}

char* __concat(char* a, char* b) {
    char *s = malloc(strlen(a) + strlen(b) + 1);
    if (s == NULL) {
        error();
    }
    strcpy(s, a);
    strcat(s, b);
    return s;
}

char* __copyString(char* s) {
    char *t = malloc(strlen(s) + 1);
    if (t == NULL) {
        error();
    }
    strcpy(t, s);
    return t;
}
