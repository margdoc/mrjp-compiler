#include <stdio.h>
#include <stdlib.h>

void printInt(int i) {
    printf("%d\n", i);
}

int readInt() {
    int i;
    scanf("%d", &i);
    return i;
}

void error () {
    printf("runtime error\n");
    exit(1);
}
