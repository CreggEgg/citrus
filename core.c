#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

extern int64_t printnum(int64_t *);
extern void printint(int64_t);
extern void printstr(int64_t *);
extern int64_t index(int64_t *, int64_t);

void printstr(int64_t *str) {
    int64_t len = str[0];
    for (int i = 0; i < len; i++) {
        int64_t c = *(str + (8 * (i + 1)));
        printf("%c", c);
    }
    printf("\n");
}
//

void printint(int64_t n) {
	printf("%d\n", n);
}
int64_t printnum(int64_t *n) {
	printf("%d\n", *n);
	return 0;
}

int64_t index(int64_t *arr, int64_t i) {
    return *(arr + (8 * (i + 1)));
}
// void *ptr(int64_t v) {
// 	int64_t *n = malloc(8);
// 	printf("number: %d", n);
// 	printf("contained: %d", v);
// 	return n;
// }

// int main() {
// 	void *pointer = ptr();
//
// 	printnum(pointer);
//
// 	return 0;
// }
