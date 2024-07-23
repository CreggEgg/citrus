#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

extern int64_t printnum(int64_t *);
extern void printint(int64_t);
extern void printstr(int64_t *);
extern int64_t getindex(int64_t *, int64_t);
extern void setindex(int64_t *, int64_t, int64_t);
extern int64_t copy(int64_t *, int64_t *, int64_t);

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

int64_t getindex(int64_t *arr, int64_t i) {
    return *(arr + (8 * (i + 1)));
}
void setindex(int64_t *arr, int64_t i, int64_t val) {
    *(arr + (8 * (i + 1))) = val;
}
int64_t copy(int64_t *a, int64_t *b, int64_t c) {
    printf("size: %d\n", c);
    return memcpy(a, b, c);
}
//
// 	printnum(pointer);
//
// 	return 0;
// }
