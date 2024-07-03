#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// extern int64_t printnum(int64_t *);
extern void printnum(int64_t);
extern void printint(int64_t);
//

void printnum(int64_t n) {
	printf("%d\n", n);
}
void printint(int64_t n) {
	printf("%d\n", n);
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
