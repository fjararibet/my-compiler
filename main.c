#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

extern int64_t our_code_starts_here() asm("our_code_starts_here");

typedef uint64_t VAL;
const VAL BOOL_TAG = 1;
const VAL BOOL_TRUE = 0x8000000000000001;
const VAL BOOL_FALSE = 0x0000000000000001;
VAL print(VAL val) {
  if ((val & BOOL_TAG) == 0) {
    printf("%" PRId64, (int64_t)val / 2);
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } else {
    // print unknown val in hex
    printf("Unknown value: %" PRIx64, val);
  }
  return val;
}

const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
void error(int errCode, int val) {
  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "Expected number, but got %010x\n", val);
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Expected boolean, but got %010x\n", val);
  }
  exit(errCode);
}

int main(int argc, char **argv) {
  VAL result = our_code_starts_here();
  print(result);
  printf("\n");
  return 0;
}
