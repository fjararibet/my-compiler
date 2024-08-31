#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

extern int64_t our_code_starts_here() asm("our_code_starts_here");

typedef uint64_t VAL;
const VAL BOOL_TAG = 1;
const VAL BOOL_TRUE = 0x8000000000000001;
const VAL BOOL_FALSE = 0x0000000000000001;
VAL print(VAL val) {
  if ((val & BOOL_TAG) == 0) {
    printf("%" PRId64, (VAL)val / 2);
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
int main(int argc, char **argv) {
  VAL result = our_code_starts_here();
  print(result);
  return 0;
}
