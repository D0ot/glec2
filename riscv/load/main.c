#include <stdint.h>

int main(void) {
  int *ptr = 0;
  for(int i = 0; i < 50; ++i) {
    ptr[i] = i;
  }

  int sum = 0;
  for(int i = 0; i < 50; ++i) {
    sum += ptr[i];
  }


  int sum2 = 0;
  for(int i = 0; i < 50; ++i) {
    ptr[i] = i;
    sum2 += ptr[i];
  }

  uint8_t *ptr2 = 0;
  uint8_t sum3 = 0;
  for(uint8_t i = 0; i < 50; ++i) {
    ptr2[i] = i;
    sum3 += ptr2[i];
  }

  return sum + sum2 + sum3;
}
