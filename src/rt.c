#include <stdio.h>
#define fixnum_mask 3
#define fixnum_tag 0
#define fixnum_shift 2
#define bool_false 0x2f
#define bool_true 0x6f
#define empty_list 0x3f
int main(int argc, char **argv) {
  int val = scheme_entry();
  if((val & fixnum_mask) == fixnum_tag) {
    printf("%d", val >> fixnum_shift);
  } else if(val == bool_false) {
    printf("#f");
  } else if(val == bool_true) {
    printf("#t");
  } else if (val == empty_list) {
    printf("()");    
  }
  printf("\n");
  return 0;
}
