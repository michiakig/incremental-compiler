#include <stdio.h>
#define fixnum_mask 3
#define fixnum_tag 0
#define fixnum_shift 2
#define bool_false 0x2f
#define bool_true 0x6f
int main(int argc, char **argv) {
  int val = scheme_entry();
  if((val & fixnum_mask) == fixnum_tag) {
    printf("%d\n", val >> fixnum_shift);
  } else if(val == bool_false) {
    printf("#f\n");
  } else if(val == bool_true) {
    printf("#t\n");
  } // else if (val == empty_list) {
  return 0;
}
