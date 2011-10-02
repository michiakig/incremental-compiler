#include <stdio.h>
#include <sys/mman.h>
#define fixnum_mask 3
#define fixnum_tag 0
#define fixnum_shift 2
#define bool_false 0x2f
#define bool_true 0x6f
#define empty_list 0x3f
#define char_mask 0xff
#define char_tag 0x0f
#define char_shift 8

static char* allocate_protected_space(int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  char* p = mmap(0, aligned_size + 2 * page,
                 PROT_READ | PROT_WRITE,
                 MAP_ANONYMOUS | MAP_PRIVATE,
                 0, 0);
  if (p == MAP_FAILED){ /*---*/ }
  status = mprotect(p, page, PROT_NONE);
  if(status != 0){ /*---*/ }
  status = mprotect(p + page + aligned_size, page, PROT_NONE);
  if(status != 0){ /*---*/ }
  return (p + page);
}

static void deallocate_protected_space(char* p, int size) {
  int page = getpagesize();
  int status;
  int aligned_size = ((size + page - 1) / page) * page;
  status = munmap(p - page, aligned_size + 2 * page);
  if(status != 0) { /*---*/ }
}

void print_ptr(int val) {
  if((val & fixnum_mask) == fixnum_tag) {
    printf("%d", val >> fixnum_shift);
  } else if(val == bool_false) {
    printf("#f");
  } else if(val == bool_true) {
    printf("#t");
  } else if(val == empty_list) {
    printf("()");
  } else if((val & char_mask) == char_tag) {
    printf("%c", val >> char_shift);
  }
  printf("\n");
}

int main(int argc, char **argv) {
  int stack_size = (16 * 4096); /* holds 16K cells */
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  print_ptr(scheme_entry(stack_base));
  deallocate_protected_space(stack_top, stack_size);
  return 0;
}
