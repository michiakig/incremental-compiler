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
#define heap_mask 0x7
#define pair_tag 0x1

// this is a bunch of voodoo that is never explained in Compilers:
// Backend to Frontend ... 

typedef struct {
  void* eax; /* 0    scratch */
  void* ebx; /* 4    preserve */
  void* ecx; /* 8    scratch */
  void* edx; /* 12    scratch */
  void* esi; /* 16    preserve */
  void* edi; /* 20    preserve */
  void* ebp; /* 24    preserve */
  void* esp; /* 28    preserve */
} context;

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

// prints a Scheme value, immediate or pointer to an object on the
// heap
void print_ptr(int val, int nl) {
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
  } else if((val & heap_mask) == pair_tag) {
    // print the car and the cdr recursively, as a dotted pair
    printf("(");
    print_ptr(*(int*)(val - 1), 0);
    printf(" . ");
    print_ptr(*(int*)(val + 3), 0);
    printf(")");
  }
  if(nl)
    printf("\n");
}

// allocate stack, heap and call scheme_entry which is the boilerplate
// that calls our actual compiled program
int main(int argc, char **argv) {
  int stack_size = (16 * 4096); /* holds 16K cells */
  char* stack_top = allocate_protected_space(stack_size);
  char* stack_base = stack_top + stack_size;
  int heap_size = (16 * 4096); /* also holds 16K cells, why not ? */
  char* heap = allocate_protected_space(heap_size);
  context ctxt;
  print_ptr(scheme_entry(&ctxt, stack_base, heap), 1);
  deallocate_protected_space(stack_top, stack_size);
  deallocate_protected_space(heap, heap_size);
  return 0;
}
