#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

extern int program(int argc, int* argv);

int* oat_malloc(int size) {
  return (int*)malloc(size);
}

void oat_abort() {
  fprintf(stderr, "Out-of-bound.\n");
  exit(-1);
}

int length_of_array (int *arr) {
  assert (NULL != arr);
  return arr[-1];	  
}

int* array_of_string (int *str) {
  int len, i, *arr;
  char *p;

  assert (NULL != str);

  len = *(str-1);
  assert (len >= 0);

  arr = (int*)malloc(sizeof(int) * (len+1));
  arr[0] = len;
  p = (char*) str;
  for (i=0; i<len; i++) {
    arr[i+1]=(int)p[i];
  }

  return arr+1; 
}

int* string_of_array (int *arr) {
  int len, i;
  char *str;

  assert (NULL != arr);

  len = *(arr-1);
  assert (len >= 0);

  str = malloc(sizeof(int) + sizeof(char) * (len+1));
  *((int*)str) = len;

  for (i=0; i<len; i++) {
    str[sizeof(int)+i] = (char)arr[i];
    assert (0 != str[sizeof(int)+i]);
  }
  str[sizeof(int)+len] = 0;

  return (int*)(str+4);
}

void print_string (int* str) {
  assert (NULL != str);
  printf ("%s", (char*)str);
}

void print_int (int i) {
  printf ("%d", i);
}

void print_bool (int i) {
  printf ("%d", i);
}

int* alloc_array (int size) {
  assert (size >= 0);
  int *arr = (int*)malloc(sizeof(int) * (size+1));
  arr[0] = size;
  return arr+1;
}	

int main(int argc, char* argv[]) {
  int *oargv, i, result;

  oargv = (int*)malloc(sizeof(int) + sizeof(int*) * argc);
  oargv[0] = argc;

  for (i=0; i<argc; i++){
    int len, j;
    char *p;

    len = strlen(argv[i]);
    p = (char*)malloc(sizeof(int) + sizeof(char) * (len + 1));
    *((int*)p) = len;
    for (j=0; j<len; j++){
      p[sizeof(int)+j] = argv[i][j];
    }    
    p[sizeof(int)+len] = 0;
    oargv[1+i] = (int)(p+4);
  }

  result = program(argc, oargv+1);
  printf("%d\n", result);
  return result;
}
