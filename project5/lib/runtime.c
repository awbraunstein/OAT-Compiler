#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#define ostring int*
#define length_of_ostring(s) s[-1]
#define ostring_index(s,i) (((char*)s)[i])

extern int program(int argc, int argv);

int* oat_malloc(int size) {
  return (int*)malloc(size);
}

void oat_abort(int errno) { 
  if (-1 == errno) {}	
  else if (0 == errno) fprintf(stderr, "Out-of-bound.\n");
  else if (1 == errno) fprintf(stderr, "Uninitialized non-null vars.\n");
  else fprintf(stderr, "Unknown error: %d.\n", errno);
  exit(-1);
}

int int_of_bool(int b) {
  return b;
}

int bool_of_int(int i) {
  return i;
}

int* alloc_array (int size) {
  assert (size >= 0);
  int *arr = (int*)malloc(sizeof(int) * (size+1));
  arr[0] = size;
  return arr+1;
}	

int length_of_array (int *arr) {
  assert (NULL != arr);
  return arr[-1];	  
}

int* array_of_string (ostring str) {
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

ostring string_alloc(int size) {
  char* str = (char*)malloc(sizeof(int) + sizeof(char) * (size + 1));
  ((int*)str)[0] = size;
  str[sizeof(int) + size] = 0;
  return (ostring)(str + sizeof(int));
}

int* string_of_array (ostring arr) {
  int len, i;
  char *str;

  assert (NULL != arr);

  len = *(arr-1);
  assert (len >= 0);

  str = (char*)string_alloc(len);
  for (i=0; i<len; i++) {
    str[i] = (char)arr[i];
    assert (0 != str[i]);
  }

  return (int*)str;
}

int length_of_string(ostring s) {
  return s[-1];
}

ostring string_of_int(int i) {
  static char buf[64];
  static int len;
  ostring str;
  len = sprintf(buf,"%d",i);
  str = string_alloc(len);
  memcpy(str, buf, len+1);
  return str;
}

int int_of_string(ostring s) {
  return atoi((const char*)s);
}

ostring string_of_bool(int b) {
  return string_of_int(b);
}

int bool_of_string(ostring s) {
  if (0 == int_of_string(s)) 
    return 0;
  else
    return 1;
}

ostring string_of_cstring(const char* cs) {
  int len;
  ostring str;
  len = strlen(cs);
  str = string_alloc(len);
  memcpy(str, cs, len);
  return str;
}

int string_at(ostring s, int i) {
  if (s == 0 || i < 0 || i >= length_of_string (s)) {
    printf("string_at: string null or out of bounds.\n");
    exit(-1);
  }
  return s[i];
}

void string_set(ostring s, int i, int c) {
  if (s == 0 || i < 0 || i >= length_of_string (s)) {
    printf("string_set: string null or out of bounds.\n");
    exit(-1);
  }
  if (0 == c) {
    printf("string_set: char in string cannot be zero.\n");
    exit(-1);
  }
  s[i]=c;
}

ostring string_cat(ostring l, ostring r) {
  int len_l, len_r;
  ostring str;

  len_l = length_of_ostring(l);
  len_r = length_of_ostring(r);
  str = string_alloc(len_l + len_r);
  memcpy(str, (char*)l, len_l);
  memcpy((char*)str+len_l, (char*)r, len_r);
  return str;
}

void print_string (ostring str) {
  assert (NULL != str);
  printf ("%s", (char*)str);
}

void print_int (int i) {
  printf ("%d", i);
}

void print_bool (int i) {
  printf ("%d", i);
}

void nullchk(void* ptr) {
  if (ptr == 0) {
    printf("FATAL ERROR: null pointer dereference (in library code)\n");
    exit(-1);
  }
}

ostring read_file(ostring name) {
  FILE* f;
  nullchk(name);
  f = fopen((const char*)name, "rb");
  if (f) {
    int* outst;
    fseek(f, 0, SEEK_END);
    outst = string_alloc(ftell(f));
    fseek(f, 0, SEEK_SET);
    if (fread(outst, length_of_ostring(outst), 1, f) != 1) {
      fclose(f);
      return 0;
    }
    fclose(f);
    return outst;
  } else {
    return 0;
  }
}

int write_file(ostring name, ostring bits) {
  FILE* f;
  nullchk(name);
  nullchk(bits);
  f = fopen((const char*)name, "wb");
  if (f) {
    if (fwrite(bits, length_of_ostring(bits), 1, f) != 1) {
      fclose(f);
      return -1;
    }
    fclose(f);
    return 0;
  } else {
    return -1;
  }
}

int random_int() { return (rand() & 0x8FFFFFFF); }

int oat_div (int a, int b) {
  return a / b;
}

int oat_mod (int a, int b) {
  return a % b;
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
    oargv[1+i] = (int)(p+sizeof(int));
  }

  result = program(argc, (int)(oargv+1));
  printf("%d\n", result); 
  return result;
}
