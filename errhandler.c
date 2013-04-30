#include <stdlib.h>
#include <ppl_c.h>

#define ERRBUFSIZE 256
char errMsg[ERRBUFSIZE];
char *lastError = NULL; 


void errorHandler(enum ppl_enum_error_code code, const char *description) {
   fprintf(stderr,"PPL error code %d\n%s", code, description);
   exit(-1);
}


/*
void printCS(ppl_const_ConSys_t x) {
  ppl_io_print_ConSys(x);
}
*/

/*
int splitDouble(double x, int *p, long long int *q) {
  return 0;
}
*/
