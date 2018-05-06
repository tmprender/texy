#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
char *bitflip(char *s)
{
  if (s == NULL) {
    return NULL;
  }
  
// get length of string without NUL
  int slen = strlen(s);


  errno = 0;
  // allocate "slen" (number of characters in string without NUL)
  
  char *binary = malloc(slen + 1);
  if(binary == NULL){
     fprintf(stderr,"malloc has failed in stringToBinary(%s): %s\n",s, strerror(errno));
     return NULL;
  }
  // finally we can put our shortcut from above here
  if (slen == 0) {
    *binary = '\0';
    return binary;
  }
  char *ptr;
  // keep an eye on the beginning
  char *start = binary;
  int i;

  // loop over the input-characters
  for (ptr = s; *ptr != '\0'; ptr++) {
    
    // loop over the input-character bits
    //for (i = slen; i >= 0; i--, binary++) {
        if(*ptr == '1') *binary++ = '0';
        if(*ptr == '0') *binary++ = '1';
	
    //}
  }
  // finalize return value
  *binary = '\0';
  // reset pointer to beginning
  binary = start;
  return binary;
}


int main(int argc, char **argv)
{
  char *output;
  if (argc != 2) {
    fprintf(stderr, "Usage: %s string\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  // TODO: check argv[1]
  output = bitflip(argv[1]);
  printf("%s\n", output);

  //printf("%s\n", output + 1);
 
  free(output);
  exit(EXIT_SUCCESS);
}
