#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
char *bincon(char *s)
{
   char byte[9];
  
   size_t len = strlen(s);
   //char *binary = malloc(len + 1);
   char *binary = malloc(len + 1);
   const char *input = s; 
 
   unsigned char c;
   int k;
   int index = 0;

   char *start = binary;
   char result[256]; 
   for(k = 0; k < len; k += 8) {    
        memcpy(byte, &input[k], 8);
        byte[8] = '\0';
	c = (unsigned char)strtol(byte, 0, 2); 
        result[index] = c; 
        *binary++ = result[index++];  
        result[index] = '\0';
        //printf("%s\n", result);      
   }
   *binary = '\0';
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

  output = bincon(argv[1]);
  printf("%s\n", output);

  //printf("%s\n", output + 1);
 
  free(output);
  exit(EXIT_SUCCESS);
}                                                             
