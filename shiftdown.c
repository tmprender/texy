#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
   char *shiftdown(char *s, int t){
   char byte[9];
  
   size_t len = strlen(s);
   //char *binary = malloc(len + 1);
   char *binary = malloc(len + 1);
   char *input = s; 
 
   long c;
   unsigned char d;
   int k;
   int index = 0;
   char *start = binary;
   char result[len]; 
   printf("%lu\n", len);
   for(k = 0; k < len; k += 8) {    
   	int remainder = 0;
   	int temp = 0;
   	int decimalnum = 0;
        
	memcpy(byte, &input[k], 8);
	byte[8] = '\0';


		
	c = atoi(byte); 
        
        //printf("%lu\n", c);
        while (c != 0)
        {
           remainder = c % 10;
           c = c / 10;
           decimalnum = decimalnum + remainder * pow(2, temp);
           temp++;
    	}
		
   	//printf("%d\n", decimalnum);
	if(decimalnum == 32){
	   c = decimalnum;
	   result[index] = c; 
	   *binary++ = result[index++];  
           result[index] = '\0';
	}
	else{	
	c = decimalnum - (t % 128);
	
	result[index] = c; 
	*binary++ = result[index++];  
        result[index] = '\0';
        //printf("%s\n", result);      
   	}
   }
   *binary = '\0';
   binary = start;
   return binary;

}


int main(int argc, char **argv)
{
  char *output;
  if (argc != 3) {
    fprintf(stderr, "Usage: %s string\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  output = shiftdown(argv[1], atoi(argv[2]));
  printf("%s\n", output);

  //printf("%s\n", output + 1);
 
  free(output);
  exit(EXIT_SUCCESS);
}                                                             
