#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
char *conbin(char *s)
{
  if (s == NULL) {
    // NULL might be 0 but you cannot be sure about it
    return NULL;
  }
  // get length of string without NUL
  size_t slen = strlen(s);

  // we cannot do that here, why?
  // if(slen == 0){ return s;}

  errno = 0;
  // allocate "slen" (number of characters in string without NUL)
  // times the number of bits in a "char" plus one byte for the NUL
  // at the end of the return value
  char *binary = malloc(slen * CHAR_BIT + 1);
  if(binary == NULL){
     fprintf(stderr,"malloc has failed in conbin(%s): %s\n",s, strerror(errno));
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
    /* perform bitwise AND for every bit of the character */
    // loop over the input-character bits
    for (i = CHAR_BIT - 1; i >= 0; i--, binary++) {
      *binary = (*ptr & 1 << i) ? '1' : '0';
    }
  }
  // finalize return value
  *binary = '\0';
  // reset pointer to beginning
  binary = start;
  return binary;
}

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

char *binshift(char *s, int t)
{
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
   //printf("%lu\n", len);
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
	c = decimalnum + (t % 128);
	
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

char *concat(char *a, char *b) {
  if (a == NULL || b == NULL) {
    // NULL might be 0 but you cannot be sure about it
    return NULL;
  }

  errno = 0;
  size_t len = strlen(a) + strlen(b);
  char *c = malloc(len+1);
  if(c == NULL){
     fprintf(stderr,"malloc has failed in concat(%s.%s): %s\n",a,b, strerror(errno));
     return NULL;
  }  
  c[len] = '\0';
  c = strcat(c, a);
  c = strcat(c, b);
  return c;
}



/*
int main(int argc, char **argv)
{
  char *output;
  if (argc != 2) {
    fprintf(stderr, "Usage: %s string\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  // TODO: check argv[1]
  output = conbin(argv[1]);
  printf("%s\n", output);

  //printf("%s\n", output + 1);
 
  free(output);
  exit(EXIT_SUCCESS);
} */
