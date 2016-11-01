#include <stdio.h>
#include <math.h>

#define N 11

int main(int argc, char *argv[])
{
  int a[N],b[N],i;
  float c;

  c = 0.0;

   for(i=0;i<N;i++)
   {
     a[i] = b[i] = (float)i+1;
   }

   for(i=0;i<N;i++)
   {
     c += a[i] * b[i];
   }

   printf("%f, ",c);
   printf("\n");
   
}
