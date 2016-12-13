
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/* Assumes that float is in the IEEE 754 single precision floating point format
 * and that int is 32 bits. */
float sqrt_approx(float z)
{
    int val_int = *(int*)&z; /* Same bits, but as an int */
    /*
     * To justify the following code, prove that
     *
     * ((((val_int / 2^m) - b) / 2) + b) * 2^m = ((val_int - 2^m) / 2) + ((b + 1) / 2) * 2^m)
     *
     * where
     *
     * b = exponent bias
     * m = number of mantissa bits
     *
     * .
     */

    val_int -= 1 << 23; /* Subtract 2^m. */
    val_int >>= 1; /* Divide by 2. */
    val_int += 1 << 29; /* Add ((b + 1) / 2) * 2^m. */

    return *(float*)&val_int; /* Interpret again as float */
}


/*Returns the square root of n. Note that the function */
float sqRt(float n) {
  /*We are using n itself as initial approximation
   This can definitely be improved */
  float x = n;
  float y = 1;
  float e = 0.00000001; /* e decides the accuracy level*/
  int numIt = 0;
  while(x - y > e)
  {
    x = (x + y)/2;
    y = n/x;
    numIt++;
  }
printf("\nSqrt computed in %d iterations\n",numIt);
  return x;
}

int main() {

float dx = -0.999984 * -0.999984;
float dy = -0.736924 * -0.736924;
float dz = 0.511211 * 0.511211;
float distSqr = dx*dx + dy*dy + dz*dz;

  float s = sqrtf(distSqr);
  float r = sqRt(distSqr);
  

printf("\n%f - %f\n\n",s,r);

  return 0;

}
