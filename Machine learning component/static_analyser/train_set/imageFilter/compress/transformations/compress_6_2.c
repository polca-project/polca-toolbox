/* This program uses the discrete cosine transform to compress a 128 x 128
   pixel image by a factor of 4:1 while preserving its information content.
   Block encoding is used with a block size of 8 x 8.

   This program is based on the routines and algorithms found in the book
   "C Language Algorithms for Digital Signal Processing" By P. M. Embree
   and B. Kimble.

   Copyright (c) 1992 -- Mazen A.R. Saghir -- University of Toronto  */
/* Modified to use arrays - SMP */


#include "traps.h"

// Added just for measure time
#include <sys/time.h>

#define         ROUND(f)        (((f)<0.0) ? (int)((f)-0.5) : (int)((f)+0.5))
#define         N               128
#define         B               8
#define         P               1024

int image[N][N];                /* 2D array that stores the original image. */
int block[B][B];                /* 2D array that stores an image block. */
float cos1[B][B];               /* 2D array that stores cosine coefficients. */
//float cos2[B][B];               /* 2D array that is the transpose of cos1. */
float temp2d[B][B];             /* Temporary 2D array. */
int result[16][16][4];          /* Result array */

void dct(int block[B][B]);

main()
{
  float factor1;
  float factor2;
  float temp_cos;

  int k;
  int l;
  int m;
  int n;

  int pack2in8();
  int pack4in32();

  /* Read the image */

  input_dsp(image, 16384, 1);

  struct timeval tvalBefore, tvalAfter;
  long total;
  int SAMPLES = 30;
 for(int i=0;i<SAMPLES;i++){
	  gettimeofday(&tvalBefore,NULL);

  /* Initialize the cosine matrices. "cos2" is the transpose of "cos1" */
  factor1 = 2.0 * atan(1.0) / B;
  factor2 = 0.0;
  for (m = 0; m < B; ++m) {
    for (n = 0; n < B; ++n) {
      temp_cos = cos(factor2 * (2*n + 1)) / B;
      cos1[m][n] = temp_cos;
      //cos2[n][m] = temp_cos;
    }
    factor2 += factor1;
  }

  /* Each image is assumed to have a resolution of 128 x 128, with 256 gray
     levels. Since each block is 8 x 8, 256 iterations are needed to compress
     the image. This is the main loop. */


  for (m = 0; m < 16; m++) {

    for(n = 0; n < 16; n++) {

      /* Read next image block. */

      for (k = 0; k < B; k++) {
        for (l = 0; l < B; l++) {
          block[k][l] = image[B*m+k][B*n+l];
        }
      }

      /* Find its DCT */

      dct(block);

      /* Select coefficients, scale, and pack */

      result[m][n][0] = pack4in32(block[0][0],block[0][1],
				  pack2in8(block[0][2] / 3, block[0][3] / 3),
				  pack2in8(block[0][4] / 2, block[0][5] / 2));

      result[m][n][1] = pack4in32(pack2in8(block[0][6],block[0][7]),
				  block[1][0],block[1][1],
				  pack2in8(block[1][2] / 2,block[1][3]));

      result[m][n][2] = pack4in32(pack2in8(block[1][4], block[1][5]),
				  pack2in8(block[2][0] / 3, block[2][1] / 2),
				  pack2in8(block[2][2] / 2, block[2][3]),
				  pack2in8(block[3][0] / 3, block[3][1]));

      result[m][n][3] = pack4in32(pack2in8(block[3][2], block[3][3]),
				  pack2in8(block[4][0] / 2, block[4][1]),
				  pack2in8(block[5][0] / 2, block[5][1]),
				  pack2in8(block[6][0] , block[7][0]));
    }

  }

	  gettimeofday(&tvalAfter,NULL);
	  total += ((tvalAfter.tv_sec - tvalBefore.tv_sec)*1000000L+tvalAfter.tv_usec) - tvalBefore.tv_usec;
	  printf("it: %i, time: %ld microseconds\n",i,total);
 } // End of loop for time samples

  /* Store processed block */

  output_dsp(result,1024,1);




}




/* This function calculates the discrete cosine transform of an image block.
   The result is returned in the same matrix as the original block. */

void dct(int block[B][B])
{
  int i;
  int j;
  int k;
  float sum;

  /* Multiply the input image block with the cos2 matrix; store the result
     in the temporary matrix "temp2d". */

  for (i = 0; i < B; i++) {
    for (j = 0; j < B; j++) {
      sum = 0.0;
      for (k = 0; k < B; k++) {
        sum += block[i][k] * cos1[j][k];
      }
      temp2d[i][j] = sum;
    }
  }


  /* Multiply the cosine matrix by the temporary matrix; store the
     result back in the original matrix.  */

  for (i = 0; i < B; i++) {  /* advances cos1 row */
    for (j = 0; j < B; j++) {  /* no change */
      sum = 0.0;
      for (k = 0; k < B; k++) {  /* advances cos1 col */
        sum += cos1[i][k] * temp2d[k][j] ;
      }
      /* round the result */
      block[i][j] = ROUND(sum);
    }
  }

}


int pack2in8(int a, int b)
{
  /* limit to signed 4 bits */

  a &= 0x0f;
  b &= 0x0f;
  return((a << 4) | (b));
}

int pack4in32(int a,int b,int c,int d)
{
  /* limit to signed 8 bits */

  a &= 0xff;
  b &= 0xff;
  c &= 0xff;
  d &= 0xff;

  return((a << 24) | (b << 16) | (c << 8) | (d));
}
