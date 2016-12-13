/* This program uses the discrete cosine transform to compress a 128 x 128
   pixel image by a factor of 4:1 while preserving its information content.
   Block encoding is used with a block size of 8 x 8.

   This program is based on the routines and algorithms found in the book
   "C Language Algorithms for Digital Signal Processing" By P. M. Embree
   and B. Kimble.

   Copyright (c) 1992 -- Mazen A.R. Saghir -- University of Toronto  */
/* Modified to use arrays - SMP */

// Added just for measure time
#include <sys/time.h>

#include "traps.h"

#define         ROUND(f)        (((f)<0.0) ? (int)((f)-0.5) : (int)((f)+0.5))
#define         N               128
#define         B               8
#define         P               1024

int image[N][N];                /* 2D array that stores the original image. */
int block[B][B];                /* 2D array that stores an image block. */
float cos1[B*B];               /* 2D array that stores cosine coefficients. */
float cos2[B*B];               /* 2D array that is the transpose of cos1. */
float temp2d[B*B];             /* Temporary 2D array. */
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
      cos1[m*B+n] = temp_cos;
      cos2[n*B+m] = temp_cos;
    }
    factor2 += factor1;
  }

	  gettimeofday(&tvalAfter,NULL);
	  total = ((tvalAfter.tv_sec - tvalBefore.tv_sec)*1000000L+tvalAfter.tv_usec) - tvalBefore.tv_usec;
  /* Read the image */

  input_dsp(image, 16384, 1);

  /* Each image is assumed to have a resolution of 128 x 128, with 256 gray
     levels. Since each block is 8 x 8, 256 iterations are needed to compress
     the image. This is the main loop. */

	  gettimeofday(&tvalBefore,NULL);

  int z;
  for (z = 0; z < 16 * 16; z++) {
  /* for (m = 0; m < 16; m++) { */

  /*   for(n = 0; n < 16; n++) { */

      /* Read next image block. */

//      for (k = 0; k < B; k++) {
//        for (l = 0; l < B; l++) {
//          block[k][l] = image[B*(z/16)+k][B*(z%16)+l];
//        }
//      }


      /* Find its DCT */
/////////////////////////////////////////////////
      int i;
      int j;
//      int k;
      float sum;

      /* Multiply the input image block with the cos2 matrix; store the result
         in the temporary matrix "temp2d". */

      for (i = 0; i < B; i++) {
        for (j = 0; j < B; j++) {
          sum = 0.0;
          for (k = 0; k < B; k++) {
            sum += image[B*(z/16)+i][B*(z%16)+k] * cos2[k*B+j];
          }
          temp2d[i*B+j] = sum;


      /* Multiply the cosine matrix by the temporary matrix; store the
         result back in the original matrix.  */
          sum = 0.0;
          for (k = 0; k < B; k++) {  /* advances cos1 col */
            sum += cos1[i*B+k] * temp2d[k*B+j] ;
          }
          /* round the result */
          image[B*(z/16)+i][B*(z%16)+j] = ROUND(sum);
        }

        /* for (j = 0; j < B; j++) {  /\* no change *\/ */

        /* } */
      }


      /* for (i = 0; i < B; i++) {  /\* advances cos1 row *\/ */

      /* } */
/////////////////////////////////////////////////
  }

  for (z = 0; z < 16 * 16; z++) {
      /* Select coefficients, scale, and pack */

      result[(z/16)][(z%16)][0] = pack4in32(image[B*(z/16)+0][B*(z%16)+0],image[B*(z/16)+0][B*(z%16)+1],
				  pack2in8(image[B*(z/16)+0][B*(z%16)+2] / 3, image[B*(z/16)+0][B*(z%16)+3] / 3),
				  pack2in8(image[B*(z/16)+0][B*(z%16)+4] / 2, image[B*(z/16)+0][B*(z%16)+5] / 2));

      result[(z/16)][(z%16)][1] = pack4in32(pack2in8(image[B*(z/16)+0][B*(z%16)+6],image[B*(z/16)+0][B*(z%16)+7]),
				  image[B*(z/16)+1][B*(z%16)+0],image[B*(z/16)+1][B*(z%16)+1],
				  pack2in8(image[B*(z/16)+1][B*(z%16)+2] / 2,image[B*(z/16)+1][B*(z%16)+3]));

      result[(z/16)][(z%16)][2] = pack4in32(pack2in8(image[B*(z/16)+1][B*(z%16)+4], image[B*(z/16)+1][B*(z%16)+5]),
				  pack2in8(image[B*(z/16)+2][B*(z%16)+0] / 3, image[B*(z/16)+2][B*(z%16)+1] / 2),
				  pack2in8(image[B*(z/16)+2][B*(z%16)+2] / 2, image[B*(z/16)+2][B*(z%16)+3]),
				  pack2in8(image[B*(z/16)+3][B*(z%16)+0] / 3, image[B*(z/16)+3][B*(z%16)+1]));

      result[(z/16)][(z%16)][3] = pack4in32(pack2in8(image[B*(z/16)+3][B*(z%16)+2], image[B*(z/16)+3][B*(z%16)+3]),
				  pack2in8(image[B*(z/16)+4][B*(z%16)+0] / 2, image[B*(z/16)+4][B*(z%16)+1]),
				  pack2in8(image[B*(z/16)+5][B*(z%16)+0] / 2, image[B*(z/16)+5][B*(z%16)+1]),
				  pack2in8(image[B*(z/16)+6][B*(z%16)+0] , image[B*(z/16)+7][B*(z%16)+0]));
    /* } */

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
        sum += block[i][k] * cos2[k*B+j];
      }
      temp2d[i*B+j] = sum;
    }
  }


  /* Multiply the cosine matrix by the temporary matrix; store the
     result back in the original matrix.  */

  for (i = 0; i < B; i++) {  /* advances cos1 row */
    for (j = 0; j < B; j++) {  /* no change */
      sum = 0.0;
      for (k = 0; k < B; k++) {  /* advances cos1 col */
        sum += cos1[i*B+k] * temp2d[k*B+j] ;
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
