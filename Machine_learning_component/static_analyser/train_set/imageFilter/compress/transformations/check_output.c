/* 
 * Copyright (c) 2014, IMDEA Software Institute,
 * web: software.imdea.org
 *
 * Any reproduction in whole or in parts is prohibited
 * without the written consent of the copyright owner.
 *
 * All Rights Reserved.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define OUT_SIZE 1024

/* Function to compare two images */
int compareImages(char *imgFileName_sequential, char *imgFileName_parallel)
{
  int i,j,k;
  /* int img_sequential[16][16][4]; */
  /* int img_parallel[16][16][4]; */
  int img_sequential[OUT_SIZE];
  int img_parallel[OUT_SIZE];


  int pixel_count = 0;

  input_dsp_arg(imgFileName_sequential,img_sequential,OUT_SIZE,1);
  input_dsp_arg(imgFileName_parallel,img_parallel,OUT_SIZE,1);

  /* Compare images pixel-wise */
  for (i = 0; i < OUT_SIZE; i++) {
        if( img_sequential[i] != img_parallel[i])
  	{
  	  printf("%d - %d \n",img_sequential[i],img_parallel[i]);
          pixel_count++;

        }
  }

  /* for (i = 0; i < 16; i++) { */
  /*   for (j = 0; j < 16; j++) { */
  /*     for (k = 0; k < 4; k++) { */
  /*       if( img_sequential[i][j][k] != img_parallel[i][j][k]) */
  /* 	{ */
  /* 	  printf("%d - %d \n",img_sequential[i][j][k],img_parallel[i][j][k]); */
  /*         pixel_count++; */

  /*       } */
  /*     } */
  /*   } */
  /* } */


  return pixel_count;
}

int main(int argc, char* argv[])
{
  char parallelFileName[80],sequentialFileName[80];

  int numDiffPixels;
  int ret;

  sprintf(parallelFileName, "../data/output.dsp");
  sprintf(sequentialFileName, "../data/reference_output.dsp");
  printf("Comparing image %s ...",parallelFileName);
  numDiffPixels = compareImages(sequentialFileName,parallelFileName);
  if(numDiffPixels==0){
    printf(" Test passed!!\n");
    ret = 0;
  }else{
    printf(" Test failed!! (%d values differs)\n",numDiffPixels);
    ret = 1;
  }

  printf("%d\n",ret);
  return 0;

}
