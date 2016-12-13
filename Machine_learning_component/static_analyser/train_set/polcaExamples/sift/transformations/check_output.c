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
#include <assert.h>
#include "pgm_import.h"
#include "sift.h"

/* Function to compare two images */
int compareImages(char *imgFileName_sequential, char *imgFileName_parallel)
{
  int32_t i;
  int32_t imgSize_sequential,imgSize_parallel;
int pixel_count = 0;
//uint8_t fracLen = 20;
//int32_t seq_pixel,par_pixel;

  /*import image data*/
  GraylevelImage gray_sequential = DefaultPgmImage;
  if (readPgm(imgFileName_sequential, &gray_sequential)) {
    printf("ERROR: Could not read from %s\n", imgFileName_sequential);
    return -1;
  }
  imgSize_sequential = gray_sequential.width * gray_sequential.height;

  /*import image data*/
  GraylevelImage gray_parallel = DefaultPgmImage;
  if (readPgm(imgFileName_parallel, &gray_parallel)) {
    printf("ERROR: Could not read from %s\n", imgFileName_parallel);
    return -1;
  }

  imgSize_parallel = gray_parallel.width * gray_parallel.height;

  assert(imgSize_sequential  == imgSize_parallel);
  assert(gray_parallel.depth == gray_sequential.depth);

  /* Compare images pixel-wise */
  for (i = 0; i < imgSize_sequential; i++) {

        /* seq_pixel = fmul32((gray_sequential.data[i] << fracLen), */
        /* (double2fix((1./IMAGE_SCALING(gray_sequential.depth)), fracLen)), */
        /* fracLen); */

        /* par_pixel = fmul32((gray_parallel.data[i] << fracLen), */
        /* (double2fix((1./IMAGE_SCALING(gray_parallel.depth)), fracLen)), */
        /* fracLen); */

        /* if( abs(gray_sequential.data[i] - gray_parallel.data[i]) > 50) */
        /* if( abs(seq_pixel - par_pixel) > 50) */
        if( gray_sequential.data[i] != gray_parallel.data[i])
	{
          pixel_count++;
          /* printf("%d (%d,%d):  %d != %d\n",i,i/gray_parallel.width,i%gray_parallel.width,gray_sequential.data[i],gray_parallel.data[i]); */
          /* printf("%d:  %d != %d\n",i,gray_sequential.data[i],gray_parallel.data[i]); */
          /* printf("%d: %d != %d ->  %d != %d\n",i,seq_pixel,par_pixel,gray_sequential.data[i],gray_parallel.data[i]); */
          /* assert( gray_sequential.data[i] == gray_parallel.data[i]); */
        }
  }
  freePgm(&gray_sequential);
  freePgm(&gray_parallel);

  return pixel_count;
}

int main(int argc, char* argv[])
{
  char parallelFileName[80],sequentialFileName[80];

  int scale, octave;

  /* Parameters */
int noOfOctaves = 2;
int noOfScales = 5;
int numDiffPixels;

  for (octave = 0; octave < noOfOctaves; octave++) {

    for (scale = 0; scale < noOfScales; scale++) {

      sprintf(parallelFileName, "../data/gauss_%d_%d_out.pgm", octave, scale);
      sprintf(sequentialFileName, "../data/gauss_%d_%d_ref.pgm", octave, scale);
      printf("Comparing image %s ...",parallelFileName);
      numDiffPixels = compareImages(sequentialFileName,parallelFileName);
      if(numDiffPixels==0)
        printf(" Test passed!!\n");
      else
        printf(" Test failed!! (%d pixels differs)\n",numDiffPixels);
    }

    for (scale = 0; scale < noOfScales - 1; scale++) {
      sprintf(parallelFileName, "../data/dog_%d_%d_out.pgm", octave, scale);
      sprintf(sequentialFileName, "../data/dog_%d_%d_ref.pgm", octave, scale);
      printf("Comparing image %s ...",parallelFileName);
      numDiffPixels = compareImages(sequentialFileName,parallelFileName);
      if(numDiffPixels==0)
        printf(" Test passed!!\n");
      else
        printf(" Test failed!! (%d pixels differs)\n",numDiffPixels);
    }
  }

}
