// FEAT_VECTOR: [0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0]
// TEST_VECTOR: [0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             0
//# anyFuncCall:                 1
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   1
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:       1
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            1
//# numNonNormalizedForLoops:    0


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


  sprintf(parallelFileName, "../data/output.dsp");
  sprintf(sequentialFileName, "../data/reference_output.dsp");
  printf("Comparing image %s ...",parallelFileName);
  numDiffPixels = compareImages(sequentialFileName,parallelFileName);
  if(numDiffPixels==0)
    printf(" Test passed!!\n");
  else
    printf(" Test failed!! (%d values differs)\n",numDiffPixels);

}
