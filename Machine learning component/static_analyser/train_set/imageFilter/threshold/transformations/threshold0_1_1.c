// FUNC_ANALYZ: threshold
// FEAT_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 2, 0, 2, 0]
// TEST_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 2, 0, 2, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             1
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        1
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               1
//# numLoopHoistedVarMods:       0
//# numNon1Darray:               2
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            2
//# numNonNormalizedForLoops:    0


// Added just for measure time
#include <sys/time.h>
#define PROFILING 1

#define         N               3000

int image[N][N];
int result[N][N];

int applyThreshold(int value, int threshold) {
  return value % threshold;
}

/* This function convolves the input image by the kernel and stores the result
   in the output image. */

void threshold(int input_image[N][N], int threshold, int output_image[N][N])
{

  int c;
  int r;
  /* int k=0; */
  /* int l=0; */
  /* int one = 1; */

  int aux;

  /* Convolve the input image with the kernel. */
  for (r = 0; r < N; r++) {
    for (c = 0; c < N; c++) {
      aux = input_image[r][c] % threshold; 
      output_image[r][c]= aux;
    }
  }
}

void main() {

  /* Read the image */

  input_dsp(image, N*N, 1);

#if PROFILING == 1
  struct timeval tvalBefore, tvalAfter;
  long total;
  int SAMPLES = 30;
 for(int i=0;i<SAMPLES;i++){
	  gettimeofday(&tvalBefore,NULL);

#endif

  int thresholdValue = 127;
  threshold(image,thresholdValue,result);


#if PROFILING == 1
	  gettimeofday(&tvalAfter,NULL);
	  total = ((tvalAfter.tv_sec - tvalBefore.tv_sec)*1000000L+tvalAfter.tv_usec) - tvalBefore.tv_usec;
	  /* printf("it: %i, time: %ld microseconds\n",i,total); */
	  printf("%ld\n",total);
 } // End of loop for time samples

#endif

  /* Store the output */

  output_dsp(result,N*N,1);

}
