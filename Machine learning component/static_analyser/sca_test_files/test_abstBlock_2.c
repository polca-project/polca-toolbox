// FUNC_ANALYZ: main BLOCK_ABS
// FEAT_VECTOR: [-1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
// TEST_VECTOR: [-1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:            -1
//# anyFuncCall:                 1
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              1
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:       0
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            0
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            1
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostamble              0
//# numForPreamble               0
//# numStructVariables           0



// #include "traps.h"

// Added just for measure time
#include "timer.h"

#include "params.h"

int image[N][N];
int result[N][N];

int applyThreshold(int value, int threshold) {

  return (value > threshold) ? 255 : 0;
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
      aux = applyThreshold(input_image[r][c],threshold); 
      output_image[r][c]= aux;
    } 
  }
}

int main() {

  /* Read the image */
  char fileName[80];
  sprintf(fileName, "%s/%s",DATA_PATH,INPUT_FILE);
  input_dsp_arg(fileName,image, N*N, 1);

#if PROFILING == 1
 for(int sample=0;sample<SAMPLES;sample++){
    StartTimer();
#endif

#pragma polca def BLOCK_ABS
{
  int thresholdValue = T;
  threshold(image,thresholdValue,result);
}

#if PROFILING == 1 
  GetTimer();
 } // End of loop for time samples
#endif

  /* Store the output */

 sprintf(fileName, "%s/output.dsp",DATA_PATH);
 output_dsp_arg(fileName,result,N*N,1);
  return 0;

}
