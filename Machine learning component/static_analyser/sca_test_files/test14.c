// FEAT_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, 1, 0]
// TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 3, 2, 1, 0]
// TEST_LABEL: 15 (FPGA/GPU/OpenMP/MPI)

//# maxForStmtDepth:             0
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:       0
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            1
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              3
//# numCompoundStmts:            2
//# anyTernaryOp:                1
//# anyUselessStmt:              0


void rgbImageFilter(char* image,int width,int height,char *redImage,char *greenImage,char *blueImage)
{
  unsigned rawWidth = width * 3;
  /* printf("Applying red color filter...\n"); */
  /* printf("Applying blue color filter...\n"); */
  /* printf("Applying green color filter...\n"); */
  for(int k=0;k<(rawWidth/3)*3*height;k++) {
    redImage[k] = (k%3)==0?image[k]:0;
    greenImage[k] = (k%3)==1?image[k]:0;
    blueImage[k] = (k%3)==2?image[k]:0;
  }
}
