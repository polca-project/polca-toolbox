// FEAT_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 3, 0, 0, 4, 3, 0, 3, 3, 0, 0]
// TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 3, 0, 0, 4, 3, 0, 3, 3, 0, 0]
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
//# totalNumForLoops:            3
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            4
//# anyTernaryOp:                3
//# anyUselessStmt:              0
//# numForPostambles:            3
//# numForPreambles:             3
//# numStructVarDecl:            0
//# numEmptyIf:                  0



void rgbImageFilter(char* image,int width,int height,char **redImage,char **greenImage,char **blueImage)
{
  unsigned rawWidth = width * 3;
  /* printf("Applying red color filter...\n"); */
      for(int k=0;k<(rawWidth/3)*3*height;k++) {
	(*redImage)[k] = (k%3)==0?image[0*rawWidth+k]:0;
      }
   
  /* printf("Applying green color filter...\n"); */
      for(int k=0;k<(rawWidth/3)*3*height;k++) {
	(*greenImage)[k] = (k%3)!=1?0:image[0*rawWidth+k];
      }
  /* printf("Applying blue color filter...\n"); */
      for(int k=0;k<(rawWidth/3)*3*height;k++) {
	(*blueImage)[k] = (k%3)!=2?0:image[0*rawWidth+k];
      }
}
