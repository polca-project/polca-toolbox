// FEAT_VECTOR: [1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 6, 0, 0, 7, 1, 0]
// TEST_VECTOR: [1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 6, 0, 0, 7, 1, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             1
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
//# totalNumForLoops:            6
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            7
//# anyTernaryOp:                1
//# anyUselessStmt:              0


void rgbImageFilter(char* image,int width,int height,char **redImage,char **greenImage,char **blueImage)
{
  unsigned rawWidth = width * 3;
  /* printf("Applying red color filter...\n"); */
  for (int i = 0; i < height; i++)
    {
      /* kernelRedFilter(image,i,rawWidth,redImage); */
      for(int k=0;k<(rawWidth/3)*3;k++) {
	(*redImage)[i*rawWidth+k] = (k%3)==0?image[i*rawWidth+k]:0;
      }
    }
  /* printf("Applying green color filter...\n"); */
  for (int i = 0; i < height; i++)
    {
      /* kernelGreenFilter(image,i,rawWidth,greenImage); */
      for(int k=0;k<(rawWidth/3)*3;k++) {
	(*greenImage)[i*rawWidth+k] = (k%3)!=1?0:image[i*rawWidth+k];
      }
    }
  /* printf("Applying blue color filter...\n"); */
  for (int i = 0; i < height; i++)
    {
      /* kernelBlueFilter(image,i,rawWidth,blueImage); */
      for(int k=0;k<(rawWidth/3)*3;k++) {
	(*blueImage)[i*rawWidth+k] = (k%3)!=2?0:image[i*rawWidth+k];
      }
    }
}
