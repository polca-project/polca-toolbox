// FEAT_VECTOR: [0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 13, 2, 0, 0]
// TEST_VECTOR: [0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 13, 2, 0, 0]
// TEST_LABEL: 5 (GPU/OpenMP)

//# maxForStmtDepth:             0
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        1
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
//# numStmtsRollUp:             13
//# numCompoundStmts:            2
//# anyTernaryOp:                0
//# anyUselessStmt:              0


void rgbImageFilter(char* image,int width,int height,char **redImage,char **greenImage,char **blueImage)
{
  unsigned rawWidth = width * 3;
  char r,g,b;
  char r1,g1,b1;
  char r2,g2,b2;
  for (int j = 0; j < height * (rawWidth/3); j++)
    {
  	  r = image[j*3];
  	  g = image[j*3+1];
  	  b = image[j*3+2];
  	  (*redImage)[j*3] = r;
  	  (*redImage)[j*3+1] = 0;
  	  (*redImage)[j*3+2] = 0;
  	  r1 = image[j*3];
  	  g1 = image[j*3+1];
  	  b1 = image[j*3+2];
  	  (*greenImage)[j*3] = 0;
  	  (*greenImage)[j*3+1] = g1;
  	  (*greenImage)[j*3+2] = 0;
  	  r2 = image[j*3];
  	  g2 = image[j*3+1];
  	  b2 = image[j*3+2];
  	  (*blueImage)[j*3] = 0;
  	  (*blueImage)[j*3+1] = 0;
  	  (*blueImage)[j*3+2] = b2;
    }
}
