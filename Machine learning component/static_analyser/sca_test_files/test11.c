// FEAT_VECTOR: [0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 3, 0, 0, 4, 0, 0, 6, 9, 0, 0]
// TEST_VECTOR: [0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 3, 0, 0, 4, 0, 0, 6, 9, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             0
//# anyFuncCall:                 1
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
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostambles:            6
//# numForPreambles:             9
//# numStructVarDecl:            0
//# numEmptyIf:                  0



void rgbImageFilter(char* image,int width,int height,char **redImage,char **greenImage,char **blueImage)
{
  unsigned rawWidth = width * 3;
  printf("Applying red color filter...\n");
  for (int i = 0; i < height; i++)
    {
      kernelRedFilter(image,i,rawWidth,redImage);
    }
  printf("Applying green color filter...\n");
  for (int i = 0; i < height; i++)
    {
      kernelGreenFilter(image,i,rawWidth,greenImage);
    }
  printf("Applying blue color filter...\n");
  for (int i = 0; i < height; i++)
    {
      kernelBlueFilter(image,i,rawWidth,blueImage);
    }
}
