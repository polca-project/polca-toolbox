// FEAT_VECTOR: [1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 2, 1, 0, 2, 0, 0, 0, 0, 0]
// TEST_VECTOR: [1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 2, 1, 0, 2, 0, 0, 0, 0, 0]
// TEST_LABEL: 7 (GPU/OpenMP/MPI)

//# maxForStmtDepth:             1
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               1
//# numLoopHoistedVarMods:       1
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            2
//# numNonNormalizedForLoops:    1
//# numStmtsRollUp:              0
//# numCompoundStmts:            2
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostamble              0
//# numForPreamble               0
//# numStructVariables           0



#define N 5
int main()
{
  int i,tot=0;
  int arr[N*3];
  
  
  int j;
  for(i=0;i < N;i=i+3)
  {
    for(j=0;j<3;j++)
      arr[i*3+j]    = (i+j) * tot;
    tot += 1;
  }
  
}
