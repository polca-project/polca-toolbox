// FEAT_VECTOR: [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 3, 0, 0, 3, 0, 0, 1, 0, 0]
// TEST_VECTOR: [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 3, 0, 0, 3, 0, 0, 1, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             0
//# anyFuncCall:                 1
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   0
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:       4
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            3
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            3
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostamble              1
//# numForPreamble               0
//# numStructVariables           0



#define N 5
int foo(void)
{
    return N;
}
int main()
{
  int i,tot=0;
  int n = N;
  for(i=0;i<N;i++)
    tot += 1;
  for(i=0;i < n;i++)
  {
    tot += 1;
    n = foo();
  }
  for(i=0;i<n;i++)
    tot += 1;
    
}
