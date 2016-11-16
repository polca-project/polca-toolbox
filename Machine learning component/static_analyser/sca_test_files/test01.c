// FEAT_VECTOR: [0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0]
// TEST_VECTOR: [0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             0
//# anyFuncCall:                 1
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              1
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
//# numStmtsRollUp:              0
//# numCompoundStmts:            2
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostamble              0
//# numForPreamble               0
//# numStructVariables           0



#define N 5

int glob_var = 0;

int foo(void)
{
    return 1;
}

int main()
{
  int i,tot=0;
  int n = N;

  if(tot=0)
    tot = 1;

  for(i=0;i<N;i++)
    tot += 1;
    
  glob_var = foo();
    
}
