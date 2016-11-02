// FEAT_VECTOR: [2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 7, 0, 0, 4, 0, 0]
// TEST_VECTOR: [2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 7, 0, 0, 4, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             2
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
//# totalNumForLoops:            7
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            4
//# anyTernaryOp:                0
//# anyUselessStmt:              0


#define N 5
int main()
{
  int i,j,k,tot;
  for(i=0;i<N;i++)
    tot += 1;
  for(i=0;i<N;i++)
  {
    for(j=0;j<N;j++)
    {
      for(k=0;k<N;k++)
	tot += 1;
    }
  }
  for(j=0;j<N;j++)
    tot += 1;
  for(i=0;i<N;i++)
  {
    for(j=0;j<N;j++)
      tot += 1;
  }
    
}
