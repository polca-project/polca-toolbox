// FEAT_VECTOR: [1, 0, 0, 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 6, 0, 0, 5, 0, 0]
// TEST_VECTOR: [1, 0, 0, 2, 0, 1, 1, 0, 0, 0, 0, 0, 0, 6, 0, 0, 5, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             1
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        2
//# usesGlobalVars:              0
//# anyIfStmt:                   1
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
//# numCompoundStmts:            5
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
    if(tot > N)
    {
      for(j=0;j<N;j++)
	tot += 1;
    }else
      break;
  }
  for(j=0;j<N;j++)
  {
    if(tot < N)
      tot += 1;
    else
      continue;
  }
  for(i=0;i<N;i++)
  {
    for(j=0;j<N;j++)
      tot += 1;
  }
    
}
