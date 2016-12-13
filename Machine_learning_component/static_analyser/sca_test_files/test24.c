// FEAT_VECTOR: [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 4, 0, 0, 6, 0, 0, 6, 10, 0, 0]
// TEST_VECTOR: [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 4, 0, 0, 6, 0, 0, 6, 10, 0, 0]
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
//# numLoopHoistedVarMods:       7
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            4
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            6
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostambles:            6
//# numForPreambles:            10
//# numStructVarDecl:            0
//# numEmptyIf:                  0



#define N 5
int cond() {
  return N;
}
int main()
{
  int i,n,tot=0;
  n = N;
  for(i=0;i<n;i++)
  {
    tot += 1;
    n++;
  }
  for(i=0;i<n;i++)
  {
    tot += 1;
    n+=1;
  }
  for(i=0;i<n;i++)
  {
    tot += 1;
    n=n+1;
  }
  for(i=0;i<cond();i++)
  {
    tot += 1;
  }
}
