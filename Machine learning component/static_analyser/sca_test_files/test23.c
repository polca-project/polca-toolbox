// FEAT_VECTOR: [0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 9, 0, 0, 9, 9, 0, 4, 0, 0, 0, 0, 0]
// TEST_VECTOR: [0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 9, 0, 0, 9, 9, 0, 4, 0, 0, 0, 0, 0]
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
//# numLoopHoistedVarMods:       9
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            9
//# numNonNormalizedForLoops:    9
//# numStmtsRollUp:              0
//# numCompoundStmts:            4
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostamble              0
//# numForPreamble               0
//# numStructVariables           0



#define N 5
int inc() {
  return 1;
}
int inc2(int i) {
  return i+1;
}
int main()
{
  int i,tot=0;
  for(i=0;i<N;i+=3)
    tot += 1;
  for(i=0;i<N;i=i+2)
    tot += 1;
  for(i=0;i<N;i=i+1+1)
    tot += 1;
  for(i=N-1;i>=0;i-=3)
    tot += 1;
  for(i=N-1;i>=0;i=i-2)
    tot += 1;
  for(i=N-1;i>=0;i=i-1-1)
    tot += 1;
  for(i=N-1;i>=0;i=i-inc())
    tot += 1;
  for(i=N-1;i>=0;i=inc2(i))
    tot += 1;
  for(i=N-1;i>=0;) {
    tot += 1;
    i--;
  }
    
}
