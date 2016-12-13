// FEAT_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 10, 0, 0, 10, 0, 0, 1, 0, 0, 45, 45, 0, 0]
// TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 10, 0, 0, 10, 0, 0, 1, 0, 0, 45, 45, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             0
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:      10
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:           10
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            1
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostambles:           45
//# numForPreambles:            45
//# numStructVarDecl:            0
//# numEmptyIf:                  0



#define N 5
int main()
{
  int i,tot=0;
  for(i=0;i<N;i++)
    tot += 1;
  for(i=0;i<N;i+=1)
    tot += 1;
  for(i=0;i<N;i=i+1)
    tot += 1;
  for(i=0;i<N;i=1+i)
    tot += 1;
  for(i=0;i<N;i=i+1+0)
    tot += 1;
  for(i=0;i<N;i=0+1+i)
    tot += 1;
  for(i=0;i<N;i=0+i+1)
    tot += 1;
  for(i=N-1;i>=0;i=0+i-1)
    tot += 1;
  for(i=N-1;i>=0;i=i-1)
    tot += 1;
  for(i=N-1;i>=0;i-=1)
    tot += 1;
    
}
