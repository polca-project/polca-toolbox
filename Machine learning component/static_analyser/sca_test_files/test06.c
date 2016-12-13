// FEAT_VECTOR: [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 3, 0, 0, 2, 0, 0, 3, 3, 0, 0]
// TEST_VECTOR: [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 3, 0, 0, 2, 0, 0, 3, 3, 0, 0]
// TEST_LABEL: 3 (OpenMP/MPI)

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
//# numLoopHoistedVarMods:       3
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            3
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            2
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostambles:            3
//# numForPreambles:             3
//# numStructVarDecl:            0
//# numEmptyIf:                  0



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
  for(i=0;i<foo();i++)
    tot += 1;
  for(i=0;i<n;i++)
    tot += 1;
    
}
