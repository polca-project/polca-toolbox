// FEAT_VECTOR: [0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 3, 2, 0, 0, 0, 0, 0, 0]
// TEST_VECTOR: [0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 2, 0, 0, 0, 0, 0, 0]
// TEST_LABEL: 7 (GPU/OpenMP/MPI)

//# maxForStmtDepth:             0
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        1
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:       1
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            1
//# numNonNormalizedForLoops:    1
//# numStmtsRollUp:              0
//# numCompoundStmts:            2
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostambles:            0
//# numForPreambles:             0
//# numStructVarDecl:            0
//# numEmptyIf:                  0



#define N 5
int main()
{
  int i,tot=0;
  int arr[N*3];
  
  for(i=0;i < N;i=i+3)
  {
    arr[i]    = i * tot;
    arr[i+1]  = (i+1) * tot;
    arr[i+2]  = (i+2) * tot;
    tot += 1;
  }
  
}
