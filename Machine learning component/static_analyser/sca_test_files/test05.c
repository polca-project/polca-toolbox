// FEAT_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 2, 0, 0, 1, 0, 0, 1, 1, 0, 0]
// TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 2, 0, 0, 1, 0, 0, 1, 1, 0, 0]
// TEST_LABEL: 15 (FPGA/GPU/OpenMP/MPI)

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
//# numLoopHoistedVarMods:       2
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            2
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            1
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostambles:            1
//# numForPreambles:             1
//# numStructVarDecl:            0
//# numEmptyIf:                  0



#define N 5
int main()
{
  int i,tot=0;
  int n = N;
  for(i=0;i<N;i++)
    tot += 1;
  for(i=0;i<n;i++)
    tot += 1;
    
}
