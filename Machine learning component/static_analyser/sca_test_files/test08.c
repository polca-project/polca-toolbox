// FEAT_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0]
// TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0]
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
int main()
{
  int i,tot=0;
  int arr[N];
  
  for(i=0;i < N;i++)
  {
    arr[i] = i * tot;
    tot += 1;
  }
    
}
