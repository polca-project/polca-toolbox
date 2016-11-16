// FEAT_VECTOR: [1, 0, 0, 0, 0, 0, 1, 1, 0, 3, 1, 1, 0, 3, 0, 0, 2, 0, 0, 0, 0, 0]
// TEST_VECTOR: [1, 0, 0, 0, 0, 0, 1, 1, 0, 3, 1, 1, 0, 3, 0, 0, 2, 0, 0, 0, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             1
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 1
//# anyLoop_Schedule:            0
//# numLoopInvVar:               3
//# numLoopHoistedVarMods:       1
//# numNon1Darray:               1
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            3
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
  int i,v[N],w[N],aux,z[N][N],tot;
  aux = 2 * 2;
#pragma stml reads aux
#pragma stml write w in {(0)}
#pragma stml read  v in {(0)}
#pragma stml iteration_independent
  for(i=0;i<N;i++)
    w[i] = v[i] * aux;
  for(i=0;i<N;i++) {
    int tot2 = aux;
    tot = 0;
    for(int j=0;j<N;j++)
      tot += z[i][j] * aux;
    v[i] = tot;
  }
    
}
