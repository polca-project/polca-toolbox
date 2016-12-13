// FEAT_VECTOR: [1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 2, 1, 0, 4, 0, 0, 2, 0, 0, 6, 5, 0, 0]
// TEST_VECTOR: [1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 2, 1, 0, 4, 0, 0, 2, 0, 0, 6, 5, 0, 0]
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
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:       2
//# numNon1Darray:               1
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            4
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            2
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostambles:            6
//# numForPreambles:             5
//# numStructVarDecl:            0
//# numEmptyIf:                  0



#define N 5
int main()
{
  int i,j,v[N],w[N][N],tot;
#pragma stml writes tot
#pragma stml reads tot
#pragma stml write v in {(0)}
#pragma stml read  w in {(0,0)}
#pragma stml iteration_independent
  for(i=0;i<N;i++) {
    tot = 0;
    for(j=0;j<N;j++)
      tot += w[i][j];
    v[i] = tot;
  }
#pragma stml write v in {(0)}
#pragma stml read  w in {(0)}
#pragma stml iteration_independent
  for(i=0;i<N;i++)
    v[i] = w[i]*w[i];
  tot = 0;
  for(i=0;i<N;i++)
    tot += v[i];
    
}
