// FEAT_VECTOR: [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 2, 0, 0, 1, 0, 0]
// TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 2, 0, 0, 1, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             0
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 1
//# anyLoop_Schedule:            0
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:       1
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            2
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            1
//# anyTernaryOp:                0
//# anyUselessStmt:              0


#define N 5
int main()
{
  int i,v[N],w[N],tot;
#pragma stml writes v
#pragma stml reads w
#pragma stml iteration_independent
  for(i=0;i<N;i++)
    v[i] = w[i]*w[i];
  tot = 0;
  for(i=0;i<N;i++)
    tot += v[i];
    
}
