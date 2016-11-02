// FEAT_VECTOR: [1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 4, 2, 0, 0, 3, 0, 0]
// TEST_VECTOR: [1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 4, 2, 0, 0, 3, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             1
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:       0
//# numNon1Darray:               1
//# numAuxVarArrayIndex:         4
//# totalNumForLoops:            2
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            3
//# anyTernaryOp:                0
//# anyUselessStmt:              0


#define N 9
#define F 3

int main()
{
  int i,j,v[N],f[N*N],w[N][N],x,y;

  for(i=0;i<N;i++) {
  
    x = i;

    v[i] = 0;
    for(j=0;j<F;j++) {
      y = j;

      v[i] += f[i*N+j];

      v[i] += f[x*N+y];

      v[i] += w[x][y];
    }
  }
}
