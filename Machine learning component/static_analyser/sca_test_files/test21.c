// FEAT_VECTOR: [1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 3, 2, 0, 0, 3, 0, 0, 0, 4, 0]
// TEST_VECTOR: [1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 3, 2, 0, 0, 3, 0, 0, 0, 4, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             1
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        1
//# usesGlobalVars:              0
//# anyIfStmt:                   1
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               1
//# numLoopHoistedVarMods:       1
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         3
//# totalNumForLoops:            2
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            3
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostamble              0
//# numForPreamble               4
//# numStructVariables           0



#define N 9
#define F 3
int main()
{
  int i,j,v[N],f[F],w[N],inv,aux,x,y;
  inv = 2 * 2;
  for(i=0;i<N;i++) {
    x = i;
    aux = i - (F / 2);
    w[i] = 0;
    for(j=0;j<F;j++) {
      y = j;
      if(aux < 0 && aux > (N-1))
	    continue;
      w[i] += v[aux] * f[j] * inv;
      w[i] += v[x*N+y] * f[j];
      aux++;
      aux+=1;
      aux = aux + 1;
    }
  }
    
}
