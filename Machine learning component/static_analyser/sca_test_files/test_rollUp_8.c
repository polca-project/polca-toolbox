// FEAT_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 3, 0, 0, 0, 4, 0, 7, 3, 0, 0]
// TEST_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 3, 0, 0, 0, 4, 0, 6, 3, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:             1
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        1
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               3
//# numLoopHoistedVarMods:       0
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            4
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              6
//# numCompoundStmts:            3
//# anyTernaryOp:                0
//# anyUselessStmt:              0


#define N 10

int main(const int argc, const char** argv) {

  float *pos,*vel,dt;

  for (int i = 0; i < N; i++) {
    float F[3];
    #pragma stml rolled-up
    for(int k=0;k<3;k++)
      F[k] = 0.0f;

    for (int j = 0; j < N; j++) {
      float d[3];
      d[0] = pos[j] - pos[i];
      d[1] = pos[j+1] - pos[i+1];
      d[2] = pos[j+2] - pos[i+2];

      F[0] = F[0] + d[0] * invDist3;
      F[1] = F[1] + d[1] * invDist3;
      F[2] = F[2] + d[2] * invDist3;
    }

    #pragma stml rolled-up
    for(int k=0;k<2;k++)
      vel[i+k] = vel[i+k] + dt*F[k];

    vel[i+2] = vel[i+2] + dt*F[2];
  }

}
