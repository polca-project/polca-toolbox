// FEAT_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 2, 3, 0, 0, 2, 0, 6, 3, 0, 0]
// TEST_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 2, 3, 0, 0, 2, 0, 6, 3, 0, 0]
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
//# numLoopInvVar:               2
//# numLoopHoistedVarMods:       3
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            2
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              6
//# numCompoundStmts:            3
//# anyTernaryOp:                0
//# anyUselessStmt:              0


#define N 10

int main(const int argc, const char** argv) {

  float *pos,*vel,dt;

  for (int i = 0; i < N; i++) {
    float Fx;
    float Fy;
    float Fz;
    Fx = 0.0f;
    Fy = 0.0f;
    Fz = 0.0f;

    for (int j = 0; j < N; j++) {
      float dx;
      float dy;
      float dz;
      dx = pos[j] - pos[i];
      dy = pos[j+1] - pos[i+1];
      dz = pos[j+2] - pos[i+2];

      Fx = Fx + dx * invDist3;
      Fy = Fy + dy * invDist3;
      Fz = Fz + dz * invDist3;
    }
    vel[i] = vel[i] + dt*Fx;
    vel[i+1] = vel[i+1] + dt*Fy;
    vel[i+2] = vel[i+2] + dt*Fz;
  }

}
