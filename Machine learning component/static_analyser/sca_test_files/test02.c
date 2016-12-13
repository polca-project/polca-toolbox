// FEAT_VECTOR: [-1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0]
// TEST_VECTOR: [-1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:            -1
//# anyFuncCall:                 0
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              1
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:       0
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            0
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            2
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostambles:            0
//# numForPreambles:             0
//# numStructVarDecl:            0
//# numEmptyIf:                  0



int glob_var = 0;
int foo(void)
{
  int local_var = 2;
  glob_var = local_var;
  return glob_var;
}
int glob_var2 = 4;
int main()
{
  int i = glob_var2;
}
