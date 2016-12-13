// FUNC_ANALYZ: main BLOCK_ABS
// FEAT_VECTOR: [1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 2, 0, 2, 0, 0, 5, 1, 1, 1, 0, 0, 0]
// TEST_VECTOR: [1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 2, 0, 2, 0, 0, 5, 1, 1, 1, 0, 0, 0]
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
//# numLoopInvVar:               1
//# numLoopHoistedVarMods:       0
//# numNon1Darray:               2
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            2
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            5
//# anyTernaryOp:                1
//# anyUselessStmt:              1
//# numForPostambles:            1
//# numForPreambles:             0
//# numStructVarDecl:            0
//# numEmptyIf:                  0



#include "timer.h"
#include "params.h"
#include <stdlib.h>



int main()
    {
      {
    char fileName[80];
    int _ret_val_0;
      }
    sprintf(fileName, "%s/%s", DATA_PATH, INPUT_FILE);
    input_dsp_arg(fileName, image, N * N, 1);
#pragma polca def BLOCK_ABS
    {
    int thresholdValue = T;
    {
    int c;
    int r;
    int aux;
    for (r = 0; r < N; r++)
        {
    for (c = 0; c < N; c++)
        {
    {
    int _ret_val_1;
    _ret_val_1 = image[r][c] > thresholdValue ? 255 : 0;
    aux = _ret_val_1;
    }
    result[r][c] = aux;
    }
    }
    0;
    }
    }
    sprintf(fileName, "%s/output.dsp", DATA_PATH);
    output_dsp_arg(fileName, result, N * N, 1);
    int _ret_val_0 = 0;
    return _ret_val_0;
    }

void dummy(){
  int x = 0;

    x = x + 2;
}

