// FUNC_ANALYZ: main BLOCK_ABS
// FEAT_VECTOR: [-1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1]
// TEST_VECTOR: [-1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:            -1
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
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            0
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            1
//# anyTernaryOp:                0
//# anyUselessStmt:              1


#include "timer.h"
#include "params.h"
#include <stdlib.h>



int main()
    {

    char fileName[80];
    int _ret_val_0;

    sprintf(fileName, "%s/%s", DATA_PATH, INPUT_FILE);
    input_dsp_arg(fileName, image, N * N, 1);
#pragma polca def BLOCK_ABS
    {
    int thresholdValue = T;

    0;

    }
    sprintf(fileName, "%s/output.dsp", DATA_PATH);
    output_dsp_arg(fileName, result, N * N, 1);
    _ret_val_0 = 0;
    return _ret_val_0;
    }

