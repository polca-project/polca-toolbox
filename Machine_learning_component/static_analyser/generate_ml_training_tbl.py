#!/usr/bin/python

# Copyright (c) 2013-2016, The IMDEA Software Institute and
# Copyright (c) 2013-2016, Universidad Politecnica de Madrid

# See LICENSE.txt and AUTHORS.txt for licensing and authorship


import sys,re,os

nameRules = []

abst2State   = {}
abst2SeqList = {}

trainTransitionTable = {}

initStates = []
finalStates = []

def convertAbsList2Key(abstraction):

    return "".join(str(x) for x in eval(abstraction))

def updateTrainTable(useCase):
    global initStates,nameRules,abst2State,trainTransitionTable,finalStates


    name = useCase[0]
    path = useCase[1]

    # use only files *.c and avoid *.c~ (emacs) and .#*.c (emacs)
    # namePattern = "^%s(_(.)*_(.)*)*\.c$" % name
    namePattern = "^%s_(\d+)_(\d+)\.c$" % name
    for dirpath, dirnames, filenames in os.walk(path):

        ruleAppldId = -1
        abstraction = None
        stateId = -1
        prevStateId = -1
        abstInitState = -1
        currTransfStep = -1
        currSeqId = -1
        prevSeqId = -1

        for file in filenames:
            m0 = re.match(namePattern,file)
            if m0:
                print("match pattern %s for %s" % (namePattern,file))
                if not None in m0.groups():
                    currTransfStep = int(m0.group(2))
                    currSeqId = int(m0.group(1))
                    # If we started a new sequence and is not the first sequence
                    if prevSeqId != -1 and prevSeqId != currSeqId:
                        # print("State %d is final" % (getStateId(abstraction)))
                        finalStates.append(getStateId(abstraction))
                        prevStateId = -1
                        ruleAppldId = -1

                filename = "%s/%s" % (dirpath,file)
                f = open(filename)

                for line in f:

                    m1 = re.match("// TEST_VECTOR: \[(.*)\]",line)
                    m6 = re.match("// RULE_APPLD:\s+(.*)",line)

                    if m6:
                        ruleAppldId = getRuleId(m6.group(1))
                        print("\t%s" % m6.group(0))

                    if m1:
                        abstraction = convertAbsList2Key(m1.group(1))
                        stateId = setStateId(currSeqId,abstraction)

                if prevStateId != -1:
                    # if we are processing the first transformation step of any sequence then take
                    # the initial code as the previous state. Ex.
                    # threshold.c -> threshold_XXX_000.c
                    # In that case we should take the abstraction of the original code (threshold.c)
                    # as the previous state instead of the last code of the previous sequence XXX-1
                    if currTransfStep == 1:
                        prevStateId = getStateId(abstInitState)
                    print("\t[%2d (%s) , %2d (%s)] -> (%2d , %2d) -> %2d" % (currSeqId,m0.group(1),currTransfStep,m0.group(2),prevStateId,ruleAppldId,stateId))
                    trainTransitionTable[prevStateId][ruleAppldId] = stateId
                else:
                    abstInitState = abstraction

                prevStateId = stateId
                prevSeqId = currSeqId
                if ruleAppldId == -1: # We are in an init state for RL training
                    initStates.append(stateId)

                f.close()

        # add the last abstraction as final state
        finalStates.append(getStateId(abstraction))


def setStateId(sequenceId,abstraction):
    global abst2State,abst2SeqList

    # absKey = convertAbsList2Key(abstraction)
    # print absKey
    if abstraction in abst2State.keys():
        if abstraction in abst2SeqList.keys():
            seqList = abst2SeqList[abstraction]
            if sequenceId in seqList:
                print("\t*******************************************")
                print("\tWARNING: Different codes have the same abstraction and are mapped to the same state Id!! ( %s )" % (abstraction))
                print("\t*******************************************")
            else:
                abst2SeqList[abstraction].append(sequenceId)
        else:
            print("\t*******************************************")
            print("\tERROR: Abstraction present in abst2State and not present in abst2SeqList!! ( %s )" % (abstraction))
            print("\t*******************************************")
        # return abst2State[abstraction]

    else:
        stateId = len(abst2State)
        initTransitionTableRow(stateId)

        abst2State[abstraction] = stateId
        abst2SeqList[abstraction] = [sequenceId]

    # for abs in abst2SeqList:
    #     print("\t%d -> %s" % (abst2State[abs],abst2SeqList[abs]))

    return abst2State[abstraction]


def getStateId(abstraction):
    global abst2State


    return abst2State[abstraction]


def getRuleId(ruleName):
    global nameRules

    ruleId = -1
    for rule in nameRules:
        ruleId += 1
        if rule == ruleName:
            return ruleId

    return ruleId

def readRulesList(rulesFile):
    global nameRules

    # Rules for nBody for POLCA demo
    nameRules = ["remove_ternary", "remove_empty_if", "normalize_iteration_step", "collapse_2_for_loops", "inlining", "remove_block", "remove_useless_statement", "subs_struct_by_fields", "roll_up_array_init", "roll_up_array", "move_inside_for_pre", "move_inside_for_post", "move_enclosing_if_inside_for", "if_2_assign_ternary", "for_loop_fusion", "contiguous_same_if", "divide_if"]
    # All rules
    # nameRules = ["remove_identity", "reduce_to_0", "undo_distributive", "sub_to_mult", "normalize_iteration_step", "loop_reversal_d2i", "loop_reversal_i2d", "loop_interchange", "loop_interchange_pragma", "for_chunk", "unrolling", "move_inside_for_pre", "move_inside_for_post", "move_enclosing_if_inside_for", "collapse_2_for_loops", "for_loop_fission", "for_loop_fusion_mapmap", "for_loop_fusion", "for_wo_block_2_for_w_block", "remove_empty_for", "for_to_while", "while_to_for", "if_wo_block_2_if_w_block", "if_wo_else_2_if_w_else", "split_addition_assign", "join_addition_assign", "mult_ternary_2_ternary", "sum_ternary_2_ternary", "assign_ternary_2_if_else", "if_else_2_assign_ternary", "if_2_assign_ternary", "empty_else", "remove_ternary", "remove_block", "remove_empty_if", "remove_useless_statement", "strength_reduction", "useless_assign", "replace_var_equal", "contiguous_same_if", "just_one_iteration_removal", "join_assignments", "propagate_assignment", "loop_inv_code_motion", "inlining", "inlining_assignment", "common_subexp_elimination", "introduce_aux_array", "flatten_float_array", "flatten_int_array", "subs_struct_by_fields", "roll_up_init", "roll_up", "roll_up_array_init", "roll_up_array", "feat_move_inside_for_pre", "feat_move_inside_for_post"]
    # nameRules = ["remove_identity", "reduce_to_0", "undo_distributive", "sub_to_mult", "normalize_iteration_step", "loop_reversal_d2i", "loop_reversal_i2d", "loop_interchange", "loop_interchange_pragma", "for_chunk", "unrolling", "move_inside_for_pre", "move_inside_for_post", "collapse_2_for_loops", "for_loop_fission", "for_loop_fusion_mapmap", "for_loop_fusion", "for_wo_block_2_for_w_block", "remove_empty_for", "for_to_while", "while_to_for", "if_wo_block_2_if_w_block", "if_wo_else_2_if_w_else", "split_addition_assign", "join_addition_assign", "mult_ternary_2_ternary", "sum_ternary_2_ternary", "assign_ternary_2_if_else", "if_else_2_assign_ternary", "if_2_assign_ternary", "empty_else", "remove_ternary", "remove_block", "remove_empty_if", "remove_useless_statement", "strength_reduction", "useless_assign", "replace_var_equal", "contiguous_same_if", "just_one_iteration_removal", "join_assignments", "propagate_assignment", "loop_inv_code_motion", "inlining", "inlining_assignment", "common_subexp_elimination", "introduce_aux_array", "flatten_float_array", "flatten_int_array", "subs_struct_by_fields", "roll_up_init", "roll_up", "roll_up_array_init", "roll_up_array"]

def initTransitionTableRow(state):
    global trainTransitionTable,nameRules

    trainTransitionTable[state] = []

    for rule in nameRules:
        trainTransitionTable[state].append(state)

def printTransitionTable():
    global trainTransitionTable

    for stateId in trainTransitionTable:
        print(trainTransitionTable[stateId])

def printTrainDataToFile(trainDataFile):
    global trainTransitionTable,initStates,finalStates,nameRules


    print("Printing train data to file %s ..." % (trainDataFile))
    f = open(trainDataFile,'w')

    f.write("Rule Names\n")
    f.write("%s\n\n" % (nameRules))

    f.write("Abst.-State mappings\n")
    f.write("%s\n\n" % (abst2State))

    f.write("Init. States\n")
    f.write("%s\n\n" % (initStates))

    f.write("Final States\n")
    f.write("%s\n\n" % (finalStates))

    f.write("Transition Table\n")
    for stateId in trainTransitionTable:
        f.write('%s\n' % (trainTransitionTable[stateId]))

    f.close()

if __name__ == "__main__":


    if len(sys.argv) < 2:
        print("ERROR: usage -> %s <target_platform>" % (sys.argv[0]))
        exit(0)
    else:
        targetPlatform = sys.argv[1]

    trainDataPath = '../machine_learning/reinforcement_learning/utils/'
    trainDataFile = 'trainingData.txt'


    # pathList = [["threshold0","./train_set/imageFilter/threshold/s2s_test"]
    #             ]
    # pathList = [["nbody","./train_set/hpcDwarfs/nBody/s2s_transformations/2arrays"]]

    # pathList = [["nbody","./train_set/hpcDwarfs/nBody/oracle_test"]
    #             ]

    pathList = [["nbody","./train_set/hpcDwarfs/nBody/s2s_transformations/merged"]]


    print("\n#####################################################\n")

    rulesFile = ''
    readRulesList(rulesFile)

    # for rule in nameRules:
    #     print rule

    for useCase in pathList:

        updateTrainTable(useCase)

    print("Init. States: %s" % (initStates))
    print("Final States: %s" % (finalStates))

    printTransitionTable()

    defaultTarget = "maxj"
    if targetPlatform == "none":
        targetPlatform = defaultTarget

    replaceStr = "_%s.txt" % (targetPlatform)
    trainDataFile = trainDataFile.replace(".txt",replaceStr)

    printTrainDataToFile(trainDataPath+trainDataFile)