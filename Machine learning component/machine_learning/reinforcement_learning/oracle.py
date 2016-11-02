#!/opt/local/bin/python
# /usr/bin/env python
__author__ = 'Thomas Rueckstiess, ruecksti@in.tum.de'

""" This example demonstrates how to use the discrete Temporal Difference
Reinforcement Learning algorithms (SARSA, Q, Q(lambda)) in a classical
fully observable MDP maze task. The goal point is the top right free
field. """

from scipy import *
import sys, time, re
import matplotlib.pyplot as plt
import pylab

from pybrain.rl.learners.valuebased import ActionValueTable
from pybrain.rl.agents import LearningAgent
from pybrain.rl.learners import Q, QLambda, SARSA #@UnusedImport
from pybrain.rl.explorers import BoltzmannExplorer #@UnusedImport
from pybrain.rl.experiments import Experiment
from pybrain.rl.environments import Task
from polcamdp import MDPPolcaTask
from PolcaEnv import PolcaEnv

trainDataPath = './utils/'
trainDataFile = 'trainingData.txt'

tableFile = 'actionValueTable.txt'

initStates       = None
goalStates       = None

transitionTable  = None

actionValueTable = None

ruleNames        = None
abst2State       = None

numActions       = -1
numStates        = -1
table            = None

def parseList(listStr,dataType):
    listStr = listStr.replace('[','')
    listStr = listStr.replace(']','')
    listStr = listStr.replace('\n','')

    if dataType == 0:
        newStr = [int(num) for num in listStr.split(',')]
    else:
        newStr = [float(num) for num in listStr.split(',')]

    if newStr == None:
        if dataType == 0:
            newStr = [int(listStr)]
        else:
            newStr = [float(listStr)]

    return newStr


def readTrainData(trainDataFile):
    global initStates,goalStates,transitionTable,ruleNames,abst2State

    print("Loading train data from file %s ..." % (trainDataFile))
    f = open(trainDataFile,'r')

    tableData = None
    dataFound = -1
    # lineToProcess = None
    for line in f:

        m1 = re.match("Init\. States",line)
        m2 = re.match("Final States",line)
        m3 = re.match("Transition Table",line)
        m4 = re.match("Rule Names",line)
        m5 = re.match("Abst.-State mappings",line)

        if dataFound == 0:
            initStates = parseList(line,0)
            # print initStates
            dataFound = -1

        if dataFound == 1:
            goalStates = parseList(line,0)
            # print goalStates
            dataFound = -1

        if dataFound == 2:
            tableRow = parseList(line,0)
            if tableData == None:
                tableData = [tableRow]
            else:
                tableData.append(tableRow)
            # print tableRow

        if dataFound == 3:
            ruleNames = eval(line)
            dataFound = -1

        if dataFound == 4:
            abst2State = eval(line)
            dataFound = -1

        if m4:
            dataFound = 3

        if m5:
            dataFound = 4

        if m1:
            dataFound = 0

        if m2:
            dataFound = 1

        if m3:
            dataFound = 2
            # lineToProcess

    f.close()
    transitionTable = array(tableData)


def readActionValueTable(tableFile):
    global actionValueTable

    print("Loading train data from file %s ..." % (tableFile))
    f = open(tableFile,'r')

    for line in f:
        actionValueTable = parseList(line,1)

    f.close()


def getRuleId(ruleName):
    global ruleNames

    ruleId = -1
    for rule in ruleNames:
        ruleId += 1
        if rule == ruleName:
            return ruleId

    return ruleId

def getRuleName(ruleId):
    global ruleNames

    return ruleNames[ruleId]

def initData():
    global numActions,numStates,table

    readTrainData(trainDataPath+trainDataFile)

    readActionValueTable(trainDataPath+tableFile)



    numActions = transitionTable.shape[1]
    numStates = transitionTable.shape[0]

    # create value table and initialize with ones
    table = ActionValueTable(numStates, numActions)

    for i in range(transitionTable.shape[0]):
        for j in range(transitionTable.shape[1]):
            table._params[i*transitionTable.shape[1]+j] = actionValueTable[i*transitionTable.shape[1]+j]

    # print type(table._params)
    #
    #
    # print table.params.reshape(numStates,numActions)

def testTransformationProcess():
    for initState in initStates:

        state = initState
        newState = initState
        # goal = array([11])
        action = None
        values = None
        steps = 0

        # values = table.params.reshape(numStates, numActions)[state, :].flatten()
        # sorted_arr = sort(values)
        # reversed_arr = sorted_arr[::-1]
        # value = sorted_arr[::-1][0]
        # print values
        # print sorted_arr
        # print reversed_arr
        # print value
        sys.stdout.write("           [ S, A] ->  S\n")
        sys.stdout.write("           -------------\n")
        try:
            # while not (newState in goal) and steps<5:
            while not (newState in goalStates):
                # action = table.getMaxAction(state)
                values = table.params.reshape(numStates, numActions)[state, :].flatten()
                action = where(values == max(values))[0]
                newState = transitionTable[state,action]

                sys.stdout.write("Transition [%2d" % (state))
                sys.stdout.write(",%2d] ->" % (action))
                sys.stdout.write(" %2d\n" % (newState))
                state = newState
                steps += 1
        except TypeError:
            print"\nTypeError in returned action:"
            print action
            print values

        print "-------------------------------------------------"

    return -1


def transformationStep(codeAbstraction,ruleIdList):

    state = abst2State[codeAbstraction]

    # values = table.params.reshape(numStates, numActions)[state, :].flatten()
    values = table.params.reshape(numStates, numActions)[state, ruleIdList].flatten()
    localAction = where(values == max(values))[0]
    # localAction is of type numpy.ndarray -> get the first value since it is the max
    globalAction = ruleIdList[localAction[0]]
    newState = transitionTable[state,globalAction]

    # print type(table.params)
    sys.stdout.write("Transition [%2d" % (state))
    sys.stdout.write(",%2d] ->" % (globalAction))
    sys.stdout.write(" %2d\n" % (newState))

    return globalAction


############################################################################

if __name__ == "__main__":

    codeAbstraction = None
    ruleIdList      = None

    if len(sys.argv) < 3:
        print("ERROR: usage -> %s <code_abstraction> <rule_id_list>" % (sys.argv[0]))
    else:
        codeAbstraction = sys.argv[1]
        ruleIdList = eval(sys.argv[2])
        #'-1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0'
        #ruleIdList = [0,4,6,41,45]

    initData()

    ruleId = -1
    if codeAbstraction == None:
        ruleId = testTransformationProcess()
    else:
        ruleId = transformationStep(codeAbstraction,ruleIdList)


    ruleName = getRuleName(ruleId)

    sys.exit(ruleName)


