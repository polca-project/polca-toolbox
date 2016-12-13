#!/usr/bin/python
# /opt/local/bin/python
# /usr/bin/env python

# Copyright (c) 2013-2016, The IMDEA Software Institute and
# Copyright (c) 2013-2016, Universidad Politecnica de Madrid

# See LICENSE.txt and AUTHORS.txt for licensing and authorship


from scipy import *
import sys, time, re
# import matplotlib.pyplot as plt
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

initStates = None
goalStates = None

transitionTable = None

def parseList(listStr):
    listStr = listStr.replace('[','')
    listStr = listStr.replace(']','')
    listStr = listStr.replace('\n','')

    newStr = [int(num) for num in listStr.split(',')]

    if newStr == None:
        newStr = [int(listStr)]

    return newStr


def readTrainData(trainDataFile):
    global initStates,goalStates,transitionTable

    print("Loading train data from file %s ..." % (trainDataFile))
    f = open(trainDataFile,'r')

    tableData = None
    dataFound = -1
    # lineToProcess = None
    for line in f:

        m1 = re.match("Init\. States",line)
        m2 = re.match("Final States",line)
        m3 = re.match("Transition Table",line)

        if dataFound == 0:
            initStates = parseList(line)
            # print initStates
            dataFound = -1

        if dataFound == 1:
            goalStates = parseList(line)
            # print goalStates
            dataFound = -1

        if dataFound == 2:
            tableRow = parseList(line)
            if tableData == None:
                tableData = [tableRow]
            else:
                tableData.append(tableRow)
            # print tableRow

        if m1:
            dataFound = 0

        if m2:
            dataFound = 1

        if m3:
            dataFound = 2
            # lineToProcess

    f.close()
    transitionTable = array(tableData)


def dumpActionValueTable(table,tableFile):

    print("Writing actionValueTable to file %s ..." % (tableFile))
    f = open(tableFile,'w+')

    f.write("[")
    for i in range(len(table)-1):
        f.write("%f, " % (table[i]))

    f.write("%d]" % (table[len(table)-1]))

    f.close()


############################################################################

if len(sys.argv) < 2:
    print("ERROR: usage -> %s <target_platform>" % (sys.argv[0]))
    exit(0)
else:
    targetPlatform = sys.argv[1]

replaceStr = "_%s.txt" % (targetPlatform)
trainDataFile = trainDataFile.replace(".txt",replaceStr)

readTrainData(trainDataPath+trainDataFile)


###### Example Convolution ######
# numActions = 6
# numStates  = 12
#
# goal = array([11])
#################################

###### Example RGBfilter ######
# numActions = 5
# numStates  = 11
#
# goal = [3,10]
#################################

###### Example Cercedilla ######
# numActions = 2
# numStates  = 5
#
# goal = array([4])
#################################

###### Example Merged RGB and Convolution ######
# numActions = 10
# numStates  = 23

# goal = array([11,15,22])
#################################

###### Example for PROHA Workshop ######
# numActions = 15
# numStates  = 62
#
# goal = array([11,15,22,37,47,61])
#################################

###### Table used for results for PROHA 2016 and PROLE 2016 slides ######
# numActions = 2
# numStates  = 5
#
# goal = array([4])
#################################


numActions = transitionTable.shape[1]
numStates = transitionTable.shape[0]

env = PolcaEnv(numStates,numActions, initStates, goalStates, transitionTable)

# create task
task = MDPPolcaTask(env)

# create value table and initialize with ones
table = ActionValueTable(numStates, numActions)
table.initialize(1.)

# create agent with controller and learner - use SARSA(), Q() or QLambda() here
# learner = QLambda()
learner = SARSA()
# learner = Q()
# standard exploration is e-greedy, but a different type can be chosen as well
# learner.explorer = BoltzmannExplorer()

# create agent
agent = LearningAgent(table, learner)

# create experiment
experiment = Experiment(task, agent)

# prepare plotting
# pylab.gray()
# pylab.ion()


# Learning phase
# Num iterations used for PROHA Workshop perliminary evaluation
# numIterations   = 1600
numIterations   = 1500
numInteractions = 600

# Num iterations used for PROHA and PROLE slides
# numIterations   = 10
# numInteractions = 3
for i in range(numIterations):


    # interact with the environment (here in batch mode)
    experiment.doInteractions(numInteractions)
    agent.learn()
    agent.reset()

#    # and draw the table
#    # pylab.pcolor(table.params.reshape(numStates,numActions).max(1).reshape(numStates,numStates))
#    # # pylab.savefig('myfilename_%2d.png' % (i))
#    # pylab.show(block=True)
#    # print table.params.reshape(numStates,numActions).max(1).reshape(numStates,1)

#     print("\nIteration: %d" % (i))
#     print table.params.reshape(numStates,numActions)
#
# print "-------------------------------------------------"
# exit(0)
# print table.params.reshape(numStates,numActions)

print table.params

replaceStr = "_%s.txt" % (targetPlatform)
tableFile = tableFile.replace(".txt",replaceStr)

dumpActionValueTable(table.params,trainDataPath+tableFile)

###### Example Convolution ######
# Predict phase
# Start in [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0] -> 8
# Start in [0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 2] -> 4
# initStates = [8,4]
#################################

###### Example RGBfilter ######
# initStates = [0]
#################################

###### Example Convolution and RGB Merged ######
# initStates = [0,7,12,8,4]
#################################

###### Example for PROHA Workshop ######
# # Init states from learning transitions
# initStates = [0,7,12]
#
# # from compress learning case
# initStates.append(23)
#
# # Init states from predict examples
# initStates.append(8)
# initStates.append(4)
#
# # for Rotate3D predict case: state 23
#
# # from image_difference predict case
# # initStates.append(38)
#################################


###### Table used for results for PROHA 2016 and PROLE 2016 slides ######
# Init states from learning transitions
# initStates = [0]
#################################

# for initState in initStates:
#
#     state = initState
#     newState = initState
#     # goal = array([11])
#     action = None
#     values = None
#     steps = 0
#
#     # values = table.params.reshape(numStates, numActions)[state, :].flatten()
#     # sorted_arr = sort(values)
#     # reversed_arr = sorted_arr[::-1]
#     # value = sorted_arr[::-1][0]
#     # print values
#     # print sorted_arr
#     # print reversed_arr
#     # print value
#     sys.stdout.write("           [ S, A] ->  S\n")
#     sys.stdout.write("           -------------\n")
#     try:
#         # while not (newState in goal) and steps<5:
#         while not (newState in goalStates):
#             # action = table.getMaxAction(state)
#             values = table.params.reshape(numStates, numActions)[state, :].flatten()
#             action = where(values == max(values))[0]
#             newState = env.actionT[state,action]
#
#             sys.stdout.write("Transition [%2d" % (state))
#             sys.stdout.write(",%2d] ->" % (action))
#             sys.stdout.write(" %2d\n" % (newState))
#             state = newState
#             steps += 1
#     except TypeError:
#         print"\nTypeError in returned action:"
#         print action
#         print values
#
#     print "-------------------------------------------------"