__author__ = 'Tom Schaul, tom@idsia.ch'

from random import random, choice
from scipy import *

import numpy as np

from pybrain.utilities import Named
from pybrain.rl.environments.environment import Environment

# TODO: mazes can have any number of dimensions?


class PolcaEnv(Environment, Named):
    """ 2D mazes, with actions being the direction of movement (N,E,S,W)
    and observations being the presence of walls in those directions.

    It has a finite number of states, a subset of which are potential starting states (default: all except goal states).
    A maze can have absorbing states, which, when reached end the episode (default: there is one, the goal).

    There is a single agent walking around in the maze (Theseus).
    The movement can succeed or not, or be stochastically determined.
    Running against a wall does not get you anywhere.

    Every state can have an an associated reward (default: 1 on goal, 0 elsewhere).
    The observations can be noisy.
    """

    goal = []

    # current state
    perseus = None

    curAction = None

    # list of possible initial states for learning phase
    initPos = None

    numActions = None

    # Array of rewards for reinforcing states resulting in better performant codes
    rewards = None

# action 0: remove loop invariant var modifications
# action 1: move loop invariant var inside loop
# action 2: remove aux var used to access array
# action 3: remove if statement
# action 4: convert non-1D array to 1D
# action 5: 2 for-loop collapse
#                                                                                      S ->  [[S,A],[S,A],...]
# convolution_initial.c:// TEST_VECTOR: [0, 0, 0, 1, 0, 1, 1, 0, 0, 2, 2, 0, 2, 4, 0]  0 ->  [[ 1,0],[0,*]]
# convolution_1_1.c://     TEST_VECTOR: [0, 0, 0, 1, 0, 1, 1, 0, 0, 2, 1, 0, 2, 4, 0]  1 ->  [[ 2,0],[1,*]]
# convolution_1_2.c://     TEST_VECTOR: [0, 0, 0, 1, 0, 1, 1, 0, 0, 2, 0, 0, 2, 4, 0]  2 ->  [[ 3,1],[2,*]]
# convolution_1_3.c://     TEST_VECTOR: [0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 0, 0, 2, 4, 0]  3 ->  [[ 4,1],[3,*]]
# convolution_1_4.c://     TEST_VECTOR: [0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 2, 4, 0]  4 ->  [[ 5,2],[4,*]]
# convolution_1_5.c://     TEST_VECTOR: [0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 4, 0]  5 ->  [[ 6,2],[5,*]]
# convolution_1_6.c://     TEST_VECTOR: [0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 4, 0]  6 ->  [[10,3],[6,*]]


# convolution_initial.c:// TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 3, 0, 4, 0]  7 -> [[ 8,4],[7,*]]
# convolution_1_1.c://     TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2, 0, 4, 0]  8 -> [[ 9,4],[8,*]]
# convolution_1_2.c://     TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 4, 0]  9 -> [[10,4],[9,*]]

# convolution_1_7.c://     TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 4, 0] 10 -> [[11,5],[10,*]]
# convolution_1_3.c://     TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 4, 0] 10 -> [[11,5],[10,*]]

# convolution_1_4.c://     TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 3, 0] 11 -> [[11,*]]
# convolution_1_8.c://     TEST_VECTOR: [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 3, 0] 11 -> [[11,*]]

#------------------------------------------------------

# action 6: loop fussion
# action 7: loop schedule
# action 8: function inline
# action 9: for loop normalization
# action 5: 2 for-loop collapse (action 5: shared with convolution example)

#                                                                                  S -> [[ S,A],[ S,A],...]
# rgb_filter.c:     // TEST_VECTOR: [0, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 3, 0] 12 -> [[13,6],[12,*]]

# rgb_filter_3_1.c: // TEST_VECTOR: [0, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 2, 0] 13 -> [[14,6],[13,*]]
# rgb_filter_7_1.c: // TEST_VECTOR: [0, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 2, 0] 13 -> [[14,6],[13,*]]

# rgb_filter_3_2.c: // TEST_VECTOR: [0, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0] 14 -> [[15,7],[16,8],[14,*]]
# rgb_filter_7_1b.c:// TEST_VECTOR: [0, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0] 14 -> [[15,7],[16,8],[14,*]]


# rgb_filter_3_3.c: // TEST_VECTOR: [0, 3, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 2, 0] 15 -> [[15,*]]


# rgb_filter_7_2.c: // TEST_VECTOR: [1, 2, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 2, 1] 16 -> [[17,8],[16,*]]
# rgb_filter_7_3.c: // TEST_VECTOR: [1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 3, 1] 17 -> [[18,8],[17,*]]
# rgb_filter_7_4.c: // TEST_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 4, 1] 18 -> [[19,6],[18,*]]
# rgb_filter_7_5.c: // TEST_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 3, 1] 19 -> [[20,6],[19,*]]
# rgb_filter_7_5b.c:// TEST_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 2, 1] 20 -> [[21,9],[20,*]]
# rgb_filter_7_6.c: // TEST_VECTOR: [1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 2, 0] 21 -> [[22,5],[21,*]]
# rgb_filter_7_7.c: // TEST_VECTOR: [0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0] 22 -> [[22,*]]

###### Example Convolution and RGB Merged ######
    # actionT   = array([[ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    #                    [ 2, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    #                    [ 2, 3, 2, 2, 2, 2, 2, 2, 2, 2],
    #                    [ 3, 4, 3, 3, 3, 3, 3, 3, 3, 3],
    #                    [ 4, 4, 5, 4, 4, 4, 4, 4, 4, 4],
    #                    [ 5, 5, 6, 5, 5, 5, 5, 5, 5, 5],
    #                    [ 6, 6, 6,10, 6, 6, 6, 6, 6, 6],
    #                    [ 7, 7, 7, 7, 8, 7, 7, 7, 7, 7],
    #                    [ 8, 8, 8, 8, 9, 8, 8, 8, 8, 8],
    #                    [ 9, 9, 9, 9,10, 9, 9, 9, 9, 9],
    #                    [10,10,10,10,10,11,10,10,10,10],
    #                    [11,11,11,11,11,11,11,11,11,11],
    #                    [12,12,12,12,12,12,13,12,12,12],
    #                    [13,13,13,13,13,13,14,13,13,13],
    #                    [14,14,14,14,14,14,14,15,16,14],
    #                    [15,15,15,15,15,15,15,15,15,15],
    #                    [16,16,16,16,16,16,16,16,17,16],
    #                    [17,17,17,17,17,17,17,17,18,17],
    #                    [18,18,18,18,18,18,19,18,18,18],
    #                    [19,19,19,19,19,19,20,19,19,19],
    #                    [20,20,20,20,20,20,20,20,20,21],
    #                    [21,21,21,21,21,22,21,21,21,21],
    #                    [22,22,22,22,22,22,22,22,22,22]])

    # initPos = [0,7,12]
#################################

###### Table used for results for PROHA Workshop ######
    # actionT   = array([[1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    #                    [2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    #                    [2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2],
    #                    [3, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3],
    #                    [4, 4, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4],
    #                    [5, 5, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5],
    #                    [6, 6, 6, 10, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6],
    #                    [7, 7, 7, 7, 8, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7],
    #                    [8, 8, 8, 8, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8],
    #                    [9, 9, 9, 9, 10, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9],
    #                    [10, 10, 10, 10, 10, 11, 10, 10, 10, 10, 10, 10, 10, 10, 10],
    #                    [11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11],
    #                    [12, 12, 12, 12, 12, 12, 13, 12, 12, 12, 12, 12, 12, 12, 12],
    #                    [13, 13, 13, 13, 13, 13, 14, 13, 13, 13, 13, 13, 13, 13, 13],
    #                    [14, 14, 14, 14, 14, 14, 14, 15, 16, 14, 14, 14, 14, 14, 14],
    #                    [15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15],
    #                    [16, 16, 16, 16, 16, 16, 16, 16, 17, 16, 16, 16, 16, 16, 16],
    #                    [17, 17, 17, 17, 17, 17, 17, 17, 18, 17, 17, 17, 17, 17, 17],
    #                    [18, 18, 18, 18, 18, 18, 19, 18, 18, 18, 18, 18, 18, 18, 18],
    #                    [19, 19, 19, 19, 19, 19, 20, 19, 19, 19, 19, 19, 19, 19, 19],
    #                    [20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 20, 20, 20, 20, 20],
    #                    [21, 21, 21, 21, 21, 22, 21, 21, 21, 21, 21, 21, 21, 21, 21],
    #                    [22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22],
    #                    [23, 23, 23, 23, 23, 38, 23, 23, 23, 23, 24, 23, 23, 23, 48],
    #                    [24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 25, 24, 24, 24],
    #                    [25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 25, 25, 25],
    #                    [26, 26, 26, 26, 26, 26, 27, 26, 26, 26, 26, 26, 26, 26, 26],
    #                    [27, 27, 27, 27, 27, 27, 28, 27, 27, 27, 27, 27, 27, 27, 27],
    #                    [28, 28, 28, 28, 28, 28, 28, 28, 29, 28, 28, 28, 28, 28, 28],
    #                    [29, 29, 29, 29, 29, 30, 29, 29, 29, 29, 29, 29, 29, 29, 29],
    #                    [30, 30, 30, 30, 31, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30],
    #                    [31, 31, 31, 31, 32, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31],
    #                    [32, 32, 32, 32, 33, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32],
    #                    [33, 33, 33, 33, 34, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33],
    #                    [34, 34, 34, 34, 35, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34],
    #                    [35, 35, 35, 35, 37, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35],
    #                    [36, 36, 36, 36, 37, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36],
    #                    [37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37],
    #                    [38, 38, 38, 38, 38, 38, 38, 38, 39, 38, 38, 38, 38, 38, 38],
    #                    [39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 40, 39, 39],
    #                    [40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 41, 40],
    #                    [41, 41, 41, 41, 41, 41, 42, 41, 41, 41, 41, 41, 41, 41, 41],
    #                    [42, 42, 42, 42, 42, 42, 43, 42, 42, 42, 42, 42, 42, 42, 42],
    #                    [43, 43, 43, 43, 44, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43],
    #                    [44, 44, 44, 44, 45, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44],
    #                    [45, 45, 45, 45, 46, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45],
    #                    [46, 46, 46, 46, 47, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46],
    #                    [47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47],
    #                    [48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 49, 48, 48, 48, 48],
    #                    [49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 50, 49, 49, 49],
    #                    [50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 51, 50, 50, 50],
    #                    [51, 51, 51, 51, 51, 51, 52, 51, 51, 51, 51, 51, 51, 51, 51],
    #                    [52, 52, 52, 52, 52, 52, 53, 52, 52, 52, 52, 52, 52, 52, 52],
    #                    [53, 53, 53, 53, 53, 53, 53, 53, 54, 53, 53, 53, 53, 53, 53],
    #                    [54, 54, 54, 54, 54, 55, 54, 54, 54, 54, 54, 54, 54, 54, 54],
    #                    [55, 55, 55, 55, 56, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55],
    #                    [56, 56, 56, 56, 57, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56],
    #                    [57, 57, 57, 57, 58, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57],
    #                    [58, 58, 58, 58, 59, 58, 58, 58, 58, 58, 58, 58, 58, 58, 58],
    #                    [59, 59, 59, 59, 60, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59],
    #                    [60, 60, 60, 60, 61, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60],
    #                    [61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61]])
    #
    #
    # initPos = [0,7,12,23]



###### Table used for results for PROHA 2016 and PROLE 2016 slides ######
#      These values is for the simple explanation about how RL works
#      with the Convolution 2D kernel
#        A_0 [3,0,0,0,0,0,1,0,0,1,1,3,2,4,0],
#        A_1 [3,0,0,0,0,0,1,0,0,1,1,2,2,4,0],
#        A_2 [3,0,0,0,0,0,1,0,0,1,1,1,2,4,0],
#        A_3 [3,0,0,0,0,0,1,0,0,1,1,0,2,4,0],
#        A_4 [2,0,0,0,0,0,1,1,0,1,1,0,2,4,0]
#     actionT   = array([[1,0],
#                        [2,1],
#                        [3,2],
#                        [3,4],
#                        [4,4]])
#
#     initPos = [0]

#################################

    # stochasticity
    stochAction = 0.
    stochObs = 0.

    # def __init__(self, topology, goal, **args):
    def __init__(self, numStates, nActions, initPos, goal, table, **args):
        self.setArgs(**args)
        # self.mazeTable = topology
        self.numActions = nActions
        self.initPos = initPos
        self.goal = goal
        self.actionT = table

        # for all states reward 1. In MDP class, the reward for non-goal states is 0
        self.rewards = np.ones(numStates)

        # # Assign different rewards based on resulting performance
        # # self.rewards[37] = 1.41
        # # self.rewards[47] = 1.29
        # # self.rewards[61] = 2.81
        # Reward values used for preliminary evaluation for PROHA Workshop
        # self.rewards[37] = 2.0
        # self.rewards[47] = 1.5
        # self.rewards[61] = 100
        #
        # # if self.initPos == None:
        # #     self.initPos = self._freePos()
        # #     self.initPos.remove(self.goal)
        self.reset()

    def reset(self):
        """ return to initial position (stochastically): """
        self.bang = False
        self.perseus = choice(self.initPos)
        self.curAction = 0

    # def _freePos(self):
    #     """ produce a list of the free positions. """
    #     res = []
    #     for i, row in enumerate(self.mazeTable):
    #         for j, p in enumerate(row):
    #             if p == False:
    #                 res.append((i, j))
    #     return res

    # def _moveInDir(self, pos, dir):
    #     """ the new state after the movement in one direction. """
    #     return (pos[0] + dir[0], pos[1] + dir[1])

    def performAction(self, action):

        # if self.stochAction > 0:
        #     if random() < self.stochAction:
        #         action = choice(list(range(len(self.allActions))))
        # tmp = self._moveInDir(self.perseus, self.allActions[action])
        # if self.mazeTable[tmp] == False:
        #     self.perseus = tmp
        #     self.bang = False
        # else:
        #     self.bang = True

        # print("################# (%d,%d) #################" % (self.perseus,action))

        self.curAction = action
        self.perseus = self.actionT[(self.perseus,action)]

        if self.perseus == 0:
            self.bang = True
        else:
            self.bang = False

    # def getSensors(self):
    #
    #     obs = zeros(self.numActions)
    #     row = self.actionT[self.perseus]
    #     for i,value in enumerate(row):
    #         obs[i] = 0 if not(value==0) else 1
    #
    #     # obs = zeros(4)
    #     # for i, a in enumerate(Maze.allActions):
    #     #     obs[i] = self.mazeTable[self._moveInDir(self.perseus, a)]
    #     # if self.stochObs > 0:
    #     #     for i in range(len(obs)):
    #     #         if random() < self.stochObs:
    #     #             obs[i] = not obs[i]
    #     return obs

    def __str__(self):
        """ Ascii representation of the maze, with the current state """
        s = ''
        for r, row in reversed(list(enumerate(self.actionT))):
            for c, p in enumerate(row):
                # if self.actionT[(r, c)] == self.goal:
                if self.actionT[(r, c)] in self.goal:
                    s += '*'
                elif self.actionT[(r, c)] == self.perseus:
                    s += '@'
                elif p == True:
                    s += '#'
                else:
                    s += ' '
            s += '\n'
        return s


