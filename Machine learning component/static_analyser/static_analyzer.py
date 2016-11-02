#!/usr/bin/python

#-----------------------------------------------------------------
# Python program using pycparser library to extract code SCA features
# for code classification
#-----------------------------------------------------------------
from __future__ import print_function
import sys

# This is not required if you've installed pycparser into
# your site-packages/ with setup.py
sys.path.extend(['.', '..'])

from pycparser import c_parser, c_ast, parse_file, c_generator
import re,os

# Global variables

# Singleton reference to CGenerator for pretty print of stmts and compute
# rollUp pattern
generator = c_generator.CGenerator()

# Variables to compute code feature vecctor

funcToAnalyze            = "*"
blockToAnalyze           = "*"

maxForStmtDepth          = -1
curForStmtDepth          = -1

loopIterVarId            = []
loopLimitVarList         = []
checkLoopLimitMod        = 0
anyForLoopNonStaticLimit = 0

anyFuncCall              = 0

anyIfStmt                = 0

numIrregularForLoops     = 0

anyArrayWriteShifted     = 0

anyLoopSchedule          = 0

currentForSIMDloop       = 0
anyChildForLoop          = 0
anySIMDloop              = 0

numLoopInvVar            = 0

numLoopHoistedVarMods    = 0

non1DArrayNames          = []
numNon1Darray            = 0

numAuxVarArrayIndex      = 0

totalNumForLoops         = 0

numNonNormalizedForLoops = 0

NO_SCOPE                 = -1
currentScope             = NO_SCOPE
# 'scopeList', will be a list of sublists, where each sublist stores the
# variables of the current scope. The list 'scopeList' is updated within
# each 'Compound statement'. Only exception is for the case:
#
#          for(int i=...;i<...;i++)
#
# That declaration of 'i' within the for header, must be treated when
# visiting the for loop, and then not treated when visiting the compound
# stmt within the for loop.
scopeList                = []

# Global variables used to obtain the feature vector but not not members the feature vector

# Constant values that currentDeclType can have
NOT_IN_DECL              = 0
EMPTY_DECL               = 1
CONST_DECL               = 2
VAR_DECL                 = 3
currentDeclType          = NOT_IN_DECL

NO_ASSIGN                = 0
PURE_MOD_ASSIGN          = 1
READ_MOD_ASSIGN          = 2
currentAssignmentType    = NO_ASSIGN

NO_UNARY_OP              = 0
PRE_READ_MOD_UNARY_OP    = 1
POST_READ_MOD_UNARY_OP   = 2
currentUnaryOP           = NO_UNARY_OP

currentArrayRefName      = ""

isWithinArrayRef         = 0
isWithinRValue           = 0
isWithinLValue           = 0
isWithinArrayDecl        = 0

isWithinForNext          = 0
evalConstantVal          = 0

insideFuncCall           = 0

# Global vars related with For Cond
isWithinForCond          = 0
loopForCondVarFound      = 0

# insideFuncDef is used to detect global vars. If we 
# are inside a funcDef, var delcaration cannot be 
# global
insideFuncDef            = 0
usesGlobalVars           = 0
globalVarList            = []


varSetLValueOutsideLoop = []

numStmtsRollUp           = 0

# The following list are list of lists. Each sublist contains var IDs (RValue or LValue) that
# appear in the different levels of a nested loop. Thus, the first sublist corresponds to the
# outer most loop, the 2nd sublist to the first nested loop and so on. E.g.
#
# for(...) {
#     int v = 0;
#     for(...) {
#         aux = aux + v;
#     }
# }
#
# The content of the lists would be:
#
# varListLValueInsideLoop = [[v],[aux]]
# varListRValueInsideLoop = [[],[aux,v]]

varSetLValueInsideLoop  = []
varSetRValueInsideLoop  = []

# This dictionary indexed by code coord (i.e. line number) stores a list
# with the
pragmaDict = {}

# Since Pycparser does not provide line numbers for Compound stmts we compute
# it and store in a list. Later when traversin the AST, we count the number
# of Compound stmts visited and get the line number querying the list with the
# current count of Compound stmts
numCompoundStmts         = 0
numCompoundStmtsInBlock  = 0
lineBeginCompounds       = []
compoundBlockId2Analyze  = -1
isWithinBlock2Analyze    = True
currentCompoundStmtId    = -1


anyTernaryOp             = 0
anyUselessStmt           = 0

def isPragmaLine(line):
        m1 = re.match("\s*#pragma\s+(.*)",line)
        if m1:
            return 1
        else:
            return 0

def isEmptyLine(line):
        m1 = re.match("\\n",line)
        if m1:
            return 1
        else:
            return 0

# TODO: to extract #pragmas correctly it should be checked whether it is within a comment or not
def readPragmas(filename):
    f = open(filename)
    content = f.readlines()

    lineNum = 1
    for line in content:
        m1 = re.match("\s*#pragma\s+(.*)",line)
        if m1:
            # vectorStr = m1.group(1)
            # headerVector = [int(num) for num in vectorStr.split(",")]
            # print("$$$$ Pragma %s found at line %d !!!\n%s" % (m1.group(1),lineNum,content[lineNum-1]))
            line2Num = lineNum + 1
            for line2 in content[lineNum:len(content)-1]:
                if (not isPragmaLine(line2)) and (not isEmptyLine(line2)):
                    # print("#pragma %s - %d" % (m1.group(1),line2Num))
                    if line2Num not in pragmaDict.keys():
                        pragmaDict[line2Num] = []

                    pragmaDict[line2Num].append(m1.group(1))
                    break
                line2Num += 1
            # processPragma(m1.group(1))
        lineNum += 1

    f.close()

# Since Pycparser does not provide line numbers for Compound stmts we compute
# it and store in a list. Later when traversin the AST, we count the number
# of Compound stmts visited and get the line number querying the list with the
# current count of Compound stmts
# TODO: to extract Compound stmts lines correctly it should be checked whether
# it is within a comment or not
def readCompound(filename):
    global lineBeginCompounds

    f = open(filename)
    content = f.readlines()

    if funcToAnalyze != "*":
        funcFound = False
    else:
        funcFound = True

    lineNum = 1
    for line in content:

        m1 = re.match("(.*)\{(.*)",line)
        m2 = re.match("(.*)\s+(.*)\s*\((.*)",line)
        if m1 and (funcToAnalyze == "*" or funcFound):
            # print("$$$$ Begining of Compound %s found at line %d !!!\n" % (m1.group(0),lineNum))
            lineBeginCompounds.append(lineNum)
        elif m2:
            if m2.group(2) == funcToAnalyze:
                funcFound = True
                # print("Yesssssssssss %s" % m2.group(2))

        lineNum += 1

    f.close()


def areDisjointSets(set1,set2):

    areDisjoint = 1

    for elem1 in set1:
        if elem1 in set2:
            areDisjoint = 0
            break

    return areDisjoint

# This is defined as global function since it can be called from different clasess
def processPragmaList(pragmaList):
        global anyLoopSchedule,currentForSIMDloop

        isIterIndependent = 0
        readSet  = []
        writeSet = []

        for pragma in pragmaList:
            if "loop_schedule" in pragma:
                anyLoopSchedule = 1
                # print("### Pragma %s found!!! (%s)" % (pragma,"KKKKKK"))
            elif "iteration_independent" in pragma:
                isIterIndependent = 1
            elif "reads" in pragma:
                continue
            elif "writes" in pragma:
                continue
            elif "read" in pragma:
                # obtain the set of read variables from a pragma like
                # #pragma stml read v in {(0)}
                l = pragma.split(" ")
                readSet = [word.strip(' ') for word in l]
                # after strip by space, remove empty strings
                readSet = [word for word in readSet if (not word=="")]
                readSet.remove("read")
                readSet.remove("stml")
                readSet = readSet[0:readSet.index("in")]
                # print(readSet)
            elif "write" in pragma:
                # obtain the set of written variables from a pragma like
                # #pragma stml write v in {(0)}
                l = pragma.split(" ")
                writeSet = [word.strip(' ') for word in l]
                # after strip by space, remove empty strings
                writeSet = [word for word in writeSet if (not word=="")]
                # print(writeSet)
                writeSet.remove("write")
                writeSet.remove("stml")
                writeSet = writeSet[0:writeSet.index("in")]
                # print(writeSet)

        # Compute code properties requiring a combination of several properties
        if isIterIndependent and areDisjointSets(readSet,writeSet):
            currentForSIMDloop = 1


def getLineNum(nodeCoordStr):
    l = nodeCoordStr.split(":")
    return int(l[1])

def insertInSet(set,elem):
    if not elem in set:
        set.append(elem)

################################################################
#
#
#
################################################################
class ForVisitor(c_ast.NodeVisitor):

    def process_ForCond(self,node):
        global checkLoopLimitMod,anyForLoopNonStaticLimit,loopLimitVarList,loopIterVarId
        # node.show()
        condNode = node
        if condNode.__class__.__name__ == "Constant":
            # print("Cond is Constant %s" % condNode.value)
            if loopIterVarId[len(loopIterVarId)-1] != condNode.value:
                checkLoopLimitMod = 0

        elif condNode.__class__.__name__ == "FuncCall":
            # print("Cond is FuncCall %s" % condNode.name.name)
            if loopIterVarId[len(loopIterVarId)-1] != condNode.name.name:
                checkLoopLimitMod = 0
                anyForLoopNonStaticLimit = 1

        # elif condNode.__class__.__name__ == "ID":
        #     # print("Cond is ID %s" % condNode.name)
        #     if loopIterVarId[len(loopIterVarId)-1] != condNode.name:
        #         checkLoopLimitMod = 1
        #         loopLimitVarId = condNode.name

        else:
            self.visit(condNode)


    def processForPragmas(self,node):
        lineNum = getLineNum(str(node.coord))
        if lineNum in pragmaDict.keys():
            # print("++++ For at %s\n\t%s" % (node.coord,pragmaDict[lineNum]))
            processPragmaList(pragmaDict[lineNum])

    def visit_Decl(self,node):
        node.show()

    def visit_Constant(self,node):
        global evalConstantVal,isWithinForNext

        # We don't care about the sign of the loop iter step, but the value
        # So we consider a loop with step +1 and -1 as normalized
        if isWithinForNext:
            # node.show()
            evalConstantVal += int(node.value)

    def visit_UnaryOp(self,node):
        global evalConstantVal,isWithinForNext

        # We don't care about the sign of the loop iter step, but the value
        # So we consider a loop with step +1 and -1 as normalized
        if isWithinForNext:
            evalConstantVal += 1

    def visit_ID(self,node):
        global checkLoopLimitMod,loopLimitVarList#,loopForCondVarFound

        if isWithinForCond:
            # We can find for loops with cond in different ways:
            # for(iter=0;iter < c_exp;...)
            # and
            # for(iter=0;c_exp > iter;...)
            # so we need to know if loop iter var is found in loop cond
            # for processing properly the loop cond
            # if loopIterVarId[len(loopIterVarId)-1] == node.name:
            #     loopForCondVarFound = 1

            if loopIterVarId[len(loopIterVarId)-1] != node.name:
                checkLoopLimitMod = 1
                loopLimitVarList.append(node.name)


    def processForNext(self,node):
        global numNonNormalizedForLoops,evalConstantVal,isWithinForNext

        isWithinForNext = 1
        evalConstantVal = 0
        # if node.next.__class__.__name__ == "Assignment":

        # for c_name, c in node.children():
        #     self.visit(c)
        if not node.__class__.__name__ == "NoneType":
            self.visit(node)
            # print("EvalConstantVal %d - %s" % (evalConstantVal,node.coord))

        if not evalConstantVal == 1:
            numNonNormalizedForLoops += 1

        evalConstantVal = 0
        isWithinForNext = 0

    def visit_For(self, node):
        global checkLoopLimitMod,loopLimitVarList,loopIterVarId,isWithinForCond

        self.processForPragmas(node)

        # print('+------- Class: %s -------+' % (node.__class__.__name__))
        #print('Init:  %s' % (node.init))    

        
        # node.init.show()
        # if for loop is like: for(i=0;i...)
        if node.init.__class__.__name__ == "Assignment":  
            # node.init.show()
            # loopIterVarId = node.init.lvalue.name
            loopIterVarId.append(node.init.lvalue.name)
        # if for loop is like: for(int i=0;i...)
        elif node.init.__class__.__name__ == "DeclList":
            # print(node.init.decls[0].name)
            # node.init.decls[0].show()
            # loopIterVarId = node.init.decls[0].name
            loopIterVarId.append(node.init.decls[0].name)

        if node.cond.__class__.__name__ == "BinaryOp":
            # node.cond.show()
            isWithinForCond = 1
            self.process_ForCond(node.cond.left)
            self.process_ForCond(node.cond.right)
            isWithinForCond = 0

        self.processForNext(node.next)

        # node.next.show()
        # node.stmt.show()
        # print('Coord: %s' % (node.coord))
        # print('+-------------------------+')

    def generic_visit(self, node):
        # print('%s at %s' % (node.__class__.__name__, node.coord))
        # node.show()
        for c_name, c in node.children():
            self.visit(c)

################################################################
#
# A simple visitor for analyzing array subscript expressions
#
################################################################
class ArraySubscriptVisitor(c_ast.NodeVisitor):
    def __init__(self):
        self.opStack = []
        # print("ArraySubscriptVisitor")

    def visit_BinaryOp(self,node):
        self.opStack.append(node.op)

        for c_name, c in node.children():
            self.visit(c)

        self.opStack.remove(node.op)

    def visit_ID(self,node):
        global anyArrayWriteShifted,loopIterVarId,loopLimitVarList,currentArrayRefName,isWithinArrayRef

        # print(loopIterVarId)
        # print(loopLimitVarList)
        # print("-------------------")

        # currentOp = self.opStack[len(self.opStack)]
        # if currentOp == "*" or currentOp == "/":
        # print("KKKKKKKKKKKKKKK %s - %s - %s" % (currentArrayRefName,node.name,str(loopIterVarId)))
        if (not node.name in loopIterVarId) and (node.name != currentArrayRefName) and (not node.name in loopLimitVarList):
            anyArrayWriteShifted = 1


        for c_name, c in node.children():
            self.visit(c)

    def visit_Constant(self,node):
        global anyArrayWriteShifted

        # node.show()
        # print("#########  %s" % self.opStack[len(self.opStack)-1])

        if len(self.opStack) > 0:
            currentOp = self.opStack[len(self.opStack)-1]
            if currentOp == "+" or currentOp == "-":
                anyArrayWriteShifted = 1

        for c_name, c in node.children():
            self.visit(c)

    def generic_visit(self, node):
        # print('%s at %s' % (node.__class__.__name__, node.coord))
        # node.show()
        for c_name, c in node.children():
            self.visit(c)

################################################################
#
# A simple visitor for SCA and obtain a vector of code features
#
################################################################
class ASTVisitor(c_ast.NodeVisitor):

    def visit_FuncDef(self, node):
        global insideFuncDef,funcToAnalyze
        global currentScope,scopeList

        insideFuncDef = 1
        # In case of a FuncDef, the new scope is incremented in the
        # for including function arguments into the scope
        currentScope += 1
        scopeList.append([])

        # print('%s at %s' % (node.decl.name, node.decl.coord))
        if node.decl.name == funcToAnalyze or funcToAnalyze == "*":
                for c_name, c in node.children():
                        self.visit(c)

        # print("Vars for function %s" % (node.decl.name))
        # for elem in varSetLValueOutsideLoop:
        #     print("\t%s" % (elem))

        insideFuncDef = 0
        # In case of a FuncDef, the new scope is decremented in the
        # for removing function arguments from the scope
        scopeList.remove(scopeList[currentScope])
        currentScope -= 1


    def visit_TypeDecl(self, node):
        global insideFuncDef, globalVarList,varSetLValueInsideLoop

        if not insideFuncDef: # definition of global var, save var id
            # print('Global var %s decl at %s' % (node.declname, node.coord))
            globalVarList.append(node.declname)
        else: # we are in var decl within a function
            if (not isWithinArrayDecl) and (not currentDeclType == EMPTY_DECL):
                # we are within a loop and the var in node.declname is not the loop iteration variable
                if (curForStmtDepth > -1) and (not node.declname in loopIterVarId):
                    insertInSet(varSetLValueInsideLoop[curForStmtDepth],node.declname)
                    # print("Decl LVal: %s - %s" % (node.declname,node.coord))
                else:
                    if curForStmtDepth == -1: # we are outside the scope of any for loop
                #     print(node.declname)
                        insertInSet(varSetLValueOutsideLoop,node.declname)

        for c_name, c in node.children():
            self.visit(c)


    def visit_ArrayDecl(self,node):
        global isWithinArrayDecl

        isWithinArrayDecl = 1

        # node.show()
        for c_name, c in node.children():
            self.visit(c)

        isWithinArrayDecl = 0

    def visit_ArrayRef(self,node):
        global isWithinArrayRef,numNon1Darray,currentArrayRefName,non1DArrayNames

        # store the name of the array for later use
        # Case: (*arrName)[...]
        if node.name.__class__.__name__ == "UnaryOp":
            currentArrayRefName = node.name.expr.name
        else: # Case: arrName[...] , arrName[...][...] , ...
            curID = node.name
            # In a multidimensional ArrayRef, the array variable name is first ID child
            # within the deepest ArrayRef (see comment below). So we iterate over all
            # the AST until we reach a node which is of ID and the slot .name is of type
            # str. In that case we reached the name of the ArrayRef variable
            while(not type(curID) is str):
                curID = curID.name
            currentArrayRefName = curID #node.name.name
            # print("else - %s %s" % (currentArrayRefName,node.name))
            # node.show()
            # print("------------------")

        # the array access z[i][j][k] is represented in the AST as:
        # ArrayRef:
        #   ArrayRef:
        #     ArrayRef:  <- node.name.class == "ArrayRef"
        #       ID: z    <- node.name.class != "ArrayRef"
        #       ID: i
        #     ID: j
        #   ID: k
        # So if we are in the deepest index within the stack of array indexes and we were
        # already in an ArryaRef
        if (not node.name.__class__.__name__ == "ArrayRef") and (isWithinArrayRef > 0) and (not currentArrayRefName in non1DArrayNames):
            non1DArrayNames.append(currentArrayRefName)
            numNon1Darray += 1

        isWithinArrayRef += 1

        if curForStmtDepth > -1 and isWithinLValue: # If we are within a for loop
            # node.show()
            # print("+++++++++")
            v = ArraySubscriptVisitor()
            v.visit(node)


        for c_name, c in node.children():
            self.visit(c)

        isWithinArrayRef -= 1
        currentArrayRefName = ""

    # def visit_Decl(self, node):
    #     global currentScope, scopeList
    #
    #     print('%s (%s) at %s' % (node.__class__.__name__,node.children()[0][1].__class__.__name__, node.coord))
    #
    #     if node.children()[0][1].__class__.__name__ == "TypeDecl" and currentScope != NO_SCOPE:
    #         # print("\t%s" % node.children()[0][1].declname)
    #         scopeList[currentScope].append(node.children()[0][1].declname)
    #
    #         curScopeStr = ''
    #         for scp in scopeList:
    #             for var in scp:
    #                 curScopeStr += "%s " % var
    #         print("\tCurrent scope: %s" % curScopeStr)
    #
    #     for c_name, c in node.children():
    #         self.visit(c)


    def visit_Decl(self, node):
        global insideFuncDef, globalVarList, usesGlobalVars,numLoopInvVar,currentDeclType
        global currentScope, scopeList

        # # we are in a variable declaration stmt
        # # we are within for loop and current var ID is not an array
        # # and is not of of the form "int var_id = 0;" -> Constant
        # # and is not of of the form "int var_id;" -> NoneType
        # if (curForStmtDepth > -1) and (not isWithinArrayRef) and \
        # (not node.init.__class__.__name__ == "Constant") and \
        # (not node.init.__class__.__name__ == "NoneType"):
        #     print("Decl RValue: %s - %s" % (node.init.name,node.coord)) #node.name
        #
        #     numLoopInvVar += 1

        if node.init.__class__.__name__ == "NoneType":
            currentDeclType = EMPTY_DECL
        elif node.init.__class__.__name__ == "Constant":
            currentDeclType = CONST_DECL
        else:
            currentDeclType = VAR_DECL

        ################# begin code for managing variable scope #################
        # if node.children()[0][1].__class__.__name__ == "TypeDecl":
        #     print('%s %s (%s) at %s' % (node.__class__.__name__,node.children()[0][1].declname,node.children()[0][1].__class__.__name__, node.coord))
        if node.children()[0][1].__class__.__name__ == "TypeDecl" and currentScope != NO_SCOPE:
            # print("\t%s" % node.children()[0][1].declname)
            scopeList[currentScope].append(node.children()[0][1].declname)

            # curScopeStr = ''
            # for scp in scopeList:
            #     for var in scp:
            #         curScopeStr += "%s " % var
            # print("\tCurrent scope: %s" % curScopeStr)
        ################# end code for managing variable scope #################

        for c_name, c in node.children():
            self.visit(c)

        currentDeclType = NOT_IN_DECL

    def visit_ID(self, node):
        global insideFuncDef, globalVarList, usesGlobalVars,numLoopInvVar,varSetLValueInsideLoop,varSetRValueInsideLoop, \
        currentAssignmentType,currentUnaryOP,numAuxVarArrayIndex,currentArrayRefName,checkLoopLimitMod,loopLimitVarList, \
        anyForLoopNonStaticLimit,anyArrayWriteShifted

        if insideFuncDef:
            if node.name in globalVarList:
                usesGlobalVars = 1

        # we are within for loop and current var ID is not a loop iter var and is not an array
        if (not isWithinArrayRef) and (not insideFuncCall):
            if (curForStmtDepth > -1) and (not node.name in loopIterVarId):
                if isWithinLValue:
                # node.show()
                    insertInSet(varSetLValueInsideLoop[curForStmtDepth],node.name)
                    if currentAssignmentType == READ_MOD_ASSIGN:
                        insertInSet(varSetRValueInsideLoop[curForStmtDepth],node.name)
                    # print("LValue: %s - %s" % (node.name,node.coord)) #node.name
                elif isWithinRValue:
                    insertInSet(varSetRValueInsideLoop[curForStmtDepth],node.name)
                    # print("RValue: %s - %s" % (node.name,node.coord)) #node.name
                elif currentDeclType == VAR_DECL:
                    insertInSet(varSetRValueInsideLoop[curForStmtDepth],node.name)
                    # print("Decl RVal: %s - %s" % (node.name,node.coord)) #node.name
                elif currentUnaryOP == PRE_READ_MOD_UNARY_OP or currentUnaryOP == POST_READ_MOD_UNARY_OP:
                    insertInSet(varSetLValueInsideLoop[curForStmtDepth],node.name)
                    insertInSet(varSetRValueInsideLoop[curForStmtDepth],node.name)
            else: # we are outside any for loop
                if curForStmtDepth == -1 and isWithinLValue:
                    insertInSet(varSetLValueOutsideLoop,node.name)
                # if currentDeclType == VAR_DECL:
                # varSetLValueOutsideLoop.append(node.name)
                #     print("%s - %s" % (node.name,node.coord)) #node.name
                #     node.show()
        elif isWithinArrayRef > 0:
            if (curForStmtDepth > -1):
                if (not node.name in loopIterVarId) and (node.name != currentArrayRefName) and (not node.name in loopLimitVarList):
                    # print("####### %s" % (node.name))
                    numAuxVarArrayIndex += 1


        # if curForStmtDepth > -1: # If we are within a for loop
        #     if (isWithinArrayRef > 0) and isWithinLValue:
        #         if (not node.name in loopIterVarId) and (node.name != currentArrayRefName) and (not node.name in loopLimitVarList):
        #             # print("---------------------- %s" % (loopIterVarId))
        #             node.show()
        #             print("-------- %d" % (isWithinArrayRef))
        #             anyArrayWriteShifted = 1
        #         # else:
        #         #     # print("---------------------- %s" % (loopIterVarId))
        #         #     node.show()
        #         #     print("-------- %d" % (isWithinArrayRef))
        #         #     anyArrayWriteShifted = 1

        if checkLoopLimitMod and currentUnaryOP != NO_UNARY_OP:
            if node.name in loopLimitVarList:
                # print("Checking loop limit %s - %s" % (node.lvalue.name,loopLimitVarId))
                anyForLoopNonStaticLimit = 1
                # Clean global variables for processing next For loop correctly
                # print("Cleaning for global vars")
                loopLimitVarList  = []
                checkLoopLimitMod = 0

        for c_name, c in node.children():
            self.visit(c)

    # def visit_FuncCall(self,node):
    #     global
    #
    #     insideFuncCall = 1
    #
    #     node.show()
    #     for c_name, c in node.children():
    #         self.visit(c)
    #
    #     insideFuncCall = 0


    def visit_FuncCall(self, node):
        global anyFuncCall,insideFuncCall
        
        # print('%s at %s' % (node.name.name, node.coord))
        anyFuncCall = 1
        insideFuncCall = 1

        for c_name, c in node.children():
            self.visit(c)

        insideFuncCall = 0

    def visit_If(self, node):
        global anyIfStmt

        # print('%s at %s' % (node.__class__.__name__, node.coord))
        anyIfStmt = 1
        for c_name, c in node.children():
            self.visit(c)

    def computeLoopInvVar(self,setLValue,setRValue):
        global numLoopInvVar

        for rVar in setRValue:
            if not rVar in setLValue:
                # print("------- %s" % rVar)
                numLoopInvVar += 1

    def computeHoistedVarMod(self,outerSetLValue,innerSetLValue):
        global numLoopHoistedVarMods

        # print("%s - %s" % (str(outerSetLValue),str(innerSetLValue)))
        for iVar in innerSetLValue:
            if iVar in outerSetLValue:
                # print("%s - %s" % (iVar,str(outerSetLValue)))
                numLoopHoistedVarMods += 1

    def isRolledUpFor(self,node):
        # print("%s: %s" % (str(node.coord),generator.visit(node)))
        lineNum = getLineNum(str(node.coord))
        # print(pragmaDict)

        rolledUp = 0
        if lineNum in pragmaDict.keys():
            # print("++++ For at %s\n\t%s" % (node.coord,pragmaDict[lineNum]))

            for pragma in pragmaDict[lineNum]:
                if "rolled-up" in pragma:
                    rolledUp = 1

        return rolledUp

    def getPOLCAblockName(self,lineNum):
        # print("%d - %s" % (lineNum,str(pragmaDict)))

        if lineNum in pragmaDict.keys():
            for pragma in pragmaDict[lineNum]:
                m1 = re.match(".*def\s+(.*)",pragma)
                if m1:
                    # print(m1.group(1))
                    return m1.group(1)

        return ""

    def visit_Compound(self, node):
        global currentScope, scopeList, curForStmtDepth, insideFuncDef
        global numStmtsRollUp,blockToAnalyze,numCompoundStmts,lineBeginCompounds,compoundBlockId2Analyze
        global isWithinBlock2Analyze,currentCompoundStmtId,numCompoundStmtsInBlock,anyUselessStmt



        # print("%d - %s" % (numCompoundStmts,str(lineBeginCompounds)))
        # print('%s at %s (%s)' % (node.__class__.__name__, lineBeginCompounds[numCompoundStmts],node.coord))

        scopeList.append([])

        if self.getPOLCAblockName(lineBeginCompounds[numCompoundStmts]) == blockToAnalyze:
            # print("Init vars for BLOCK!!!!!")
            initVars(INIT_BLOCK)
            compoundBlockId2Analyze = numCompoundStmts
            isWithinBlock2Analyze   = True

            self.checkUselessStmt(node)

            # node.show(showcoord=True,nodenames=True,attrnames=True)

        if isWithinBlock2Analyze:
            numCompoundStmtsInBlock += 1

        currentCompoundStmtId = numCompoundStmts
        numCompoundStmts+=1

        # # blockToAnalyze
        # print('%s at %s' % (node.__class__.__name__, node.coord))
        # # print(node.block_items)
        # lineNum = getLineNum(str(node.coord))
        # if lineNum in pragmaDict.keys():
        #     print("++++ Compound at %s\n\t%s" % (node.coord,pragmaDict[lineNum]))
        #     # processPragmaList(pragmaDict[lineNum])

        # if not within a for or funcDef, scope must me managed in the
        # Compound stmt
        if curForStmtDepth == -1 or insideFuncDef == 0:
            currentScope += 1
            scopeList.append([])

        # print('%s at %s' % (node.__class__.__name__, node.coord))

        c_name, prev_stmt = node.children()[0]
        stmtsPairsRollUp = 0
        foundRolledUpFor = 0
        for i in range(1,len(node.children())):
            pStmtStr = generator.visit(prev_stmt)

            c_name, next_stmt = node.children()[i]
            nStmtStr = generator.visit(next_stmt)
            # We skip statements which are pragmas since they are not of interest for roll-up pattern
            if "#pragma" in nStmtStr or "#pragma" in pStmtStr:
                continue
            # print("%s - %s" % (type(stmt),stmt))

            # We limit the type of stmts to check for rollUp pattern to Assignment and both stmts
            # that are compared must be same type of operator, i.e., '=', '+=', '-='
            # We also include the case of rolled-up for, for detecting pending stmts to roll-up,
            # but it is assumed that the rolled-up for cannot be in the 'next_stmt'
            isRolledUpFor = self.isRolledUpFor(prev_stmt)
            if ((prev_stmt.__class__.__name__ == next_stmt.__class__.__name__ == "Assignment") and \
                (prev_stmt.op == next_stmt.op)) or \
                (isRolledUpFor and next_stmt.__class__.__name__ == "Assignment"):
                # print("##################################")
                # print("%d: %s (%s)" % (i,pStmtStr,prev_stmt.__class__.__name__))
                # print("----------")
                # print("%d: %s (%s)" % (i+1,nStmtStr,next_stmt.__class__.__name__))
                if not isRolledUpFor:
                    val = os.system('./stmtDiff.py "%s" "%s"' % (pStmtStr,nStmtStr))
                else:
                    bodyStmtStr = generator.visit(prev_stmt.stmt)
                    val = os.system('./stmtDiff.py "%s" "%s"' % (bodyStmtStr,nStmtStr))
                    if val != 0:
                        foundRolledUpFor = 1
                    else:
                        foundRolledUpFor = 0

                if val != 0:
                    stmtsPairsRollUp += 1
                else:
                    if stmtsPairsRollUp != 0:
                        # In case there is just one stmt that is rollable but due to a
                        # rolledUpFor, then we do not add the 1 to compute correctly the
                        # number of rollUp stmts. For example, in this case the rolled-up
                        # for does not count, so rollUp stmts should be 1 and not 2:
                        #             #pragma stml rolled-up
                        #             for(int k=0;k<2;k++)
                        #               F[k] = 0.0f;
                        #             F[2] = 0.0f;
                        numStmtsRollUp += stmtsPairsRollUp + (not foundRolledUpFor) * 1
                        stmtsPairsRollUp = 0
                        foundRolledUpFor = 0
                # print(val)

            # only update 'prev_stmt' if 'next_stmt' is an assignment, otherwise we fail to count
            # correctly number of rollUp stmts since one condition to increment and reset is when
            # a stmt is different with respect the next stmt
            # We also include the case 'next_stmt' is a rolled-up for
            if next_stmt.__class__.__name__ == "Assignment" or self.isRolledUpFor(next_stmt):
                prev_stmt = next_stmt

        # In case we found at least one pair of stmts with rollUp pattern, then the number
        # of stmts with rollUp pattern is the number of pairs + 1
        if stmtsPairsRollUp != 0:
            numStmtsRollUp += stmtsPairsRollUp + (not foundRolledUpFor) * 1


        for c_name, c in node.children():
            self.visit(c)


        if isWithinBlock2Analyze and compoundBlockId2Analyze == currentCompoundStmtId:
            isWithinBlock2Analyze = False

        currentCompoundStmtId -= 1



        # if not within a for or funcDef, scope must me managed in the
        # Compound stmt
        if curForStmtDepth == -1 or insideFuncDef == 0:
            scopeList.remove(scopeList[currentScope])
            currentScope -= 1

            # if currentScope != -1:
            #     print("---- %d: %s" % (currentScope,scopeList[currentScope]))


    def visit_For(self, node):
        global maxForStmtDepth,curForStmtDepth,anySIMDloop,currentForSIMDloop,anyChildForLoop,varSetLValueInsideLoop, \
        varSetRValueInsideLoop,totalNumForLoops,loopLimitVarList
        global currentScope, scopeList

        # In case of a For loop, the new scope is incremented in the
        # For header for the case a variable is declared within the
        # For header, i.e., for(int i=0;i...) or when the body of the
        # For loop just contains one stmt without compound stmt
        currentScope += 1
        scopeList.append([])


        totalNumForLoops += 1
        # node.show()
        currentForSIMDloop = 0
        v = ForVisitor()
        v.visit(node)

        # save the property in case it is reset by any child for loop
        thisForSIMDloop = currentForSIMDloop
        # In case this for loop has the code properties from pragmas to be SIMD, check
        # the other condition, i.e. that it has no nested loops inside. For that, reset
        # the flag anyChildForLoop, for not using a value set a previous SIMD analysis.
        if thisForSIMDloop:
            anyChildForLoop = 0

        curForStmtDepth = curForStmtDepth + 1
        if maxForStmtDepth < curForStmtDepth:
            maxForStmtDepth = curForStmtDepth

        varSetLValueInsideLoop.append([])
        varSetRValueInsideLoop.append([])

        for c_name, c in node.children():
            # print('%s at %s' % (c.__class__.__name__, c.coord))
            self.visit(c)

        # In case of a For loop, the new scope is decremented in the
        # For header for the case a variable is declared within the
        # For header, i.e., for(int i=0;i...) or when the body of the
        # For loop just contains one stmt without compound stmt
        scopeList.remove(scopeList[currentScope])
        currentScope -= 1

        loopIterVarId.remove(loopIterVarId[len(loopIterVarId)-1])

        # If this for loop has the code properties from pragmas for being SIMD and it has
        # not any nested For loop, activate the code feature anySIMDloop
        # if thisForSIMDloop and not anyChildForLoop:
        
        # For the moment the decission of SIMD loop is based on annotations. It is left
        # for the future to check correctly inner loops with respect to the SIMD loop. For
        # example a convolution will have inner loop but still will be SIMD loop because
        # iteration space of inner loops is much smaller and iteration limits are tipically 
        # constants
        if thisForSIMDloop:
            anySIMDloop = 1

        # tabStr = ""
        # for i in range(0,curForStmtDepth):
        #     tabStr += "\t"
        # print("%sVars for loop %d" % (tabStr,curForStmtDepth))
        # print("%s\tLValue" % (tabStr))
        # for elem in varSetLValueInsideLoop[curForStmtDepth]:
        #     print("%s\t- %s" % (tabStr,elem))
        # print("%s\tRValue" % (tabStr))
        # for elem in varSetRValueInsideLoop[curForStmtDepth]:
        #     print("%s\t- %s" % (tabStr,elem))

        self.computeLoopInvVar(varSetLValueInsideLoop[curForStmtDepth],varSetRValueInsideLoop[curForStmtDepth])

        # We want to count the number of variables modified inside the loop that are initialized outside this for
        # loop, i.e. in all the outer loops plus the function where the loop is
        for i in range(0,curForStmtDepth):
            self.computeHoistedVarMod(varSetLValueInsideLoop[i],varSetLValueInsideLoop[curForStmtDepth])
        self.computeHoistedVarMod(varSetLValueOutsideLoop,varSetLValueInsideLoop[curForStmtDepth])

        varSetLValueInsideLoop[curForStmtDepth] = []
        varSetRValueInsideLoop[curForStmtDepth] = []

        # We set this flag to true. In this way parents nodes in the AST will know that
        # a child node is a For loop
        anyChildForLoop = 1
        curForStmtDepth = curForStmtDepth - 1

        # In a set of nested for loops if we get out from the outer most loop we
        # have to reset the list of loop limit list (i.e. loopLimitVarList)
        if curForStmtDepth == -1:
            loopLimitVarList = []

    def process_BreakAndContinue(self,node):
        global curForStmtDepth,numIrregularForLoops

        if curForStmtDepth > -1: # If we are within a for loop
            numIrregularForLoops = numIrregularForLoops + 1

    def visit_Break(self, node):
        self.process_BreakAndContinue(node)

        for c_name, c in node.children():
            self.visit(c)

    def visit_Continue(self, node):
        self.process_BreakAndContinue(node)

        for c_name, c in node.children():
            self.visit(c)

    def visit_UnaryOp(self,node):
        global currentUnaryOP

        if node.op == "p++" or node.op == "p--":
            currentUnaryOP = POST_READ_MOD_UNARY_OP
        elif node.op == "++p" or node.op == "--p":
            currentUnaryOP = PRE_READ_MOD_UNARY_OP

        # print(node.op)
        for c_name, c in node.children():
            self.visit(c)

        currentUnaryOP = NO_UNARY_OP

    def visit_TernaryOp(self,node):
        global anyTernaryOp

        anyTernaryOp = 1

        for c_name, c in node.children():
            self.visit(c)


    def visit_Assignment(self, node):
        global checkLoopLimitMod, loopLimitVarList, anyForLoopNonStaticLimit,anyArrayWriteShifted,loopIterVarId,\
        isWithinRValue,isWithinLValue,numLoopInvVar,currentAssignmentType

        # print('%s at %s withinLoop %d %d' % (node.__class__.__name__, node.coord,curForStmtDepth,checkLoopLimitMod))
        # node.show()        

        if checkLoopLimitMod:
            if node.lvalue.name in loopLimitVarList:
                # print("Checking loop limit %s - %s" % (node.lvalue.name,loopLimitVarId))
                anyForLoopNonStaticLimit = 1
                # Clean global variables for processing next For loop correctly
                # print("Cleaning for global vars")
                loopLimitVarList  = []
                checkLoopLimitMod = 0

        # if curForStmtDepth > -1: # If we are within a for loop
        #     if node.lvalue.__class__.__name__ == "ArrayRef":
        #         # print('%s at %s' % (node.lvalue.__class__.__name__, node.lvalue.coord))
        #         # node.lvalue.show()
        #         if node.lvalue.subscript.__class__.__name__ == "ID":
        #             if not node.lvalue.subscript.name in loopIterVarId:
        #                 # print("---------------------- %s" % (loopIterVarId))
        #                 # node.lvalue.show()
        #                 anyArrayWriteShifted = 1
        #         else:
        #             # print("---------------------- %s" % (loopIterVarId))
        #             # node.lvalue.show()
        #             anyArrayWriteShifted = 1
        #     # elif node.lvalue.__class__.__name__ == "ID" and (not node.lvalue.name in loopIterVarId):
        #     #     print("%s - %s" % (node.lvalue.name,node.coord))
        #     # isWithinRValue
        #     # print(node.rvalue)

        # print("--------------------")
        # for c_name, c in node.children():
        #     c.show()
        #     # self.visit(c)
        # print("--------------------")

        if node.op == "=":
            currentAssignmentType = PURE_MOD_ASSIGN
        elif node.op == "+=" or node.op == "-=":
            currentAssignmentType = READ_MOD_ASSIGN

        isWithinLValue = 1
        self.visit(node.lvalue)
        isWithinLValue = 0

        isWithinRValue = 1
        self.visit(node.rvalue)
        isWithinRValue = 0

        # Once we go out from Assignment node, we reset the flag
        currentAssignmentType = NO_ASSIGN


    # def visit_Pragma(self, node):
    #     # global anyLoopSchedule
    #
    #     print('%s (%s) at %s' % (node.__class__.__name__,node.string, node.coord))
    #
    #     print(len(node.children()))
    #
    #     # for c_name, c in node.children():
    #     #     self.visit(c)

    def checkUselessStmt(self,node):
        global anyUselessStmt

        if node.__class__.__name__ == 'Compound':
            for c_name, c in node.children():
                if c.__class__.__name__ == 'Constant':
                    # c.show(showcoord=True)
                    anyUselessStmt = 1

    def visit(self, node):
        global isWithinBlock2Analyze,anyUselessStmt

        if isWithinBlock2Analyze == True:
            self.checkUselessStmt(node)

            method = 'visit_' + node.__class__.__name__
            visitor = getattr(self, method, self.generic_visit)
            return visitor(node)
        else:
            return

    def generic_visit(self, node):
        # print('%s at %s' % (node.__class__.__name__, node.coord))
        # node.show()
        for c_name, c in node.children():
            self.visit(c)


# class IfVisitor(c_ast.NodeVisitor):
#     def visit_If(self, node):
#         print('%s at %s' % (node.cond, node.coord))

class GenericVisitor(c_ast.NodeVisitor):

    # def visit_Compound(self, node):
    #     global currentScope, scopeList
    #
    #     currentScope += 1
    #     scopeList.append([])
    #
    #     print('%s at %s' % (node.__class__.__name__, node.coord))
    #     # for c_name, c in node.children():
    #     #     generator = c_generator.CGenerator()
    #     #     stmt = generator.visit(c)
    #     #     print("%s - %s" % (type(stmt),stmt))
    #
    #     for c_name, c in node.children():
    #         self.visit(c)
    #
    #     print("---- %d: %s" % (currentScope,scopeList[currentScope]))
    #     scopeList.remove(scopeList[currentScope])
    #     currentScope -= 1
    #     if currentScope != -1:
    #         print("---- %d: %s" % (currentScope,scopeList[currentScope]))

    # def visit_Decl(self, node):
    #     global currentScope, scopeList
    #
    #     print('%s (%s) at %s' % (node.__class__.__name__,node.children()[0][1].__class__.__name__, node.coord))
    #
    #     if node.children()[0][1].__class__.__name__ == "TypeDecl" and currentScope != NO_SCOPE:
    #         # print("\t%s" % node.children()[0][1].declname)
    #         scopeList[currentScope].append(node.children()[0][1].declname)
    #
    #         curScopeStr = ''
    #         for scp in scopeList:
    #             for var in scp:
    #                 curScopeStr += "%s " % var
    #         print("\tCurrent scope: %s" % curScopeStr)
    #
    #     for c_name, c in node.children():
    #         self.visit(c)


    def generic_visit(self, node):
        # print('%s at %s' % (node.__class__.__name__, node.coord))
        # if node.__class__.__name__ == "Assignment" or node.__class__.__name__ == "TypeDecl" or \
        # node.__class__.__name__ ==  "IdentifierType":
        #     node.show()

        # if node.__class__.__name__ == "Decl":
        #     if (not node.init.__class__.__name__ == "Constant") and (not node.init.__class__.__name__ == "NoneType"):
        #         print("Decl - %s (%s)" % (node.init.name,node.init.__class__.__name__))
        #     # node.show()

        # if node.__class__.__name__ == "ArrayRef":
        #     # node.show()
        #     print("ArrayRef - %s , %s" % (node.name,node.subscript))

        # for c_name, c in node.children():
        #     self.visit(c)
        node.show(showcoord=True)

class PragmaParserVisitor(c_ast.NodeVisitor):

    def generic_visit(self, node):
        # print('%s at %s' % (node.__class__.__name__, node.coord))
        # if node.__class__.__name__ == "Assignment" or node.__class__.__name__ == "TypeDecl" or \
        # node.__class__.__name__ ==  "IdentifierType":
        #     node.show()

        # if node.__class__.__name__ == "Decl":
        #     if (not node.init.__class__.__name__ == "Constant") and (not node.init.__class__.__name__ == "NoneType"):
        #         print("Decl - %s (%s)" % (node.init.name,node.init.__class__.__name__))
        #     # node.show()

        # if node.__class__.__name__ == "ArrayRef":
        #     # node.show()
        #     print("ArrayRef - %s , %s" % (node.name,node.subscript))

        prevStmt = None
        for c_name, c in node.children():
            if prevStmt != None and prevStmt.__class__.__name__ == "Pragma":
        #     # node.show()
                print("Pragma - %s" % (prevStmt.string))
                print("\tOver %s: %s" % (c.__class__.__name__,c.coord))
                # c.show()

            self.visit(c)
            prevStmt = c
        # node.show()

def show_func_defs(filename):

    # Note that cpp is used. Provide a path to your own cpp or
    # make sure one exists in PATH.
    ast = parse_file(filename, use_cpp=True)

    # if showAST == 1, print the whole AST for testing purposes
    # if showAST == 0, run the SCA to extract code features vector
    showAST = 0
    if showAST:
        v = GenericVisitor()
        v.generic_visit(ast)
    else:
        v = ASTVisitor()
        v.generic_visit(ast)


def parsePragmas(filename):

    # Note that cpp is used. Provide a path to your own cpp or
    # make sure one exists in PATH.
    ast = parse_file(filename, use_cpp=True)

    v = PragmaParserVisitor()
    v.generic_visit(ast)


INIT_AST   = 0
INIT_BLOCK = 1

def initVars(initType):
    global maxForStmtDepth, curForStmtDepth,loopIterVarId,loopLimitVarList,checkLoopLimitMod,anyForLoopNonStaticLimit,\
    anyFuncCall,anyIfStmt,numIrregularForLoops,anyArrayWriteShifted,insideFuncDef,usesGlobalVars,globalVarList,\
    currentForSIMDloop,anyChildForLoop,anySIMDloop,anyLoopSchedule,numLoopInvVar,numLoopHoistedVarMods,isWithinArrayRef,\
    isWithinRValue,isWithinLValue,currentDeclType,isWithinArrayDecl,currentAssignmentType,currentUnaryOP,numNon1Darray, \
    numAuxVarArrayIndex,totalNumForLoops,numNonNormalizedForLoops

    global evalConstantVal,isWithinForNext,insideFuncCall,currentArrayRefName,isWithinForCond,loopForCondVarFound

    global pragmaDict,varSetLValueOutsideLoop,varSetLValueInsideLoop,varSetRValueInsideLoop,non1DArrayNames,currentScope, \
    scopeList
    global numStmtsRollUp,lineBeginCompounds,numCompoundStmts,compoundBlockId2Analyze
    global isWithinBlock2Analyze,currentCompoundStmtId,numCompoundStmtsInBlock,anyTernaryOp,anyUselessStmt

    if(initType == INIT_AST):
        loopIterVarId            = []
        loopLimitVarList         = []

        currentScope             = -1
        scopeList                = []

        # insideFuncDef is used to detect global vars. If we
        # are inside a funcDef, var delcaration cannot be
        # global
        insideFuncDef            = 0

        globalVarList            = []

        pragmaDict               = {}
        numCompoundStmts         = 0
        numCompoundStmtsInBlock  = 0
        lineBeginCompounds       = []
        compoundBlockId2Analyze  = -1
        isWithinBlock2Analyze    = True
        currentCompoundStmtId    = -1


        curForStmtDepth          = -1

        checkLoopLimitMod        = 0

        currentArrayRefName      = ""

        isWithinArrayRef         = 0
        isWithinRValue           = 0
        isWithinLValue           = 0
        currentDeclType          = NOT_IN_DECL
        isWithinArrayDecl        = 0

        currentAssignmentType    = NO_ASSIGN

        currentUnaryOP           = NO_UNARY_OP

        isWithinForNext          = 0
        evalConstantVal          = 0

        isWithinForCond          = 0
        loopForCondVarFound      = 0

        insideFuncCall           = 0

        varSetLValueOutsideLoop = []

        varSetLValueInsideLoop  = []
        varSetRValueInsideLoop  = []


    ############################

    maxForStmtDepth          = -1
    anyFuncCall              = 0
    anyArrayWriteShifted     = 0
    numIrregularForLoops     = 0
    usesGlobalVars           = 0
    anyIfStmt                = 0
    anyForLoopNonStaticLimit = 0


    anySIMDloop              = 0
    currentForSIMDloop       = 0
    anyChildForLoop          = 0

    anyLoopSchedule          = 0

    numLoopInvVar            = 0

    numLoopHoistedVarMods    = 0

    non1DArrayNames          = []
    numNon1Darray            = 0


    numAuxVarArrayIndex      = 0

    totalNumForLoops         = 0
    numNonNormalizedForLoops = 0
    numStmtsRollUp           = 0

    numCompoundStmtsInBlock  = 0
    compoundBlockId2Analyze  = -1
    isWithinBlock2Analyze    = True
    currentCompoundStmtId    = -1

    anyTernaryOp             = 0
    anyUselessStmt           = 0


def generateStats():

    statsStr = ""
    statsStr += "//# maxForStmtDepth:            %2d\n" % (maxForStmtDepth)
    statsStr += "//# anyFuncCall:                %2d\n" % (anyFuncCall)
    statsStr += "//# anyArrayWriteShifted:       %2d\n" % (anyArrayWriteShifted)
    statsStr += "//# numIrregularForLoops:       %2d\n" % (numIrregularForLoops)
    statsStr += "//# usesGlobalVars:             %2d\n" % (usesGlobalVars)
    statsStr += "//# anyIfStmt:                  %2d\n" % (anyIfStmt)
    statsStr += "//# allForLoopWithStaticLimit:  %2d\n" % (not anyForLoopNonStaticLimit)
    statsStr += "//# anySIMDloop:                %2d\n" % (anySIMDloop)
    statsStr += "//# anyLoop_Schedule:           %2d\n" % (anyLoopSchedule)
    statsStr += "//# numLoopInvVar:              %2d\n" % (numLoopInvVar)
    statsStr += "//# numLoopHoistedVarMods:      %2d\n" % (numLoopHoistedVarMods)
    statsStr += "//# numNon1Darray:              %2d\n" % (numNon1Darray)
    statsStr += "//# numAuxVarArrayIndex:        %2d\n" % (numAuxVarArrayIndex)
    statsStr += "//# totalNumForLoops:           %2d\n" % (totalNumForLoops)
    statsStr += "//# numNonNormalizedForLoops:   %2d\n" % (numNonNormalizedForLoops)
    statsStr += "//# numStmtsRollUp:             %2d\n" % (numStmtsRollUp)
    statsStr += "//# numCompoundStmts:           %2d\n" % (numCompoundStmtsInBlock)
    statsStr += "//# anyTernaryOp:               %2d\n" % (anyTernaryOp)
    statsStr += "//# anyUselessStmt:             %2d"   % (anyUselessStmt)



    return statsStr


# Function for:
#  - Commenting out header files, and avoid pycparser errors
#  - Getting the name of the function to analyze and extract code features vector
def prepareCode(sourceFile,targetFile):
    global funcToAnalyze, blockToAnalyze

    funcToAnalyze            = "*"
    blockToAnalyze           = "*"


    f = open(sourceFile)
    fw = open(targetFile,"w")

    for line in f:
        m1 = re.match("#include .*",line)
        m2 = re.match("// FUNC_ANALYZ:\s+([^\s]*)\s*(.*)",line)
        m3 = re.match("//(.*)",line)

        if m2:
                funcToAnalyze  = m2.group(1)
                blockToAnalyze = m2.group(2)
                # print("%s %s" % (funcToAnalyze,blockToAnalyze))
        if not m1 and not m3:
                fw.write(line)
        # else:
        #         print(m1.group(0))
        
    f.close()
    fw.close()


def analyzeCode(filename):

    tmpFileName = "./sca_tmp.c"
    prepareCode(filename,tmpFileName)

    initVars(INIT_AST)
    # parsePragmas(tmpFileName)
    readPragmas(tmpFileName)
    readCompound(tmpFileName)
    show_func_defs(tmpFileName)

    featureVector = [maxForStmtDepth,anyFuncCall,anyArrayWriteShifted,numIrregularForLoops,usesGlobalVars,
                     anyIfStmt,(int)(not anyForLoopNonStaticLimit),anySIMDloop,anyLoopSchedule,numLoopInvVar,
                     numLoopHoistedVarMods,numNon1Darray,numAuxVarArrayIndex,totalNumForLoops,numNonNormalizedForLoops,
                     numStmtsRollUp,numCompoundStmtsInBlock,anyTernaryOp,anyUselessStmt]

    statsStr = generateStats()

    # print(featureVector)

    os.system("rm %s" % (tmpFileName))
    
    return [featureVector,statsStr]

def analyzeCodeFromStr(strCode):

    s2s_tmp_file = "s2s_code_tmp.c"

    text_file = open(s2s_tmp_file, "w")
    text_file.write(strCode)
    text_file.close()    

    analysisTuple = analyzeCode(s2s_tmp_file)
    
    print("#########################################")
    sys.stdout.write("%s\n" % generateStats())
    print("#########################################")

    os.system("rm %s" % (s2s_tmp_file))


if __name__ == "__main__":

    if len(sys.argv) > 1:
        filename  = sys.argv[1]
    else:
        # filename = 'sca_test_files/test_rollUp_8.c'
        filename = 'train_set/imageFilter/threshold/s2s_transformations/threshold0_001_002.c'
        # filename = 'sca_test_files/test15.c'

    analyzeCode(filename)
    # print stats
    print("#########################################")
    sys.stdout.write("%s\n" % generateStats())
    print("#########################################")



######################################################
#                TODO list                           
# - Do not consider calls to printf() as function calls
#   from the code, as they will be eliminated in the 
#   translation phase 
#
# - For a 1D index scheme in a 2D array like for example:
#     for(i=0;...)
#         for(j=0;...)
#             v[i*width+j] = ... w[i*width+j]
#
# it says numAuxVarArrayIndex = 0, but should it? Which
# criteria to say that "width" is not aux variable?


######################################################
