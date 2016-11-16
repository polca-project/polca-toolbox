#!/usr/bin/python

# Copyright (c) 2013-2016, The IMDEA Software Institute and
# Copyright (c) 2013-2016, Universidad Politecnica de Madrid

# See LICENSE.txt and AUTHORS.txt for licensing and authorship


import json,sys,os
import static_analyzer as sca
import warnings
warnings.filterwarnings("ignore")

sys.path.extend(['../machine_learning/reinforcement_learning'])
import oracle


rule2IdchangeDict = {}

def prepareCode(codeStr):

    # modeCode = ""
    # if "{" in codeStr:
    #     modCode = "// FUNC_ANALYZ: main BLOCK_ABS\nvoid main()\n%s" % codeStr
    # else:
    #     modCode = "// FUNC_ANALYZ: main BLOCK_ABS\nvoid main()\n{\n%s;\n}" % codeStr

    modCode = "// FUNC_ANALYZ: main BLOCK_ABS\n\n%s" % codeStr

    return modCode

def convertAbsList2Key(abstraction):

    return "".join(str(x) for x in abstraction)

if __name__ == "__main__":

    if len(sys.argv) > 1:
        # jsonStr  = sys.argv[1]
        # codeChanges = json.loads(jsonStr)
        # sys.stderr.write("\n\n########"+sys.argv[1] + "\n\n")
        # exit(0)
        codeChanges = json.loads(open(sys.argv[1]).read())
    else:
        codeChanges = json.loads(open('output_s2s.json').read())
        # print len(codeChanges['changes'])

# oldCode = prepareCode(codeChanges["codeBlock"])
oldCode = prepareCode(codeChanges["code"])
# sys.stderr.write(str(oldCode)+"\n")

abstraction = sca.analyzeCodeFromStr(oldCode)

# print oldCode
# sys.stderr.write("#####################################################"+"\n")
# sys.stderr.write(str(abstraction[0])+"\n")


ruleList = []
rule2IdchangeDict = {}
for i in range(len(codeChanges['changes'])):
    # Convert from unicode  to regular string
    ruleName = str(codeChanges['changes'][i]['ruleName'])
    # idChange = int(codeChanges['changes'][i]["idChange"])
    ruleList.append(ruleName)


nextStateAbs,ruleName = oracle.predict(abstraction[0],ruleList)

for i in range(len(codeChanges['changes'])):
    sys.stderr.write(ruleName + " - " + str(codeChanges['changes'][i]['ruleName']) + "\n")
    if ruleName == str(codeChanges['changes'][i]['ruleName']):
        newCode = prepareCode(codeChanges['changes'][i]['newCodeAll'])

        # sys.stderr.write(str(codeChanges['changes'][i]["idChange"])+"\n")
        # sys.stderr.write(str(codeChanges['changes'][i]['ruleName'])+"\n")
#        # sys.stderr.write(str(codeChanges['changes'][i]['newCode'])+"\n")
#         sys.stderr.write(str(newCode)+"\n")

        newCodeAbs = sca.analyzeCodeFromStr(newCode)

        sys.stderr.write(convertAbsList2Key(newCodeAbs[0]) + "\n")
        sys.stderr.write(nextStateAbs + "\n")
        sys.stderr.write("#####################################################\n")
        if convertAbsList2Key(newCodeAbs[0]) == nextStateAbs:
            rule2IdchangeDict[ruleName] = int(codeChanges['changes'][i]["idChange"])
            break



if ruleName == "":
    print(-1)
else:
    print(rule2IdchangeDict[ruleName])
