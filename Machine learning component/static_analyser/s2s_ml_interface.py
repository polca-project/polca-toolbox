#!/usr/bin/python

# Copyright (c) 2013-2016, The IMDEA Software Institute and
# Copyright (c) 2013-2016, Universidad Politécnica de Madrid

# See LICENSE.txt and AUTHORS.txt for licensing and authorship


import json,sys,os
import static_analyzer as sca
import warnings
warnings.filterwarnings("ignore")

sys.path.extend(['../machine_learning/reinforcement_learning'])
import oracle


rule2IdchangeDict = {}

def prepareCode(codeStr):

    modeCode = ""
    if "{" in codeStr:
        modCode = "void main()%s" % codeStr
    else:
        modCode = "void main(){\n%s;\n}" % codeStr

    return modCode

if __name__ == "__main__":

    if len(sys.argv) > 1:
        jsonStr  = sys.argv[1]
        # print jsonStr
        codeChanges = json.loads(jsonStr)
        # print len(codeChanges['changes'])
    else:
        codeChanges = json.loads(open('output_s2s.json').read())
        # print len(codeChanges['changes'])

oldCode = prepareCode(codeChanges["codeBlock"])
abstraction = sca.analyzeCodeFromStr(oldCode)

# print oldCode
# sys.stderr.write("#####################################################"+"\n")
# sys.stderr.write(str(abstraction[0])+"\n")


ruleList = []
rule2IdchangeDict = {}
for i in range(len(codeChanges['changes'])):
    # sys.stderr.write(str(codeChanges['changes'][i]["idChange"])+"\n")
    # # print codeChanges['changes'][i]["line"]
    # sys.stderr.write(str(codeChanges['changes'][i]['ruleName'])+"\n")
    # # print codeChanges['changes'][i]['oldCode']
    # sys.stderr.write(str(codeChanges['changes'][i]['newCode'])+"\n")
    # sys.stderr.write("#####################################################"+"\n\n")

    # Convert from unicode  to regular string
    ruleName = str(codeChanges['changes'][i]['ruleName'])
    idChange = int(codeChanges['changes'][i]["idChange"])
    ruleList.append(ruleName)
    rule2IdchangeDict[ruleName] = idChange

    # if codeChanges['changes'][i]['ruleName'] == 'inlining':
    #
    #     modCode = prepareCode(codeChanges['changes'][i]['newCode'])
    #
    #     # modCode = "void main(){\n%s;\n}" % codeChanges['changes'][i]['newCode']
    #     # modCode = codeChanges['changes'][i]['newCode']
    #
    #     print modCode
    #     abstraction = sca.analyzeCodeFromStr(modCode)
    #     print "###########################################"
    #     print abstraction[0]

# print ruleList

# # cmd = "../machine_learning/reinforcement_learning/oracle.py"
# cmd = "../machine_learning/reinforcement_learning/oracle.py  '%s' '%s'" % (abstraction[0],ruleList)
# # print(cmd)
# ruleName = os.system(cmd)


# oracle.predict(None,None)
# print "######################################"
# sys.stderr.write(str(ruleList)+"\n")
ruleName = oracle.predict(abstraction[0],ruleList)
# sys.stderr.write(str(ruleName)+"\n")
# sys.stderr.write(str(rule2IdchangeDict[ruleName])+"\n")
# sys.stderr.write(str(rule2IdchangeDict)+"\n")
# print ruleName
# if ruleName == "":
#     print("Final state reached!!!")
#     sys.exit(-1)
# else:
#     sys.exit(rule2IdchangeDict[ruleName])

if ruleName == "":
    print(-1)
else:
    print(rule2IdchangeDict[ruleName])
