#!/usr/bin/python

import json,sys
import static_analyzer as sca

if __name__ == "__main__":

    if len(sys.argv) > 1:
        jsonStr  = sys.argv[1]
        # print jsonStr
        codeChanges = json.loads(jsonStr)
        print len(codeChanges['changes'])
    else:
        codeChanges = json.loads(open('output_s2s.json').read())
        print len(codeChanges['changes'])



for i in range(len(codeChanges['changes'])-1):
    print codeChanges['changes'][i]["idChange"]
    print codeChanges['changes'][i]["line"]
    print codeChanges['changes'][i]['ruleName']
    print "#####################################################"
    # print codeChanges['changes'][i]['oldCode']
    # print "#####################################################"
    # print codeChanges['changes'][i]['newCode']

    if "{" in codeChanges['changes'][i]['newCode']:
        modCode = "void main()%s" % codeChanges['changes'][i]['newCode']
    else:
        modCode = "void main(){\n%s;\n}" % codeChanges['changes'][i]['newCode']

    print modCode
    sca.analyzeCodeFromStr(modCode)

# sca.analyzeCodeFromStr("")
