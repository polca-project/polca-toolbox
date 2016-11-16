#!/usr/bin/python

# Copyright (c) 2013-2016, The IMDEA Software Institute and
# Copyright (c) 2013-2016, Universidad Politecnica de Madrid

# See LICENSE.txt and AUTHORS.txt for licensing and authorship


import sys,re,os
from sklearn import tree
from sklearn.externals import joblib

# This is not required if you've installed pycparser into
# your site-packages/ with setup.py
sys.path.extend(['../machine_learning/classifier'])

import classifier as clf
import static_analyzer as sca

def checkTestFiles(testsPath):

    for dirpath, dirnames, filenames in os.walk(testsPath):
        for file in filenames:
            # use only files *.c and avoid *.c~ (emacs) and .#*.c (emacs)
            if re.match("^[^.].*\.c$",file):
            # if re.match("test22\.c$",file):
            #     print("%s/%s" % (dirpath,file))
                filename = "%s/%s" % (dirpath,file)
                sys.stdout.write("Checking file %s ..." % (filename))
                numFound = 0
                listID     = []
                listVector = []

                f = open(filename)

                for line in f:
                    m1 = re.match("// (TEST_VECTOR): \[(.*)\]",line)
                    m2 = re.match("// (FEAT_VECTOR): \[(.*)\]",line)
                    if m1:
                            listID.append(m1.group(1))
                            listVector.append([int(x) for x in m1.group(2).split(",")])
                            numFound += 1
                            if numFound==2:
                                break

                    elif m2:
                            listID.append(m2.group(1))
                            listVector.append([int(x) for x in m2.group(2).split(",")])
                            numFound += 1
                            if numFound==2:
                                break

                f.close()

                if numFound == 2:
                    error = 0
                    for i in range(0,len(listVector[0])):
                        if not listVector[0][i] == listVector[1][i]:
                            sys.stdout.write("\n")
                            print("ERROR: in file %s" % (filename))
                            print("\t%s: %s" % (listID[0],listVector[0]))
                            print("\t%s: %s" % (listID[1],listVector[1]))
                            error = 1
                            break
                    if not error:
                            sys.stdout.write(" OK!\n")
                else:
                    sys.stdout.write("\n")
                    print("ERROR: file %s doesn't have 2 vectors to compare" % (filename))
                    print("\t%s: %s" % (listID[0],listVector[0]))

def updateTestFileHeader(analysisTuple,filename,featureKeepRange):
    filenameAux = filename+"_aux"

    # analysisTuple = sca.analyzeCode(filename)
    featureVector = analysisTuple[0]
    featureStr    = analysisTuple[1]
    print(featureVector)
    f = open(filename)

    headerLabel = 0
    # search previous label to reuse
    for line in f:
        m1 = re.match("// TEST_VECTOR: \[(.*)\]",line)
        m2 = re.match("// TEST_LABEL: (.*)\s+\(.*\)",line)
        m3 = re.match("//#(.*)",line)
        if m1 or m3:
            continue
        if m2:
            headerLabel = int(m2.group(1))
            break

    f.close()

    f = open(filename)
    fw = open(filenameAux,"w")
    # content = f.readlines()

    # fw.write("// TEST_VECTOR: ")
    # fw.write(str(featureVector))
    # fw.write("\n// TEST_LABEL: %d (%s)\n" % (headerLabel,sca.labelDict[headerLabel]))

    listVector = []
    manualVectorPresent = 0
    featureVectorUpdated = 0
    codeNotReached = 1
    for line in f:
        m1 = re.match("// TEST_VECTOR: \[(.*)\]",line)
        m2 = re.match("// TEST_LABEL: (.*)\s+\(.*\)",line)
        m3 = re.match("//#(.*)",line)
        m4 = re.match("\\n(.*)",line)
        m5 = re.match("// FEAT_VECTOR: \[(.*)\]",line)
        m6 = re.match("// FUNC_ANALYZ:\s+([^\s]*)\s*(.*)",line)
        m7 = re.match("// RULE_APPLD:\s+(.*)",line)

        if m6 or m7:
            fw.write(line)
        # 'FEAT_VECTOR' must be written at the beginning of file
        elif m5:
            manualVectorPresent = 1

            listVector = [int(x) for x in m5.group(1).split(",")]
            # we consider 'FEAT_VECTOR' in the file to be obsolete if the length is not
            # the same as the vector computed by SCA abstraction tool
            # if 'FEAT_VECTOR' present but obsolete, then only update the new features
            # indicated by 'featureKeepRange'
            if not len(featureVector) == len(listVector) or (len(listVector) != (featureKeepRange[1]+1)):
                handVector = []
                for i in range(len(featureVector)):
                    if i >= featureKeepRange[0] and i <= featureKeepRange[1]:
                        handVector.append(listVector[i])
                    else:
                        handVector.append(featureVector[i])
                # print("#### %s" % handVector)
                fw.write("// FEAT_VECTOR: ")
                fw.write("%s\n" % str(handVector))

            else: # 'FEAT_VECTOR' present and not obsolete -> write it as it is
                fw.write(line)
        elif m1:
            fw.write("// TEST_VECTOR: ")
            fw.write("%s\n" % str(featureVector))
            featureVectorUpdated = 1
        elif m2:
            if not featureVectorUpdated:
                fw.write("// TEST_VECTOR: ")
                fw.write("%s\n" % str(featureVector))
                featureVectorUpdated = 1
            fw.write("// TEST_LABEL: %d (%s)\n\n" % (headerLabel,clf.labelDict[headerLabel]))
            # we write \n at the end because we have removed all \n with
            # matching regex m4
            fw.write("%s\n\n\n" % featureStr)
        elif m3:
            continue
        elif m4 and codeNotReached:
            continue
        else:

            if not manualVectorPresent:
                fw.write("// FEAT_VECTOR: ")
                fw.write("%s\n" % str(featureVector))
                manualVectorPresent = 1

            if not featureVectorUpdated:
                fw.write("// TEST_VECTOR: ")
                fw.write("%s\n" % str(featureVector))
                featureVectorUpdated = 1
                fw.write("// TEST_LABEL: %d (%s)\n\n" % (headerLabel,clf.labelDict[headerLabel]))
                # we write \n at the end because we have removed all \n with
                # matching regex m4
                fw.write("%s\n\n\n" % featureStr)
                featureVectorUpdated = 1

            # print(line)
            codeNotReached = 0
            fw.write(line)

        # if not (m1 or m2):
        #     fw.write(line)

    fw.close()
    f.close()
    
    os.system("mv %s %s" % (filenameAux,filename))
    

def readTestFileHeader(filename):
    f = open(filename)
    content = f.readlines()
    headerCorrect = 0
    headerVector = []
    headerLabel = -1
    # print content
    for line in content:
        m1 = re.match("// TEST_VECTOR: \[(.*)\]",line)
        m2 = re.match("// TEST_LABEL: (.*)\s+\(.*\)",line)
        if m1:
            vectorStr = m1.group(1)
            headerVector = [int(num) for num in vectorStr.split(",")]
            # print vec
        if m2:
            headerLabel = int(m2.group(1))
            # print m2.group(1)
            headerCorrect = 1

    f.close()
    if not headerCorrect:
        print("ERROR: test file %s has incorrect header" % filename)
        # exit(0)

    return [headerVector,headerLabel]

def update_feature_vector(analysisTuple,filename,featureKeepRange):
    # testsPath = "./c_files"

    # for dirpath, dirnames, filenames in os.walk(testsPath):
    #     for file in filenames:
    #         # use only files *.c and avoid *.c~
    #         if re.match(".*\.c$",file):
    #             # print("%s/%s" % (testsPath,file))
    #             filename = "%s/%s" % (dirpath,file)
    #             print("Updating %s ..." % (filename))
    #             updateTestFileHeader(filename)

    print("Updating %s ..." % (filename))
    updateTestFileHeader(analysisTuple,filename,featureKeepRange)

def check_SCA(testsPath,featureKeepRange):
    # testsPath = "./c_files"

    featureMatrix = []
    labelVector   = []

    for dirpath, dirnames, filenames in os.walk(testsPath):
        for file in filenames:
            # use only files *.c and avoid *.c~ (emacs) and .#*.c (emacs)
            if re.match("^[^.].*\.c$",file):
            # if re.match("test22\.c$",file):
            #     print("%s/%s" % (dirpath,file))
                filename = "%s/%s" % (dirpath,file)


                analysisTuple = sca.analyzeCode(filename)
                featureVector = analysisTuple[0]
                update_feature_vector(analysisTuple,filename,featureKeepRange)

                testHeader = readTestFileHeader(filename)
                # sys.stdout.write("test %.2d: " % test)
                sys.stdout.write("test %s: \n\t" % file)
                print(" %s" % str(featureVector))
                sys.stdout.write("\t")
                print(testHeader)
                featureMatrix.append(featureVector)
                labelVector.append(testHeader[1])


def train(testsPath):

    dumpFileName = 'code_classifier.pkl'

    print("###################################################")
    print("                  Begin training                   ")

    if(not os.path.exists("%s/%s" % (".",dumpFileName))):
        
        print(" File %s/%s doesn't exist. Start training..." % (".",dumpFileName))

        # testsPath = "./train_set"
        numTests = 7

        featureMatrix = []
        labelVector   = []
        # for sample in range(1,numTests+1):
        #     filename = "%s/file%d.c" % (testsPath,sample)
        #     testHeader = readTestFileHeader(filename)
        #     # featureVector = sca.analyzeCode(filename)
        #     sys.stdout.write("sample %.2d: \n\t" % sample)
        #     # print(featureVector)
        #     # featureMatrix.append(featureVector)
        #     # labelVector.append(testHeader[1])
        #     sys.stdout.write("\t")
        #     print(testHeader)
        #     featureMatrix.append(testHeader[0])
        #     labelVector.append(testHeader[1])

        for dirpath, dirnames, filenames in os.walk(testsPath):
            for file in filenames:
                # use only files *.c and avoid *.c~ (emacs) and .#*.c (emacs)
                if re.match("^[^.].*\.c$",file):
                # if re.match("test22\.c$",file):
                #     print("%s/%s" % (dirpath,file))
                    filename = "%s/%s" % (dirpath,file)
                    #
                    #
                    # update_feature_vector(filename)

                    testHeader = readTestFileHeader(filename)
                    # analysisTuple = sca.analyzeCode(filename)
                    # featureVector = analysisTuple[0]
                    # sys.stdout.write("test %.2d: " % test)
                    sys.stdout.write("sample %s: \n\t" % file)
                    # print(" %s" % str(featureVector))
                    sys.stdout.write("\t")
                    print(testHeader)
                    featureMatrix.append(testHeader[0])
                    labelVector.append(testHeader[1])

        tree = clf.trainWithInput(featureMatrix,labelVector)
        clf.dumpTree(tree)
        joblib.dump(tree, dumpFileName)
    else:
        print(" File %s/%s exists. Loading..." % (".",dumpFileName))
        tree = joblib.load(dumpFileName) 

    print("                        Done!                      ")
    print("###################################################")

    return tree

def predict(path):

    for dirpath, dirnames, filenames in os.walk(path):
        for inputFile in filenames:
            # avoid unwanted files. Match just files: "filename.c"
            m = re.match("[^#]*\.c$",inputFile)
            if m:
                # print("%s/%s" % (dirpath,inputFile))

                filename = "%s/%s" % (dirpath,inputFile)
                testHeader = readTestFileHeader(filename)
                # featureVector = sca.analyzeCode(filename)
                sys.stdout.write("file: %s/%s \n" % (dirpath,inputFile))
                # print(featureVector)
                # featureMatrix.append(featureVector)
                # labelVector.append(testHeader[1])
                sys.stdout.write("\t")
                print(testHeader)
        
                clf.classify(tree,testHeader[0])


if __name__ == "__main__":

    # if len(sys.argv) > 1:
    #     filename  = sys.argv[1]
    # else:
    #     filename = 'examples/c_files/memmgr.c'


    # pathList = ["./c_files","./predict_set"]
    # pathList = ["./c_files"]

    # Learning examples
    # pathList = ["./train_set/imageFilter/convolucion"]
    # pathList = ["./train_set/imageFilter/compress/test"]
    # pathList = ["./train_set/imageFilter/threshold"]
    # pathList = ["./train_set/imageFilter/rgbFilter/test"]
    # pathList = ["./train_set/imageFilter/edge_detect/transformations"]

    # Predict examples
    # pathList = ["./predict_set/imageFilter/image_correlation"]
    # pathList = ["./predict_set/imageFilter/convolucion/kernel"]
    # pathList = ["./predict_set/imageFilter/brightness/transformations","./train_set/imageFilter/threshold/transformations"]
    # pathList = ["./train_set/imageFilter/threshold/transformations"]
    # pathList = ["./train_set/imageFilter/histogram/transformations"]

    # pathList = ["./predict_set/geometry/rotate"]

    # pathList = ["./sca_test_files"]
    # pathList = ["./problematic_codes"]
    # pathList = ["./train_set/imageFilter/threshold/oracle_test","./predict_set/imageFilter/brightness/oracle_test"]
    # pathList = ["./train_set/hpcDwarfs/nBody/oracle_test"]
    pathList = ["./train_set/hpcDwarfs/nBody/s2s_transformations/2arrays"]

    print("\n#####################################################\n")

    # Uncomment this code if you want to update files TEST_VECTOR header
    # according to current SCA feature extraction method.
    # If FEAT_VECTOR is not present as a header, it will be updated
    # with the same feature vector as TEST_VECTOR
    # If FEAT_VECTOR is obsolete (i.e. len(FEAT_VECTOR) != len(TEST_VECTOR))
    # then FEAT_VECTOR is updated keeping the previous values specified
    # with the range in 'featureKeepRange'

    featureKeepRange = [0,18]

    for path in pathList:
        check_SCA(path,featureKeepRange)

    # print("\n#####################################################\n")

    # Uncomment this code if you want to check that files SCA headers
    # are compliant to current SCA feature extraction method. Check
    # is performed by comparing TEST_VECTOR with FEAT_VECTOR

    for path in pathList:
        checkTestFiles(path)

    print("\n#####################################################\n")

    #
    # for path in pathList:
    #     tree = train(path)
    #
    # testsPath = "./predict_set"
    # predict(testsPath)

