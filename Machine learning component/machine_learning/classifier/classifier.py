#!/usr/bin/python

from sklearn import tree
from sklearn.externals.six import StringIO  
import pydot, sys 


labelDict = {}
labelDict[-1] = "NONE"
labelDict[0]  = "CPU"
labelDict[1]  = "OpenMP"
labelDict[2]  = "MPI"
labelDict[3]  = "OpenMP/MPI"
labelDict[4]  = "GPU"
labelDict[5]  = "GPU/OpenMP"
labelDict[6]  = "GPU/MPI"
labelDict[7]  = "GPU/OpenMP/MPI"
labelDict[8]  = "FPGA"
labelDict[9]  = "FPGA/GPU"
labelDict[10] = "FPGA/OpenMP"
labelDict[11] = "FPGA/MPI"
labelDict[12] = "FPGA/GPU/OpenMP"
labelDict[13] = "FPGA/GPU/MPI"
labelDict[14] = "FPGA/OpenMP/MPI"
labelDict[15] = "FPGA/GPU/OpenMP/MPI"

def train():
    # Feature vector for each sample:
    # 0: max nested loop depth (i.e. 0: one for loop, 1: two for loops and one nested, ...)
    # 1: any function call? (0: no, 1: yes)
    # 2: any array write shifted within for loops? (0: no, 1: yes)
    # 3: irregular loops (i.e. containing break, continue, ...)
    # 4: any global variable (0: no, 1: yes)
    # 5: any if statement? (0: no, 1: yes)
    # 6: foor loop limits static for all loops? (0: no, 1: yes)

    # Samples features
    X = [
        [0, 1, 0, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_1.c
        [0, 1, 0, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_2.c
        [0, 1, 0, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_3.c
        [0, 1, 0, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_4.c
        [0, 1, 0, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_5.c
        [0, 1, 0, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_6.c
        [0, 1, 0, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_7.c
        [0, 1, 0, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_8.c
        [1, 0, 1, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_9.c
        [0, 0, 0, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_10.c
        [0, 0, 0, 0, 0, 0, 1],  # sample vector corresponding to code rgb_filter_9_11.c
        [0, 0, 0, 0, 1, 0, 1],  # extra sample to learn that there cannot be global variables
        [0, 0, 0, 1, 0, 0, 1],  # extra sample to learn that there cannot be irregular loops
        [0, 0, 1, 0, 0, 0, 1]   # extra sample to learn to avoid shifted array writes
    ] 


    # Labels 
    # 0:  CPU
    # 1:  OpenMP
    # 2:  MPI
    # 3:  OpenMP/MPI
    # 4:  GPU
    # 5:  GPU/OpenMP
    # 6:  GPU/MPI
    # 7:  GPU/OpenMP/MPI
    # 8:  FPGA
    # 9:  FPGA/GPU
    # 10: FPGA/OpenMP
    # 11: FPGA/MPI
    # 12: FPGA/GPU/OpenMP
    # 13: FPGA/GPU/MPI
    # 14: FPGA/OpenMP/MPI
    # 15: FPGA/GPU/OpenMP/MPI

    # Samples labels
    Y = [0,       # label corresponding to code rgb_filter_9_1.c
         0,       # label corresponding to code rgb_filter_9_2.c
         0,       # label corresponding to code rgb_filter_9_3.c
         0,       # label corresponding to code rgb_filter_9_4.c
         0,       # label corresponding to code rgb_filter_9_5.c
         0,       # label corresponding to code rgb_filter_9_6.c
         0,       # label corresponding to code rgb_filter_9_7.c
         0,       # label corresponding to code rgb_filter_9_8.c
         0,       # label corresponding to code rgb_filter_9_9.c
         1,       # label corresponding to code rgb_filter_9_10.c
         1,       # label corresponding to code rgb_filter_9_11.c
         0,
         0,
         0
    ]       

    clf = tree.DecisionTreeClassifier()
    clf = clf.fit(X, Y)
    print clf

    return clf

def trainWithInput(X,Y):
    clf = tree.DecisionTreeClassifier()
    clf = clf.fit(X, Y)
    # print clf

    return clf



def classify(clf,featureVector):
    pred = clf.predict([featureVector])
    sys.stdout.write("\t%s -> %s\n\n" % (str(pred),labelDict[pred[0]]))
    # print pred


def dumpTree(clf):
    dot_data = StringIO() 
    tree.export_graphviz(clf, out_file=dot_data) 
    graph = pydot.graph_from_dot_data(dot_data.getvalue()) 
    graph.write_pdf("tree.pdf")

##########################

if __name__ == "__main__":

    train()
