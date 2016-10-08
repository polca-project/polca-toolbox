#!/usr/bin/python

import sys
import os.path
from subprocess import call
import fileinput

########################
###      STATUS      ###
########################
dictPragma = {}
scopes = []
cLinesASM = []
scopesNamesFuncLines = []
cLinesASMCode = []
########################


class Scope:
    lstart = 0
    lend   = 0
    name   = ''
    work   = 0
    def __init__(self):
        pass
    def toStr(self):
        nstr = "*"
        if(len(self.name)>0):
            nstr = self.name
        return str(self.lstart) + "\t\t" + str(self.lend) + "\t" + str(self.work) + "\t\t" + nstr


def isFourHex(s):
    if(len(s)==4):
        r = True
        for c in s:
            if (c >= '0' and c <= '9') or (c >= 'A' and c <='F') or (c >= 'a' and c <='f'):
                pass
            else:
                r = False
        return r
    else:
        return False


def findScope(line):
    global scopes
    possibles = []
    for i in range(len(scopes)):
        if(line >= scopes[i].lstart and line <= scopes[i].lend):
            possibles.append(i)
    if(len(possibles) == 0):
        return -1
    selected = possibles[0]
    size = scopes[selected].lend - scopes[selected].lstart
    for i in range(1, len(possibles)):
        _i = possibles[i]
        if scopes[_i].lend - scopes[_i].lstart < size:
            selected = _i
            size = scopes[_i].lend - scopes[_i].lstart
    return selected


def matchScopesAndName(n):
    global scopes
    sel = -1
    diff = -1
    for i in range(len(scopes)):
        if scopes[i].lstart > n:
            if sel < 0:
                sel = i
                diff = scopes[i].lstart - n
            elif scopes[i].lstart - n < diff:
                sel = i
                diff = scopes[i].lstart - n
    return sel
    
    
def matchScopesAndNames():
    global scopes
    global scopesNamesFuncLines
    global dictPragma

    sel = -1    
    for i in range(len(scopesNamesFuncLines)):
        line = scopesNamesFuncLines[i][0]
        scope = matchScopesAndName(line)
        scopes[scope].name = scopesNamesFuncLines[i][1]
      
    for k in dictPragma:
        line = dictPragma[k]
        scope = matchScopesAndName(line)
        scopes[scope].name = k
        

def matchScopesAndASM():
    global cLinesASM
    global scopes
    
    for i in range(len(cLinesASM)):
        if cLinesASM[i] > 0:
            s = findScope(i)
            if(s >= 0):
                scopes[s].work = scopes[s].work + cLinesASM[i]
            

def processASMCount(lines, fname):
    global cLinesASM
    global cLinesASMCode
    global scopesNamesFunc
    global dictPragma
    count = 0
    cline = 0
    startScope = ''
    funcStartScope = ''
    for line in lines:
        l = line.rstrip().lstrip().split()
        if(len(l)>1):
            r = l[0].find(':'+fname)
            if(r >= 0):

                if (len(l) > 2 and len(funcStartScope) > 0):
                    for s in l[2:]:
                        ss = s.split('(')
                        for sss in ss:
                            if (sss == funcStartScope):
                                startScope = funcStartScope
                                
                cLinesASM[cline] = cLinesASM[cline] + count
                cline = int(l[0][:r])
                if(len(cLinesASMCode[cline]) == 0):
                    cLinesASMCode[cline] = ' '.join(l[2:])
                count = 0
                if len(startScope) > 0:
                    scopesNamesFuncLines.append((cline, startScope))
                    startScope = ''
                    
                if (len(l) > 5):
                    if(l[2] == '#pragma' and l[3] == 'polca' and l[4] == 'def'):
                        polcaScopeName = l[5]
                        if(not dictPragma.has_key(polcaScopeName)):
                            dictPragma[polcaScopeName] = cline
                
               
            elif (l[1] == '.globl'):
                if(len(l) == 3):
                    funcStartScope = l[2]
                    
            else:
                if(len(l) > 3):
                    if(isFourHex(l[1])):
                        count = count + 1

    
def processScopes(lines):
    i = 1 #line counter
    stack = []
    global scopes
    for line in lines:
        closing = 0;
        for c in line:
            if c == '{':
                stack.append(i)
            elif c == '}':
                closing = stack.pop()
            else:
                pass
        if(closing > 0):
            s = Scope()
            s.lstart = closing
            s.lend   = i
            scopes.append(s)
        else:
            pass
        i = i+1 #next line
        
        
if __name__ == "__main__":
    if(len(sys.argv) != 2):
        print 'Usage: ' + sys.argv[0] + ' file_to_process.c'
        sys.exit(1)
    fname = sys.argv[1]
    if(not os.path.isfile(fname)):
        print 'File: ' + fname + ' does not exist'
        sys.exit(2)

    f = open(fname, 'r')
    lines = f.readlines()
    f.close()

    numLines = len(lines)
    processScopes(lines)
    
    fout = fname + '_asm'
    f = open(fout, 'w')
    gresult = call(['gcc', '-w', '-c', '-g', '-O2', '-march=native', '-Wa,-a,-ad', fname], stdout=f)
    f.close()
    if(gresult != 0):
        print 'Error executing GCC'
        sys.exit(3)        
    f = open(fout, 'r')
    lines = f.readlines()
    f.close()
    
    #init C Lines Assembler Count array
    for i in range(numLines+2):
        cLinesASM.append(0)
        cLinesASMCode.append("")
    
    processASMCount(lines, fname)
    matchScopesAndASM()
    matchScopesAndNames()

    print "StartLine\tEndLine\tASM Ins\tName"
    for i in range(len(scopes)):
        print scopes[i].toStr()
    
    print "\nLine\tASM Ins\tCode"
    for i in range(len(cLinesASM)):
        print str(i) + "\t" + str(cLinesASM[i]) + "\t" + cLinesASMCode[i]
        
