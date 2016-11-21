The machine learning module
===========================

The machine learning module has been developed for tackling the two main problems faced by the source-to-source transformation engine developed within the POLCA project. The first problem consists of efficiently and smartly searching in a very large (even infinite) state space where non-functional properties of the codes, in the search space, present a non-monotonic behavior. The second problem consist of deciding when to stop the search process, since there is no a-priori general way to determine that an optimum (in the sense of the most efficient possible code) state has been reached. These problems have been tackled using classification trees to solve the latter and reinforcement learning to solve the former.

The machine learning module has been contributed by the
[IMDEA SW Institute](http://software.imdea.org/es/) and the [Technical University of Madrid](http://www.upm.es/internacional).

Installation
------------

The Machine Learning Module requires the following python modules to work:

* SciPy: this library is one of the core packages that make up the SciPy stack. It provides many user-friendly and efficient numerical routines such as routines for numerical integration and optimization. It is required by Scikit-learn and PyBrain. Installation can be done as a standard Python installation through pip command. For more details please visit:

https://www.scipy.org/install.html

* Scikit-learn: This library implements several machine learning methods for different purposes like data mining and data analysis. It is an open source project with an ample development community and rich documentation. Installation can be done as a standard Python installation through pip command. For more details please visit:
http://scikit-learn.org/stable/install.html

* PyBrain: This is a Python modular Machine Learning Library. Its goal is to offer flexible, easy-to-use yet still powerful algorithms for Machine Learning tasks and a variety of predefined environments to test and compare your algorithms. The main functionality used from PyBrain is the implementation of Reinforcement Learning algorithms and learning models. In order to install it please follow the instructions in the following link:
                  http://pybrain.org/pages/download
* Pycparser: is a parser for the C language, written in pure Python. It is a module designed to be easily integrated into applications that need to parse C source code. The Machine Learning Module uses Pycparser to obtain the code abstractions as a representation to apply machine learning methods. Installation can be done as a standard Python installation through pip command. For more details please visit:
https://github.com/eliben/pycparser#installation-process


Developer information
---------------------

### Reinforcement Learning

The functionality to use Reinforcement Learning as a guide for the source-to-source transformation tool has been implemented in a modular way using different files and extending the PyBrain classes when needed.
* main.py: This files is the main interface with the s2s engine. During the learning phase it calls the reinforcement learning classes to fill in the action-value table. During the predict phase it queries the action-value table in order to guide the transformation process.
* POLCAenv.py: This class implements functionality required to store and manipulate the reinforcement learning environment. Thus, it stores the current state, the transition matrix used during the learning phase and defines methods to perform actions and to reset the environment for starting a new iteration during the learning phase.
* POLCAmdp.py: This class provides the functionality to model the reinforcement learning environment as a fully observable MDP problem. It interacts with the previous class (i.e. S2Senv.py) providing observation and rewards obtained as a result of performing actions.

### Classification tree

The functionality to learn final states for the source-to-source engine, has been implemented in a single file. During the learning phase the program basically reads an input file with pairs (Abstraction, Label) to train the classification method calling the Scikit-learn API. Later during the prediction phase the classification method is queried to decide if a given abstraction is a final state for the transformation engine or not.
The program implementing the classification method also allows to dump the learned classification tree to a file. In this way the process of building a fitting the tree is avoided if the file containing the training (Abstraction, Label) pairs has not changed.

### Code Abstraction
The functionality to obtain the abstraction associated to a given code has been implemented in a single file. This file calls the Pycparser API in order to parse the C code and obtain the abstractions. The parsing has been implemented in the ASTVisitor class that extends the Pycparser class c ast.NodeVisitor.
There are two methods that can be called to obtain the abstraction of a given code:
* analyzeCode(filename): This function computes the code abstraction by reading first the content of the file located in the string argument filename. The argument must be a valid path in the machine, either relative of absolute. This function is left for debug purposes.
* analyzeCodeFromStr(strCode): This function computes the code abstraction by directly parsing the code stored in the string argument. This is the function called by the Machine Learning methods when used to guide the transformation engine.


Error Reports
-------------

If you think you found a genuine bug please report it together
with the following information:

  - a short test file showing the behavior with all unnecessary
    code removed.

  - a transcript (log file) of the session that shows the error.

Please note that it is important to make the file as small as possible
to allow us to find and fix the error soon.

Original author Salvador Tamarit
Correspondence e-mail: <polca-project-madrid@software.imdea.org>

Please send error reports for contributed files to the original authors.
