The C source to source transformation tool
==========================================

The machine learning module has been developed for tackling the two main problems faced by the source-to-source transformation engine developed within the POLCA project. The first problem consists of efficiently and smartly searching in a very large (even infinite) state space where non-functional properties of the codes, in the search space, present a non-monotonic behavior. The second problem consist of deciding when to stop the search process, since there is no a-priori general way to determine that an optimum (in the sense of the most efficient possible code) state has been reached. These problems have been tackled using classification trees to solve the latter and re- inforcement learning to solve the former.

The machine learning module has been contributed by the
[IMDEA SW Institute](http://software.imdea.org/es/) and the [Technical University of Madrid](http://www.upm.es/internacional).

Installation
------------

The Machine Learning Module requires the following python modules to work:
* Scikit-learn: This library implements several machine learning methods for dif- ferent purposes like data mining and data analysis. It is an open source project with an ample development community and rich documentation. Installation can be done as a standard Python installation through pip command. For more de- tails please visit:
http://scikit- learn.org/stable/install.html

* PyBrain: This is a Python modular Machine Learning Library. Its goal is to of- fer flexible, easy-to-use yet still powerful algorithms for Machine Learning tasks and a variety of predefined environments to test and compare your algorithms. The main functionality used from PyBrain is the implementation of Reinforce- ment Learning algorithms and learning models. In order to install it please follow the instructions in the following link:
                  http://pybrain.org/pages/download
* Pycparser: is a parser for the C language, written in pure Python. It is a mod- ule designed to be easily integrated into applications that need to parse C source code. The Machine Learning Module uses Pycparser to obtain the code abstrac- tions as a representation to apply machine learning methods. Installation can be done as a standard Python installation through pip command. For more details please visit:
https://github.com/eliben/pycparser#installation-process


Getting Started
---------------



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
