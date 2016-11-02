This directory contains the SCA tool in charge of obtaining code
abstractions and application use cases used for the Machine Learning
techniques. The content of the directory is the following:

- 00README.txt: this file

- static_analyzer.py: the program that actually computes program
  abstractions using pycparser

- trainer.py: a python program that automatically computes and updates
  (if requred) the abstraction of a code or set of codes. This program
  can process recursively all code files contained in a list of
  directories. The list of directories is, at the moment, hard coded
  in the python script. The header of each code has the form:

// FUNC_ANALYZ: NAME_OF_FUNC (function name within the code to analyse)
// FEAT_VECTOR: Hand written vector of features used for checking correctness 
                of the vector in TEST_VECTOR automatically computed
// TEST_VECTOR: Vector of features automatically computed by SCA tool
// TEST_LABEL:  Label stating the possible platform(s) associated to
                this code abstraction 

- sca_test_files: directory containing C codes for testing purposes
  and checking SCA tool is not broken when functionality is expanded

- train_set: directory containing C codes as a training set for Machine
  Learning techniques. Currently there are only use case applications
  within the cathegory of Image Processing (in directory
  "imageFilter"). For each use case the content of directory is:
  
  - 00README.txt:    A file describing transformation sequences
  - parallel:        Directory with parallel versions (currently only
                     GPU)
  - transformations: Directory with all codes described in file
                     00README.txt 
  - data:            Directory with input data for each use case. Also
                     output is written into this directory

- predict_set: directory containing C codes as the predict set for
  Machine Learning techinques

- utils: directory containing some utils to manipulate code for
  profiling purposes
