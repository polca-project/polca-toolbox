#!/bin/bash

echo -e '#####################################################'
echo This installer has been checked for the following OS:
echo -e '\t- MacOs'
echo -e '\t- Linux Ubutu'

echo -e '\nIf you are under linux, check you have installed the following packages'
echo -e '\t- python-pip'
echo -e '\t- python-dev'
echo -e '\t- libblas-dev'
echo -e '\t- liblapack-dev'
echo -e '\t- libatlas-base-dev'
echo -e '\t- gfortran'

echo -e '\nOtherwise, you can install them with the following commands'
echo -e '\tsudo apt-get install python-pip python-dev'
echo -e '\tsudo apt-get install libblas-dev liblapack-dev libatlas-base-dev gfortran'
echo -e '#####################################################'

echo -e '\ninstalling required libraries for Machine Learning Module ...\n'

echo installing pycparser ...
pip install --user pycparser &> /dev/null
echo Done!
echo installing numpy and scipy ...
pip install --user numpy scipy &> /dev/null
echo Done!

echo installing scikit-learn ...
pip install --user scikit-learn &> /dev/null
echo Done!

echo installing pyBrain ...
# Commands to install pyBrain
wget http://github.com/pybrain/pybrain/tarball/master &> /dev/null
tar xzvf master pybrain-pybrain-9c56862 &> /dev/null
cd pybrain-pybrain-9c56862 &> /dev/null
python ./setup.py install --user &> /dev/null
cd .. &> /dev/null
rm master &> /dev/null 
rm -rf pybrain-pybrain-9c56862/ &> /dev/null
rm -rf PyBrain.egg-info/ &> /dev/null
rm -rf build/ &> /dev/null
rm -rf dist &> /dev/null
echo Done!

echo -e '\nAll dependencies installed successfully!\n'
