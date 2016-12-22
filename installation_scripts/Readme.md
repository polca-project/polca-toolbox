Installation Scripts
====================

Installation scripts for to ease the installation of POLCA tool-box components.

POGADE
------

It requires a modern debian-based GNU/Linux distribution (preferably an Ubuntu 16.04 flavour).
Download an run as root the pogade_install.sh script file, which will download compile and install the dependencies and POGADE itself.

C source to source transformation tool
------------

In order to compile all the needed dependencies and complementary packages for the C source to source transformation tool, the following commands should be run:

```
$ make configure
$ make rules FILE=rules.c
$ make all
```

Further information at [the C source to source transformation tool's page](https://github.com/polca-project/polca-toolbox/tree/master/C_source2source).


The machine learning module
------------

A script is provided for installing the required dependencies of the Machine Learning Module. Simply run from the command line:

```
$ ./install.sh
```
