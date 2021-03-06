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

Alternativelly, the [instalation script used by POGADE](https://github.com/polca-project/polca-toolbox/blob/master/installation_scripts/s2s_install_pogade.sh) could be adapted and used to install the tool. 


The machine learning module
------------

A script is provided for installing the required dependencies of the Machine Learning Module. Simply run from the command line:

```
$ ./install.sh
```

ClaSH
------------

ClaSH is written in Haskell and depends on the [GHC] Haskell compiler (http://haskell.org/ghc) and requires [Cabal](http://www.haskell.org/cabal/download.html). If these have been installed, you can install ClaSH simply by running

for i386 Linux:
```
cabal install clash-ghc --enable-documentation --enable-executable-dynamic
```

others:
```
cabal install clash-ghc --enable-documentation
```

Please refer to [ClaSH Setup](http://www.clash-lang.org/#details) for a detailed instruction on how to install GHC and Cabal.
  
SmartDriver
------------

[SmartDriver](https://github.com/polca-project/polca-toolbox/tree/master/SmartDriver) is composed by different components.  Some are libraries that have to be used within the source code (to use it is just needed to download it, make the desired calls to the interface of the library and compile), others are scripts that will run if python is installed and, finally, others just need to be compiled with a generic C compiler not needing any special dependencies to be installed.  Visit the description page of it component for their installation details and usage.


Maxeler DFE Platform and FlexAware
------------

The [Maxeler DFE Platform](https://www.maxeler.com/products/software/) and [FlexAware](http://www.flexaware.net/what/sde/) are commercial tools available through [MAXELER Technologies](https://www.maxeler.com) and [RECORE Systems](http://recoresystems.com), respectively. For installation and usage instructions, please refer to the respective provider's website.
