## Maxeler DFE platform and MaxCompiler

Maxeler provides hardware and software solutions for accelerating computing applications
on dataflow engines (DFEs). 

DFEs are in-house designed accelerators
that encapsulate reconfigurable high-end FPGAs at their core and are equipped
with large amounts of DDR memory.

A Maxeler dataflow computing application consists of two parts: a DFE part
and a CPU part. The DFE part performs the compute-intensive parts of the application
while the CPU part acts as a host that sets up and manages the computation
on the DFE and also carries out the control-intensive tasks. 

The compute-intensive DFE parts are described in the MaxJ programming language which is a Java-based
metaprogramming approach. The compute kernels handling the data-intensive part
of the application and the associated manager, which orchestrates data movement
within the DFE, are written using this language. The CPU part of the application
can be written in C, C++, Matlab, Python, Fortran or many other conventional
languages.

Maxeler provides an in-house developed programming environment and runtime
which comprises of the following components (that currently require Linux
CentOS version 6.7 or higher):

* **MaxCompiler** Maxeler’s custom-built general-purpose programming environment
for developing data-flow applications. MaxCompiler takes as input a MaxJ
program and generates a DFE implementation (a .max file) that can be called
from a CPU application via the SLiC interface (see below). A .max file contains
a bitstream as well as relevant metadata.

* **SLiC** The Simple Live CPU interface is Maxeler’s application programming interface
for seamless CPU-DFE integration. SLiC allows CPU applications
to configure and load a number of DFEs as well as to subsequently schedule
and run actions on those DFEs using simple function calls.

* **MaxelerOS** A software layer and run time sitting between the SLiC interface, the
Linux operating system and the hardware, which manages DFE hardware
and CPU-DFE interactions in a way transparent to the user.

* **MaxIDE** A specialised Eclipse-based integrated development environment for MaxJ
and DFE design, a fast DFE software simulator and comprehensive debug
provisioning tool set used during development.

MaxCompiler, SLiC, MaxelerOS and MaxIDE have been developed by Maxeler
Technologies. 

For more details on these tools please check [Maxeler's website](https://www.maxeler.com/products/software/).

A wide variety of examples and their implementations are available at [Maxeler's AppGallery](http://appgallery.maxeler.com/#/).

