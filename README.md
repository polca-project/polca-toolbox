# polca-toolbox

The set of tools delivered by POLCA consists of the following components.
You can check individual folders under this repository for more details on each tool.

## C source to source transformation tool

The C source to source transformation tool is a program transformation environment,
implemented in Haskell, where architecture-agnostic scientific C code with
semantic annotations is transformed into functionally equivalent code better suited
for a given platform. 

The transformation steps are represented as rules which can
be fired when certain syntactic and semantic conditions are fulfilled. These rules
are not hard-wired into the rewriting engine: they are written in a C-like language
(STML) and are automatically processed and incorporated by the rewriting engine.
That makes it possible for end-users to add their own rules or to provide sets of
rules which are adapted to certain specific domains or purposes.
The C source transformation toolchain has been contributed by the IMDEA
Software Institute and the Technical University of Madrid.

## CLaSH

CLaSH – pronounced: clash – is a functional hardware description language that
borrows both the syntax and semantics from the functional programming language
Haskell. 

It provides a familiar structural design approach to both combinational
and synchronous sequential circuits. 

The CLaSH compiler transforms these highlevel
descriptions to low-level synthesisable VHDL, Verilog, or SystemVerilog.
The CLaSH compiler is developed at the University of Twente, in the chair
CAES (Computer Architecture for Embedded Systems).

## Haskell transformations

The Haskell transformations tool consists of two parts:

* H2H: Haskell-to-Haskell transformations, which realize mathematical equivalences of algorithmic structures into actual transformations of Haskell code,
* H2C: Haskell-to-C transformations, to generate C-code from specifications in Haskell of algorithmic structures.

The Haskell-to-Haskell transformation tool (H2H) can apply transformations
to Haskell code that is derived from the annotations attached to a given C-program.
The tool H2H first of all parses this Haskell program to produce its abstract syntax
tree (AST), and then produces a new abstract tree by applying a transformation
rule.

The Haskell to C tool generates C-code from Haskell code, in particular from
higher order functions since these functions express algorithmic structure. We will
present the method for two higher order functions (HOF): map and foldl, the others
following the same pattern.

H2H and H2C have been developed at the University of Twente and their code
is licensed under a 2-clause BSD license.

## POGADE

POGADE (POlca Graph Analysis & Development Environment) is a tool that
makes it possible to visualize the syntactic structure of POLCA-anotated C code,
showing as a graph graph the inter-relationships and hierarchies between blocks
of code affected by POLCA annotations, the HO combinators and the data dependencies
among them. Visualization parameters, including the ability for the user to
surf the generated graph (i.e., zooming in and out subgraphs and the hierarchical
annotations) can be changed interactively; it supports exporting to external files.
POGADE also integrates a dedicated code editor with C reserved keyword and
POLCA directive highlighting, and help for introducing POLCA annotations.
POGADE has been developed at HLRS and is licensed under GPLv3 (all dependencies
fall under open-source licenses).

## PoRoTo

The PoRoTo tool automatises several tasks related to the compilation of C code
aimed at its execution on FPGAs. The tool expects a set of C source files containing
the code to be executed on a CPU and a separate file containing a C function
that will be translated to VHDL, compiled, synthesised and sent to the target FPGA
accelerator board. The code on the CPU side will be adapted by PoRoTo to substitute
the call for the original function by calls to the FPGA board driver API.
PoRoTo also adapts the C code for being fed to the ROCCC tool, generates
VHDL code for interfaces, FPGA glue and the CPU code that interfaces with the
FPGA implementation (sending the bit stream to program the FPGA, transferring
the data, execute the remote code and retrieve back the result data). Special comments
in the C code help PoRoTo decide on which are the original data structures
to be treated as input and output streams.
Poroto has been developed at CETIC, in the department of Embedded and
Communicating Systems.

## Machine learning component

The machine learning component provides strategies to guide (automatically) the
transformation process implemented by the source-to-source transformation engine.
The C transformation tool is able to apply code transformation rules (written
in STML) provided they are semantically sound. However, different rules can be
applied at a given time and, while it is possible to ask the user for the next rule to
apply, it is batter to have some way of doing that automatically.
This component uses code abstraction, reinforceent learning and classification
tree techniques in order to learn “good” patterns for applying the transformation
rules. At this moment, this component is rather coupled with the source to source
engine, so it is not yet a standalone product of the POLCA project. However, some
of their principles can potentially be applied in a broader context.
The machine learning component has been developed at the IMDEA Software
Institute.

## SmartDriver

The SmartDriver has comprises a set of tools and libraries that allow the static and
dynamic instrumentation, profiling and run-time control of applications. The different
components of SmartDriver are designed to gather the necessary information
to assist and asses the impact of the POLCA transformations on the applications.
* TSC library The TSC library (libTSC) uses the hardware resources of modern
x86_64 CPUs to determine the CPU cycle count from the latest reset.
* ACC library The ARM Cycle Counter library (libACC) uses the hardware resources
of ARM-v7 and newer ARM-based CPUs to obtain the cycle counters
of each core since it was reset.
* libEMOXU-3 The Energy Monitoring for Odroid XU-3 board library (libEMOXU-3)
uses the four voltage and current sensors that power the SoC to report the energy
usage of the ARM Cortex A-15 cluster, the ARM Cortex A-7 cluster,
the GPU and the system memory to create reports of the memory usage on
the system.
* libEMOHH The Energy Monitoring for Hazel Hen HPC system (libEMOHH)
uses energy reporting hardware of each node to create reports of the memory
usage on the system.
* ASMCount ASMCounts an application that analyzes statically the binary code
generated for the compiler and predicts the computational cost of the different
POLCA scopes in the code.
The output is a JSON file that can be used by other tools to determine the
code transformations to perform or visualization tools such as SDFpy and
POGADE.
* POLCA Event Driver The POLCA event driver is a tool based on the Extrae
package that generates race-files for post-mortem analysis. This tool uses
different interposition mechanisms to inject probes into the target application
so as to gather information regarding the application performance. It supports
different platforms (Generic x86 and ARM linux-based clusters, Cray
Systems and GPUs among others) and many programming models such as
OpenMP, OpenCL and MPI.

## SDFpy

SDFpy is a library for the management and analysis of synchronous dataflow graphs,
written in Python. Synchronous dataflow (SDF) graphs model stream processing
systems. An SDF graph represents a function, or computation, which is applied to
an infinite stream of data. Nodes in an SDF graph act as functions: they map input
(data) streams, taken from incoming edges, to output streams, which are produced
onto outgoing edges. The model allows for temporal analysis: by assigning (worstcase)
processing times (execution times) to the graph’s nodes, one may compute
how much data can be processed by an SDF graph, per time unit.
SDFpy is capable of performing several analyses on SDF graphs, including
throughput analysis, which computes the performance bottleneck of the graph.
Performance bottlenecks indicate which sequences of computations limit the maximum
throughput of the graph, and thus provide valuable insight into which parts of
the computation should be improved, or assigned more resources.
SDFpy has been developed at the University of Twente.

## Maxeler DFE platform

Maxeler provides hardware and software solutions for accelerating computing applications
on dataflow engines (DFEs). DFEs are in-house designed accelerators
that encapsulate reconfigurable high-end FPGAs at their core and are equipped
with large amounts of DDR memory.
A Maxeler dataflow computing application consists of two parts: a DFE part
and a CPU part. The DFE part performs the compute-intensive parts of the application
while the CPU part acts as a host that sets up and manages the computation
on the DFE and also carries out the control-intensive tasks. The compute-intensive
DFE parts are described in the MaxJ programming language which is a Java-based
metaprogramming approach. The compute kernels handling the data-intensive part
of the application and the associated manager, which orchestrates data movement
within the DFE, are written using this language. The CPU part of the application
can be written in C, C++, Matlab, Python, Fortran or many other conventional
languages.
Maxeler provides an in-house developed programming environment and runtime
which comprises of the following components (that currently require Linux
CentOS version 6.7 or higher):

* MaxCompiler Maxeler’s custom-built general-purpose programming environment
for developing data-flow applications. MaxCompiler takes as input a MaxJ
program and generates a DFE implementation (a .max file) that can be called
from a CPU application via the SLiC interface (see below). A .max file contains
a bitstream as well as relevant metadata.
* SLiC The Simple Live CPU interface is Maxeler’s application programming interface
for seamless CPU-DFE integration. SLiC allows CPU applications
to configure and load a number of DFEs as well as to subsequently schedule
and run actions on those DFEs using simple function calls.
* MaxelerOS A software layer and run time sitting between the SLiC interface, the
Linux operating system and the hardware, which manages DFE hardware
and CPU-DFE interactions in a way transparent to the user.
* MaxIDE A specialised Eclipse-based integrated development environment for MaxJ
and DFE design, a fast DFE software simulator and comprehensive debug
provisioning tool set used during development.

MaxCompiler, SLiC, MaxelerOS and MaxIDE have been developed by Maxeler
Technologies. All extensions developed within the context of POLCA are not
open source and will not be made available on the project’s GitHub site.

## Flexaware

FlexaWare is an embedded heterogeneous many-core processing platform, comprising
a many-core hardware architecture and a many-core coordination software
layer. The embedded many-core platform offers flexibility and scalability to cater
to range of application domains from clear cut to compute intensive and massively
parallel. The many-core architecture can be implemented on FPGA or ASIC.
FlexaWare offers abstraction from the many-core hardware complexity using
a task-based programming model. The programming model targets heterogeneous
distributed (shared) memory systems. An application runs on top of the many-core
coordination software layer. Applications are composed hierarchically of explicitly
defined tasks and communication channels.
POLCA’s source to source transformation engine uses the annotations in the
original C code to identify functions that can be used to extract tasks that will be
passed to the FlexaWare compiler.
FlexaWare has been developed by Recore Systems.
