Smart Driver
============

The SmartDriver is a set of tools and libraries that allow the static
and dynamic instrumentation, profiling and run-time control of applications. The
different components of SmartDriver are designed to gather the necessary information
to assist and asses the impact of the POLCA transformations on the applications.

ASMCount 
--------

ASMCount statically analyzes the binary code generated for the compiler and predicts the computational cost of the different POLCA scopes in the code. The output is a JSON file that can be used by other tools to determine the code transformations to perform or visualization tools such as SDFpy and POGADE.

EMOXU3
--------

EMOXU-3, based on libEMOXU-3, is provided and allows to measure and create detailed reports of performance and energy usage of executable files and scripts.

EMOhh
--------

EMOHH is based on libEMOHH, is provided and allows to measure and create detailed reports of performance and energy usage of executable files and scripts on Hazel Hen nodes.

libEMOHH
--------

The Energy Monitoring for Hazel Hen HPC system (libEMOHH) uses energy reporting hardware of each node to create reports of the memory usage on the system. POLCA tools can use this library to asses the performance and detailed energy usage and drive the code transformations. 

libEMOXU3
--------

The Energy Monitoring for Odroid XU-3 board (libEMOXU-3) uses the four voltage and current sensors that power the SoC to report the energy usage of the ARM Cortex A-15 cluster, the ARM Cortex A-7 cluster, the GPU and the system memory to create reports of the memory usage on the system.

libTSC
--------

The TSC library (libTSC) uses the hardware resources of modern x86_64 CPUs to determine the CPU cycle count from the latest reset.

libACC
--------

The ARM Cycle Counter library (libACC) uses the hardware resources of ARM-v7 and newer ARM-based CPUs to obtain the cycle counters of each core since it was reset. 

polca-event-driver
--------

The POLCA event driver is a tool based on the Extrae package that generates trace-files for post-mortem analysis. This tool uses different interposition mechanisms to inject probes into the target application so as to gather information regarding the application performance.
