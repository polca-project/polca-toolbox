The POLCA event driver is a tool based on the Extrae package that generates trace-files for post-mortem analysis. This tool uses different interposition mechanisms to inject probes into the target application so as to gather information regarding the application performance. It supports different platforms (Generic x86 and ARM linux-based clusters, Cray Systems and GPUs among others) and many programming models such as OpenMP, OpenCL and MPI.

POLCA Event Driver takes advantage of multiple interposition mechanisms to add monitors into the application. No matter which mechanism is being used, the target is the same, to collect performance metrics at known applications points to finally provide the performance analyst a correlation between performance and the application execution.


Installation
--------------

Compiling can be done using the Makefile, so just execute "make".


Usage
--------------

Include the library to instrumentate the code. Predefined calls are in the file "polca_tran.c", or build custom ones extending the descriptions of "inst_polca.h".
