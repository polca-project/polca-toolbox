libEMOXU3
==============

The Energy Monitoring for Odroid XU-3 board (libEMOXU-3) uses the four voltage and current sensors that power the SoC to report the energy usage of the ARM Cortex A-15 cluster, the ARM Cortex A-7 cluster, the GPU and the system memory to create reports of the memory usage on the system. This library can be used to asses the performance and detailed energy usage. The class GetNode provides the instrumentation calls and necessary data structures to obtain and process the energy information of the different devices of the Exynos 5422 SoC.

Usage
--------------

Compile it as part of the application and use the "GetNode" class to obtain the different data as escribed in "libemoxu3.h"
