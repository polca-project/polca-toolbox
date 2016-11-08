libEMOHH
==============

The Energy Monitoring for Hazel Hen HPC system (libEMOHH) uses energy reporting hardware of each node to create reports of the memory usage on the system. POLCA tools can use this library to asses the performance and detailed energy usage and drive the code transformations. 


Usage
--------------

Include and compile the library with the application to be analyzed. First the "EHHReset()" function call has to be used to initialize the library data structures and then the functions "EHHGetEnergy()" and "EHHGetTime()" functions can be used to obtain the energy used and the time passed since the initialization of the library.
