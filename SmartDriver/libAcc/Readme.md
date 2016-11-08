ACC Library
==============

The ARM Cycle Counter library (libACC) uses the hardware resources of ARM-v7 and newer ARM-based CPUs to obtain the cycle counters of each core since it was reset. This counter is linked to the frequency to which core is running, thus there is a different counter for each core with different values as they do not need to be synchronized. The access to these resources can only be exposed by the Operating System, so the EACC linux kernel module is provided for the library to work.

Usage
--------------

To use the ACC library certain calls have to be integrated into the source code. First the library has to be initialized using the "resetACC()" call. Then the clock cycles can be obtained by calling "getAccDiff()" (clock cycles from the reset call) or "get_cyclecount()" (clock cycles from the start of the ARM CPU.

Example
--------------

The following is an example to measure the clock cycles in a second:

<pre>
#include <libacc.h>

int main(void) {
  unsigned int ticks;

  resetACC();
  sleep(1);
  ticks = getTscDiff();

  printf("Ticks: %u\n", (ticks));
  
  return 0;
}
</pre>
