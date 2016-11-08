TSC Library
==============

The TSC library (libTSC) uses the hardware resources of modern x86_64 CPUs to determine the CPU cycle count from the latest reset. The TSC is a 64-bit register that can be used as a time-stamp counter that increments at aconstant rate, which may be set by the maximum core-clock to bus-clock ratio of the processor or may be set by the maximum resolved frequency at which the processor is booted. Since the family 10h (Barcelona/Phenom) for AMD and Nehalem CPUs for Intel, the TSC increases at a constant rate and is synchronized across allcores of the CPU.

Usage
--------------

To use the TSC library certain calls have to be integrated into the source code. First the library has to be initialized using the "resetTSC()" call. Then the clock cycles can be obtained by calling "getTscDiff()" (clock cycles from the reset call) or "readTsc()" (clock cycles from the start of the CPU. The following is an example to measure the clock cycles in a second:

Example
--------------

<pre>
#include <libtsc.h>
int main(void) {
  Tsc_t ticks;
  
  resetTSC();
  sleep(1);
  ticks = getTscDiff();
  
  printf("Ticks: %llu\n", (ticks));
  
  return 0;
}
</pre>
