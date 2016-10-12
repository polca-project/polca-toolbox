#include "libtsc.h"

Tsc_t sT;

void resetTSC() {
  sT = readTsc();
}

inline Tsc_t readTsc()
{
  unsigned hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  return ( (unsigned long long)lo)|( ((unsigned long long)hi)<<32 );
}

Tsc_t getTscDiff() {
  return readTsc - sT;
}
