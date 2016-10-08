#include "libemohh.h"
#include <stdlib.h>

uint64_t EHHReset() {
  sTime = EHHGetTimeNow();
  sEnergy = EHHGetEnergyNow();
}

uint64_t EHHGetEnergyNow() {
  FILE *f;
  uint64_t energy;

  f = fopen("/sys/cray/pm_counters/energy", "r");
  fscanf(f, "%llu", &energy);
  fclose(f);

  return energy;
}

uint64_t EHHGetTimeNow() {
  uint64_t tn;
  struct timeval tv;
  gettimeofday(&tv, NULL);

  tn = (tv.tv_sec * 1000000) + tv.tv_usec;

  return tn;
}

uint64_t EHHGetEnergy() {
  return EHHGetEnergyNow() - sEnergy;
}

uint64_t EHHGetTime() {
  return EHHGetTimeNow() - sTime;
}
