#include "libemohh.h"
#include <stdlib.h>
#include <iostream>
#include <unistd.h>
#include <string>
#include <sstream>
#include <fstream>
#include <sys/time.h>

uint64_t sTime;
uint64_t sEnergy;


void EHHReset() {
  sTime = EHHGetTimeNow();
  sEnergy = EHHGetEnergyNow();
}

uint64_t EHHGetEnergyNow() {
  FILE *f;
  uint64_t energy;

  f = fopen("/sys/cray/pm_counters/energy", "r");
  fscanf(f, "%lu", &energy);
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
