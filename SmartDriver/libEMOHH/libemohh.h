#ifndef LIBEMOHH_H
#define LIBEMOHH_H

uint64_t sTime;
uint64_t sEnergy;

uint64_t EHHReset();
uint64_t EHHGetEnergyNow();
uint64_t EHHGetTimeNow();
uint64_t EHHGetEnergy();
uint64_t EHHGetTime();

#endif
