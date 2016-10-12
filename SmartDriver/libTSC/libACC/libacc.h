#ifndef LIBACC_H
#define LIBACC_H

void resetACC();
static inline void init_perfcounters (int32_t do_reset, int32_t enable_divider);
static inline unsigned int get_cyclecount ();
unsigned int getAccDiff();

#endif //LIBACC_H
