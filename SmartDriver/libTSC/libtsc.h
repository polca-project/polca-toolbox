#ifndef LIBTSC_H
#define LIBTSC_H

typedef unsigned long long Tsc_t;

void resetTSC();
inline Tsc_t readTsc();
Tsc_t getTscDiff();

#endif //LIBTSC_H
