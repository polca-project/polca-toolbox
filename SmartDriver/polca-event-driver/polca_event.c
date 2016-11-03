#include "inst_polca.h"

//wrapper for C/C++ interface
void polca_event(unsigned int a, unsigned int b)
{ 
        Extrae_eventandcounters(a,b);
}

//wrapper for fortran interface
void polca_event_(unsigned int *a, unsigned int *b)
{
        Extrae_eventandcounters(*a,*b);
}
