#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "params.h"

int check_output(float *ref,float *out,int nElems) {

  int nErrors = 0;

  for(int i=0;i<nElems;i++){
    if(fabs(ref[i]-out[i]) > FP_TOLERANCE) {
      printf("\tError: %d - %.16f != %.16f\n",i,ref[i],out[i]);
      nErrors++;
    }
    /* else */
    /*   printf("\tOK: %d - %.16f != %.16f\n",i,ref[i],out[i]);       */
  }

  return nErrors;

}

int main() {

  int nBodies1,dataType1;
  int nBodies2,dataType2;
  float *ref,*out;
  int nErrors;

  int nFiles = 2;
  const char* const refFiles[] = { "../../data/pos_ref.dat", 
				   "../../data/vel_ref.dat"};

  const char* const outFiles[] = { "../../data/pos_out.dat",
				   "../../data/vel_out.dat"};

  for(int i=0;i<nFiles;i++) {
    printf("Comparing file %s\n",refFiles[i]);
    printf("     with file %s\n",outFiles[i]);
    input_arr_arg(refFiles[i],  &ref,&nBodies1,&dataType1);
    input_arr_arg(outFiles[i],  &out,&nBodies2,&dataType2);
    assert(nBodies1 == nBodies2 && dataType1 == dataType2);
    nErrors = check_output(ref,out,nBodies1);
    if(!nErrors) printf("test OK!\n");
    printf("------------------------------------------------\n");
  }

  return 0;

}
