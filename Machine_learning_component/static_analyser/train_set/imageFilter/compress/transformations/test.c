#include "stdio.h"

#define I 3
#define J 4
#define K 2

void printArr(void *arr){

  int i,j,k;

  int *linArr = (int*) arr;

  for(i = 0; i < I; i++) {
    for(j = 0; j < J; j++) {
      for(k = 0; k < K; k++) {
	printf("%d\n",linArr[i*(J*K)+j*K+k]);
      }
    }
  }

  for(i = 0; i < J*K; i++) {
    printf("%d\n",linArr[i]);
  }


}

int main()
{
  int arr[I][J][K];

  int i,j,k;

  int value = 0;
  for(i = 0; i < I; i++) {
    for(j = 0; j < J; j++) {
      for(k = 0; k < K; k++) {
	arr[i][j][k] = value;
	value++;
      }
    }
  }  

  for(i = 0; i < I; i++) {
    for(j = 0; j < J; j++) {
      for(k = 0; k < K; k++) {
	printf("%d\n",arr[i][j][k]);
      }
    }
  }

  for(i = 0; i < I; i++) {
    for(j = 0; j < J; j++) {
      for(k = 0; k < K; k++) {
	/* if(arr[i][j][k]!=arr[i*I+j*J+k*K]) */
	int *linArr = (int*) arr;
	printf("%d - %d (%d)\n",arr[i][j][k],linArr[i*(J*K)+j*K+k],i*(J*K)+j*K+k);
      }
    }
  }

  printArr(arr);

  /* for(i = 0; i < J*K; i++) { */
  /*   printf("%d\n",arr[i][i][i]); */
  /* } */

  return 0;
}
