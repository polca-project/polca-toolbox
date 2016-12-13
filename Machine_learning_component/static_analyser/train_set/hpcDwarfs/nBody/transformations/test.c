
#include <stdio.h>

#define SIZE 3
#define DIM  3

void main() {

  int arr[SIZE*DIM];
int i,j,t,k,t1;

  for(i=0;i<SIZE*DIM;i++)
    arr[i] = i;

/*   for(i=0;i<SIZE;i++){ */
/*     for(j=0;j<SIZE;j++) { */
/*       for(k=0;k<DIM;k++) { */
/*         printf("%3d, ",arr[i*DIM+k]); */
/*       } */
/*     } */
/*     printf("\n"); */
/*   } */

/*   printf("\n\n"); */

/*   for(t=0;t<SIZE*SIZE*DIM;t++) { */

/*       i = t/(SIZE*DIM); */
/*       j = t%(SIZE*DIM); */
/*       k = j % DIM; */

/*       int aj = arr[t-(i*SIZE*DIM)]; */
/* int ai = arr[(t-(i*(SIZE-1)*DIM+j))+k]; */
      
/*     printf("%3d, ",ai); */

/*       if(j==(SIZE*DIM-1)) */
/*   	printf("\n"); */
/*   } */


  for(i=0;i<SIZE;i++){
    for(j=0;j<SIZE;j++) {
      for(k=0;k<DIM;k++) {
        printf("%3d, ",arr[j*DIM+k]-arr[i*DIM+k]);
        /* printf("%3d, ",arr[i*DIM+k]);  */
      }
    }
    printf("\n");
  }

  printf("\n\n");

  for(t=0;t<SIZE*SIZE*DIM;t++){

      i  = t/(SIZE*DIM);
      t1 = t%(SIZE*DIM);
      j  = t1/DIM;
      k  = t1 % DIM;

      int aj = arr[t-(i*SIZE*DIM)];
      int ai = arr[(t-(i*(SIZE-1)*DIM+j*DIM))];

      printf("%3d, ",aj - ai);
      /* printf("%3d, ",ai); */

      if(j==(SIZE-1) && k==(DIM-1))
  	printf("\n");
  }

}
