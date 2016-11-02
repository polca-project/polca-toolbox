
// Obtained from: https://github.com/harrism/mini-nbody

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "params.h"

/* void randomizeBodies(float *x,float *y,float *z,float *vx,float *vy,float *vz, int n) { */
/*   for (int i = 0; i < n; i++) { */
/*     x[i]  = 2.0f * (rand() / (float)RAND_MAX) - 1.0f; */
/*     y[i]  = 2.0f * (rand() / (float)RAND_MAX) - 1.0f; */
/*     z[i]  = 2.0f * (rand() / (float)RAND_MAX) - 1.0f; */
/*     vx[i] = 2.0f * (rand() / (float)RAND_MAX) - 1.0f; */
/*     vy[i] = 2.0f * (rand() / (float)RAND_MAX) - 1.0f; */
/*     vz[i] = 2.0f * (rand() / (float)RAND_MAX) - 1.0f; */
/*   } */
/* } */

void randomizeBodies(float *pos,float *vel, int n) {
  for (int i = 0; i < n*DIM; i+=DIM) {
    pos[i]   = 2.0f * (rand() / (float)RAND_MAX) - 1.0f;
    pos[i+1] = 2.0f * (rand() / (float)RAND_MAX) - 1.0f;
    pos[i+2] = 2.0f * (rand() / (float)RAND_MAX) - 1.0f;
    vel[i]   = 2.0f * (rand() / (float)RAND_MAX) - 1.0f;
    vel[i+1] = 2.0f * (rand() / (float)RAND_MAX) - 1.0f;
    vel[i+2] = 2.0f * (rand() / (float)RAND_MAX) - 1.0f;
  }
}


int main(const int argc, const char** argv) {
  
  int nBodies = PROBLEM_SIZE;
  /* if (argc > 1) nBodies = atoi(argv[1]); */
  
  int bytes = nBodies*sizeof(float)*DIM;
  /* float *x, *y, *z, *vx, *vy, *vz; */
  /* x  = (float*)malloc(bytes); */
  /* y  = (float*)malloc(bytes); */
  /* z  = (float*)malloc(bytes); */
  /* vx = (float*)malloc(bytes); */
  /* vy = (float*)malloc(bytes); */
  /* vz = (float*)malloc(bytes); */

  float *pos,*vel;
  pos = (float*)malloc(bytes);
  vel = (float*)malloc(bytes);

  /* randomizeBodies(x,y,z,vx,vy,vx, nBodies); // Init pos / vel data */
  randomizeBodies(pos,vel, nBodies); // Init pos / vel data

  output_arr_arg("../data/pos_input.dat",  pos, nBodies*DIM, 4);
  output_arr_arg("../data/vel_input.dat",  vel, nBodies*DIM, 4);

  free(pos);
  free(vel);

  /* output_arr_arg("../data/x_input.dat",  x, nBodies, 4); */
  /* output_arr_arg("../data/y_input.dat",  y, nBodies, 4); */
  /* output_arr_arg("../data/z_input.dat",  z, nBodies, 4); */
  /* output_arr_arg("../data/vx_input.dat", vx, nBodies, 4); */
  /* output_arr_arg("../data/vy_input.dat", vy, nBodies, 4); */
  /* output_arr_arg("../data/vz_input.dat", vz, nBodies, 4); */

  /* free(x); */
  /* free(y); */
  /* free(z); */
  /* free(vx); */
  /* free(vy); */
  /* free(vz); */
}
