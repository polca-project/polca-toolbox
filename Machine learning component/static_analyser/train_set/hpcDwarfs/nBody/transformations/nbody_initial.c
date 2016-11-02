
// Obtained from: https://github.com/harrism/mini-nbody

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "timer.h"
#include "params.h"
#define SOFTENING 1e-9f

/* #define PROFILING 1 */

typedef struct { float x, y, z, vx, vy, vz; } Body;

void randomizeBodies(float *data, int n) {
  for (int i = 0; i < n; i++) {
    data[i] = 2.0f * (rand() / (float)RAND_MAX) - 1.0f;
  }
}

void bodyForce(Body *p, float dt, int n) {
  for (int i = 0; i < n; i++) { 
    float Fx = 0.0f; float Fy = 0.0f; float Fz = 0.0f;

    for (int j = 0; j < n; j++) {
      float dx = p[j].x - p[i].x;
      float dy = p[j].y - p[i].y;
      float dz = p[j].z - p[i].z;
      float distSqr = dx*dx + dy*dy + dz*dz + SOFTENING;
      float invDist = 1.0f / sqrtf(distSqr);
      float invDist3 = invDist * invDist * invDist;

      Fx += dx * invDist3; Fy += dy * invDist3; Fz += dz * invDist3;
    }

    printf("%d: %f %f %f %f\n",n,dt,Fx,p[i].vx,p[i].vx + dt*Fx);
    p[i].vx += dt*Fx; p[i].vy += dt*Fy; p[i].vz += dt*Fz;
    printf("%d: %f\n",n,p[i].vx);
  }
}

int main(const int argc, const char** argv) {
  
  int nBodies = PROBLEM_SIZE;
  /* if (argc > 1) nBodies = atoi(argv[1]); */

  const float dt = 0.01f; // time step
  const int nIters = ITERS;  // simulation iterations

  int bytes = nBodies*sizeof(Body);
  float *buf = (float*)malloc(bytes);
  Body *p = (Body*)buf;

  randomizeBodies(buf, 6*nBodies); // Init pos / vel data

  double totalTime = 0.0;

#if PROFILING == 1
 for(int i=0;i<SAMPLES;i++){
    StartTimer();
#endif
  for (int iter = 1; iter <= nIters; iter++) {


    bodyForce(p, dt, nBodies); // compute interbody forces

    for (int i = 0 ; i < nBodies; i++) { // integrate position
      p[i].x += p[i].vx*dt;
      p[i].y += p[i].vy*dt;
      p[i].z += p[i].vz*dt;
    }


    /* if (iter > 1) { // First iter is warm up */
    /*   totalTime += tElapsed;  */
    /* } */
/* #ifndef SHMOO */
/*     printf("Iteration %d: %.3f seconds\n", iter, tElapsed); */
/* #endif */
  }
#if PROFILING == 1
  GetTimer();
 } // End of loop for time samples
#endif

/*   double avgTime = totalTime / (double)(nIters-1);  */

/* #ifdef SHMOO */
/*   printf("%d, %0.3f\n", nBodies, 1e-9 * nBodies * nBodies / avgTime); */
/* #else */
/*   printf("Average rate for iterations 2 through %d: %.3f +- %.3f steps per second.\n", */
/*          nIters, rate); */
/*   printf("%d Bodies: average %0.3f Billion Interactions / second\n", nBodies, 1e-9 * nBodies * nBodies / avgTime); */
/* #endif */
  free(buf);
}
