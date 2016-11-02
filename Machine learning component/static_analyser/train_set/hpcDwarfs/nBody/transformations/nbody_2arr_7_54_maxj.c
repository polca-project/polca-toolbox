
// Obtained from: https://github.com/harrism/mini-nbody

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "timer.h"
#include "params.h"


void bodyForce(float *pos,float *vel, float dt, int n) {
  for (int i = 0; i < n*DIM; i+=3) {
    float Fx = 0.0f; float Fy = 0.0f; float Fz = 0.0f;

    for (int j = 0; j < n*DIM; j+=3) {
      float dx = pos[j]   - pos[i];
      float dy = pos[j+1] - pos[i];
      float dz = pos[j+2] - pos[i];
      float distSqr = dx*dx + dy*dy + dz*dz + SOFTENING;
      float invDist = 1.0f / sqrtf(distSqr);
      float invDist3 = invDist * invDist * invDist;

      Fx += dx * invDist3; Fy += dy * invDist3; Fz += dz * invDist3;
    }
    /* printf("%d: %f %f %f %f\n",n,dt,Fx,vx[i],vx[i] + dt*Fx); */
    vel[i] += dt*Fx; vel[i+1] += dt*Fy; vel[i+2] += dt*Fz;
    /* printf("%d: %f\n",n,vx[i]); */
    /* printf("%d:\n",i); */
  }
}

int main(const int argc, const char** argv) {
  
  int nBodies,nElems;
  int dataType;
  /* if (argc > 1) nBodies = atoi(argv[1]); */

  const float dt   = DT;    // time step
  const int nIters = ITERS; // simulation iterations

  /* int bytes = nBodies*sizeof(Body); */
  /* float *buf = (float*)malloc(bytes); */
  /* Body *p = (Body*)buf; */
  
  float *pos,*vel;

  input_arr_arg("../../data/pos_input.dat",  &pos,&nElems,&dataType);
  input_arr_arg("../../data/vel_input.dat",  &vel,&nElems,&dataType);

  nBodies = nElems / DIM;

#if PROFILING == 1
 for(int sample=0;sample<SAMPLES;sample++){
    StartTimer();
#endif
  for (int iter = 1; iter <= nIters; iter++) {
    /* printf("%d: %f\n",nBodies,x[0]); */
    /* bodyForce(pos,vel,dt, nBodies); // compute interbody forces */

    float Fx;
    float Fy;
    float Fz;

    float dx;
    float dy;
    float dz;
    float *distSqr = (float*) malloc(sizeof(float) *nBodies*nBodies*3*3);
    float *invDist = (float*) malloc(sizeof(float) *nBodies*nBodies*3*3);
    float *invDist3 = (float*) malloc(sizeof(float) *nBodies*nBodies*3*3);
    float *d = (float*) malloc(sizeof(float) *nBodies*nBodies*3*3);
    /* float d3[3]; */
    float *F = (float*) malloc(sizeof(float) *nBodies*nBodies*3*3);

    for (int ijkw = 0; ijkw < nBodies*nBodies*3*3; ijkw++) {
      int i   = ijkw / (nBodies*3*3);
      int jkw = ijkw % (nBodies*3*3);
      int j   = jkw / (3*3);
      int kw  = jkw % (3*3);
      int k   = kw / 3;
      int w   = kw % 3;
	  
      d[ijkw] = w==0 ? pos[j*3+k]   - pos[i*3+k] : d[ijkw-1];
      /* d3[k] = w==0 ? pos[j*3+k]   - pos[i*3+k] : d3[k]; */
      
      distSqr[ijkw]  = w==0 && k==2 ? d[ijkw-3*(2-0)]*d[ijkw-3*(2-0)] + d[ijkw-3*(2-1)]*d[ijkw-3*(2-1)] + d[ijkw-3*(2-2)]*d[ijkw-3*(2-2)] + SOFTENING : distSqr[ijkw-1];
      /* distSqr[ijkw]  = w==0 && k==2 ? d3[0]*d3[0] + d3[1]*d3[1] + d3[2]*d3[2] + SOFTENING : distSqr[ijkw]; */
      /* if(w==0 && k==2) printf("%3d: % .3f - % .3f\n",ijkw,d3[0],d[ijkw-3*(2-0)]); */
      /* if(w==0 && k==2) printf("%3d: % .3f - % .3f\n",ijkw,d3[1],d[ijkw-3*(2-1)]); */
      /* if(w==0 && k==2) printf("%3d: % .3f - % .3f\n",ijkw,d3[2],d[ijkw-3*(2-2)]); */

      invDist[ijkw]  = w==0 && k==2 ? 1.0f / sqrtf(distSqr[ijkw]) : invDist[ijkw-1];
      invDist3[ijkw] = w==0 && k==2 ? invDist[ijkw] * invDist[ijkw] * invDist[ijkw] : invDist3[ijkw-1];

      F[ijkw]    = k==2 ? (j==0 ? 0.0f : F[ijkw-3]) + d[ijkw-3*(2-w)] * invDist3[ijkw] : F[ijkw-3];
      /* F[ijkw]    = k==2 ? (j==0 ? 0.0f : F[ijkw-3]) + d3[w] * invDist3[ijkw] : F[ijkw-3]; */
      vel[i*3+w] = k==2 && j==nBodies-1 ? vel[i*3+w]   + dt*F[ijkw] : vel[i*3+w];

    }

    for (int i = 0 ; i < nBodies*DIM; i+=DIM) { // integrate position
      pos[i]   += vel[i]*dt;
      pos[i+1] += vel[i+1]*dt;
      pos[i+2] += vel[i+2]*dt;
    }
    /* printf("%d: %f\n",nBodies,vx[0]); */
    /* printf("%d: %f\n",nBodies,x[0]); */
  }
#if PROFILING == 1
  GetTimer();
 } // End of loop for time samples
#endif

  output_arr_arg("../../data/pos_out.dat",   pos, nBodies*DIM, dataType);
  output_arr_arg("../../data/vel_out.dat",   vel, nBodies*DIM, dataType);

  free(pos);
  free(vel);
}
