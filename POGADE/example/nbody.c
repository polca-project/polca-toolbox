#include <math.h>
#include <stdio.h>
#include <stdlib.h>

/////////////////////////////////////////////
// Test related params
#define PROFILING       1
#define SAMPLES         1
#define DEBUG           1
// Tolerance for comparing FP results
// due to different rounding in GPU ops.
#define FP_TOLERANCE    1e-3f

// Use case related params
#define DIM           3
#define PROBLEM_SIZE  10
#define DT            0.01f
#define ITERS         10
#define SOFTENING     1e-9f
/////////////////////////////////////////////


typedef struct
{
  float x, y, z;
} Triple;
 
#pragma polca map CALCFORCE pos frc
void bodyForce(Triple *pos, Triple *frc, int n)
{
  for (int i = 0; i < n; i++)
#pragma polca def CALCFORCE
#pragma polca input pos
#pragma polca output frc[i]
    {
      frc[i].x = 0.0f;
      frc[i].y = 0.0f;
      frc[i].z = 0.0f;
 
#pragma polca foldl ADDFORCE zip(frc[i], pos[i]) pos zip(frc[i], pos[i])
      for (int j = 0; j < n; j++)
#pragma polca def ADDFORCE
#pragma polca input pos[j]
#pragma polca inout zip(frc[i], pos[i])
	{
	  float dx = pos[j].x - pos[i].x;
	  float dy = pos[j].y - pos[i].y;
	  float dz = pos[j].z - pos[i].z;
	  float distSqr = dx*dx + dy*dy + dz*dz + SOFTENING;
	  float invDist = 1.0f / sqrtf(distSqr);
	  float invDist3 = invDist * invDist * invDist;
 
	  frc[i].x += dx * invDist3;
	  frc[i].y += dy * invDist3;
	  frc[i].z += dz * invDist3;
        }
    }
}
 
#pragma polca ZipWith UPDV frc vel vel
void velocities(Triple *frc, Triple *vel, float dt, int n)
{
  for (int i = 0; i < n; i++)
#pragma polca def UPDV
#pragma polca input vel[i] frc[i]
#pragma polca output vel[i]
    {
      vel[i].x += dt*frc[i].x;
      vel[i].y += dt*frc[i].y;
      vel[i].z += dt*frc[i].z;
    }
}
 
#pragma polca ZipWith UPDP pos vel pos
void integrate(Triple *pos, Triple *vel, float dt, int n)
{
  for (int i = 0 ; i < n; i++)
#pragma polca def UPDP
#pragma polca input pos[i] vel[i]
#pragma polca output pos[i]
    {
      pos[i].x += vel[i].x*dt;
      pos[i].y += vel[i].y*dt;
      pos[i].z += vel[i].z*dt;
    }
}

void loadData(Triple *ds, int n)
{
  for(int i = 0; i<n; i++)
    {
      ds[i].x = ((float)(rand() % 1000))/10.0f;
      ds[i].y = ((float)(rand() % 1000))/10.0f;
      ds[i].z = ((float)(rand() % 1000))/10.0f;
    }
}

void saveData(Triple *ds, int n)
{
  for(int i = 0; i<n; i++)
    {
      printf("%f - %f - %f\n", ds[i].x, ds[i].y, ds[i].z);
    }
}

int main(const int argc, const char** argv)
{ 
  int nBodies;
 
  const float dt   = DT;    // time step
  const int nIters = ITERS; // simulation iterations

  srand(0);
  
  nBodies = PROBLEM_SIZE;
 
#pragma polca memAlloc (sizeof(Triple)) nBodies pStruct
  Triple *pStruct = (Triple*)malloc(nBodies*sizeof(Triple));
#pragma polca memAlloc (sizeof(Triple)) nBodies vStruct
  Triple *vStruct = (Triple*)malloc(nBodies*sizeof(Triple));
#pragma polca memAlloc (sizeof(Triple)) nBodies fStruct
  Triple *fStruct = (Triple*)malloc(nBodies*sizeof(Triple));
 
  loadData(pStruct, nBodies);
  loadData(vStruct, nBodies);
 
#pragma polca itn MAINLOOP zip(pStruct, vStruct) nIters zip(pStruct, vStruct)
  for (int iter = 0; iter < nIters; iter++)
#pragma polca def MAINLOOP
#pragma polca inout zip(pStruct, vStruct)
    {
      bodyForce(pStruct, fStruct, nBodies); // compute interbody forces
      velocities(fStruct, vStruct, dt, nBodies); // update velocities
      integrate(pStruct, vStruct, dt, nBodies);  // integrate for dt
    }
 
  saveData(pStruct, nBodies);
  saveData(vStruct, nBodies);
 
#pragma polca memFree pStruct
  free(pStruct);
#pragma polca memFree vStruct
  free(vStruct);
#pragma polca memFree fStruct
  free(fStruct);
 
  return 0;
}
