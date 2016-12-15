// FUNC_ANALYZ: main BLOCK_ABS
// FEAT_VECTOR: [-1, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 3, 0]
// TEST_VECTOR: [-1, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 3, 0]
// TEST_LABEL: 0 (CPU)

//# maxForStmtDepth:            -1
//# anyFuncCall:                 3
//# anyArrayWriteShifted:        0
//# numIrregularForLoops:        0
//# usesGlobalVars:              0
//# anyIfStmt:                   0
//# allForLoopWithStaticLimit:   1
//# anySIMDloop:                 0
//# anyLoop_Schedule:            0
//# numLoopInvVar:               0
//# numLoopHoistedVarMods:       0
//# numNon1Darray:               0
//# numAuxVarArrayIndex:         0
//# totalNumForLoops:            0
//# numNonNormalizedForLoops:    0
//# numStmtsRollUp:              0
//# numCompoundStmts:            1
//# anyTernaryOp:                0
//# anyUselessStmt:              0
//# numForPostambles:            0
//# numForPreambles:             0
//# numStructVarDecl:            3
//# numEmptyIf:                  0



// This is an adapted version of a code obtained from: https://github.com/harrism/mini-nbody

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "timer.h"
#include "params.h"
#define SOFTENING     1e-9f

typedef struct Triple Triple;

struct Triple { float x, y, z; };

// The original bodyForce traversed pos (the bodies' positions) and
// modified vel (the bodies' velocities), but this hardly fits any
// sensible definition for a map, so we refactorize the code so as to
// just obtain forces (anew) from positions...
void bodyForce(struct Triple *pos, struct Triple *frc, int n) {
  int i,j;
  float d[3];
  float distSqr,invDist,invDist3;
  #pragma polca map CALCFORCE pos frc
  for(i = 0; i < n; i++) {
    #pragma polca def CALCFORCE
    #pragma polca input pos[i]
    #pragma polca output frc[i]
    {
      // we want the inner loop to perform a fold, so the neutral
      // element should be explicit... 
      // we initialize frc[i] to be the null vector   
      frc[i].x = 0.0f;
      frc[i].y = 0.0f;
      frc[i].z = 0.0f;

      #pragma polca fold ADDFORCE frc[i] pos frc[i]
      for(j = 0; j < n; j++) {
        #pragma polca def ADDFORCE
        #pragma polca input frc[i] pos[j]
        #pragma polca output frc[i] 
	{
	  d[0] = pos[j].x - pos[i].x;
	  d[1] = pos[j].y - pos[i].y;
	  d[2] = pos[j].z - pos[i].z;
	  distSqr = d[0]*d[0] + d[1]*d[1] + d[2]*d[2] + SOFTENING;
	  invDist = 1.0f / sqrt(distSqr);
	  invDist3 = invDist * invDist * invDist;

	  frc[i].x = frc[i].x + d[0] * invDist3;
	  frc[i].y = frc[i].y + d[1] * invDist3;
	  frc[i].z = frc[i].z + d[2] * invDist3;
        }
      }
    }
  }
}  

// velocities updates an array of velocity vectors using an array of
// forces and a time lapse.
// As mass is 1, force and accelleration are the same as absolute
// values.
void velocities(struct Triple *frc, struct Triple *vel, float dt, int n) {
  int i;
#pragma polca zipWith UPDV vel frc
  for(i = 0; i < n; i++) {
#pragma polca def UPDV
#pragma polca input vel[i] frc[i]
#pragma polca output vel[i]
    {
    vel[i].x = vel[i].x + dt*frc[i].x;
    vel[i].y = vel[i].y + dt*frc[i].y;
    vel[i].z = vel[i].z + dt*frc[i].z;
    }
  }
}

// integrate updates the array of position vectors using the array of
// velocities and a time lapse.
void integrate(struct Triple *pos, struct Triple *vel, float dt, int n) {
    int i;
#pragma polca zipWith UPDP pos vel
    for(i = 0 ; i < n; i++) { // integrate position
#pragma polca def UPDP
#pragma polca input pos[i] vel[i]
#pragma polca output pos[i]
      {      
      pos[i].x = pos[i].x + vel[i].x*dt;
      pos[i].y = pos[i].y + vel[i].y*dt;
      pos[i].z = pos[i].z + vel[i].z*dt;
      }
    }

}


int main(const int argc, const char** argv) {
  
  int nBodies,nElems;
  int dataType;
  int i,iter;

  const float dt   = DT;    // time step
  const int nIters = ITERS; // simulation iterations
  
  float *pos, *vel;

  char fileName[80];
  sprintf(fileName, "%s/pos_input.dat",DATA_PATH);
  input_arr_arg(fileName,&pos,&nElems,&dataType);
  sprintf(fileName, "%s/vel_input.dat",DATA_PATH);
  input_arr_arg(fileName,&vel,&nElems,&dataType);

  nBodies = nElems / DIM;
  struct Triple pStruct[N];
  struct Triple vStruct[N];
  struct Triple fStruct[N];
  //code for initializing linearized arrays into struct-based arrays
  for(i=0;i<nBodies;i++) {
	pStruct[i].x = pos[i*3];
	pStruct[i].y = pos[i*3+1];
	pStruct[i].z = pos[i*3+2];
  }
  for(i=0;i<nBodies;i++) {
	vStruct[i].x = vel[i*3];
	vStruct[i].y = vel[i*3+1];
	vStruct[i].z = vel[i*3+2];
  }

#if PROFILING == 1
 for(int sample=0;sample<SAMPLES;sample++){
    StartTimer();
#endif
#pragma polca itn UPD_FOR_VEL_POS (pStruct,fStruct) nIters (pStruct,fStruct)
  for(iter = 1; iter <= nIters; iter++) {
#pragma polca def UPD_FOR_VEL_POS
#pragma polca input (pStruct,fStruct)
#pragma polca output (pStruct,fStruct)
#pragma polca adapt maxj
#pragma polca def BLOCK_ABS
    {
    bodyForce(pStruct, fStruct, nBodies); // compute interbody forces
    velocities(fStruct, vStruct, dt, nBodies); // update velocities
    integrate(pStruct, vStruct, dt, nBodies);  // integrate for dt
    }
  }
#if PROFILING == 1
  GetTimer();
 } // End of loop for time samples
#endif

  //code for copy back struct-based arrays into linearized arrays
  for(i=0;i<nBodies;i++) {
    pos[i*3]   = pStruct[i].x;
    pos[i*3+1] = pStruct[i].y;
    pos[i*3+2] = pStruct[i].z;
  }
  for(i=0;i<nBodies;i++) {
    vel[i*3]   = vStruct[i].x;
    vel[i*3+1] = vStruct[i].y;
    vel[i*3+2] = vStruct[i].z;
  }

  sprintf(fileName, "%s/pos_out.dat",DATA_PATH);
  output_arr_arg(fileName,pos,nBodies * DIM,dataType);
  sprintf(fileName, "%s/vel_out.dat",DATA_PATH);
  output_arr_arg(fileName,vel,nBodies * DIM,dataType);

  free(pos);
  free(vel);

}
