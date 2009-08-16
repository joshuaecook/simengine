// Solvers Header File
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

#ifndef SOLVERS_H
#define SOLVERS_H

#include <simengine_target.h>
#include <stdlib.h>
#include <math.h>
//#include <stdio.h>
//#include <string.h>

// Defines a solver entry point
#define SOLVER(solver, entry, target, type, args...)  \
  JOIN4(solver, entry, target, type)(args)
// Helper macro to allow nested macro expansion of arguments to INTEGRATION_METHOD
#define JOIN4(w, x, y, z) w##_##x##_##y##_##z

// Solver indexing mode for states
#define STATE_IDX TARGET_IDX(mem->props->statesize, mem->props->num_models, i, modelid)

// Pre-declaration of model_flows, the interface between the solver and the model
__DEVICE__ int model_flows(CDATAFORMAT t, const CDATAFORMAT *y, CDATAFORMAT *dydt, CDATAFORMAT *inputs, CDATAFORMAT *outputs, void *ob, unsigned int first_iteration, unsigned int modelid);

// Properties data structure
// ============================================================================================================

typedef struct {
  CDATAFORMAT timestep;
  CDATAFORMAT abstol;
  CDATAFORMAT reltol;
  CDATAFORMAT starttime;
  CDATAFORMAT stoptime;
  CDATAFORMAT *time;
  CDATAFORMAT *model_states;
  CDATAFORMAT *inputs;
  CDATAFORMAT *outputs;
  unsigned int first_iteration;
  unsigned int statesize;
  unsigned int inputsize;
  unsigned int num_models;
  void *ob;
} solver_props;

// Forward Euler data structures and function declarations
// ============================================================================================================

typedef struct {
  solver_props *props;
  CDATAFORMAT *k1;
} forwardeuler_mem;

forwardeuler_mem *SOLVER(forwardeuler, init, TARGET, SIMENGINE_STORAGE, solver_props *props);

int SOLVER(forwardeuler, eval, TARGET, SIMENGINE_STORAGE, forwardeuler_mem *mem, unsigned int modelid);

void SOLVER(forwardeuler, free, TARGET, SIMENGINE_STORAGE, forwardeuler_mem *mem);


// Runga-Kutta (4th order) data structures and function declarations
// ============================================================================================================

typedef struct {
  solver_props *props;
  CDATAFORMAT *k1;
  CDATAFORMAT *k2;
  CDATAFORMAT *k3;
  CDATAFORMAT *k4;
  CDATAFORMAT *temp;
} rk4_mem;

rk4_mem *SOLVER(rk4, init, TARGET, SIMENGINE_STORAGE, solver_props *props);

int SOLVER(rk4, eval, TARGET, SIMENGINE_STORAGE, rk4_mem *mem, unsigned int modelid);

void SOLVER(rk4, free, TARGET, SIMENGINE_STORAGE, rk4_mem *mem);


// Bogacki-Shampine (ode23) data structures and function declarations
// ============================================================================================================

typedef struct {
  solver_props *props;
  CDATAFORMAT *k1;
  CDATAFORMAT *k2;
  CDATAFORMAT *k3;
  CDATAFORMAT *k4;
  CDATAFORMAT *temp;
  CDATAFORMAT *next_states;
  CDATAFORMAT *z_next_states;
  CDATAFORMAT *cur_timestep;
} bogacki_shampine_mem;

bogacki_shampine_mem *SOLVER(bogacki_shampine, init, TARGET, SIMENGINE_STORAGE, solver_props *props);

int SOLVER(bogacki_shampine, eval, TARGET, SIMENGINE_STORAGE, bogacki_shampine_mem *mem, unsigned int modelid);

void SOLVER(bogacki_shampine, free, TARGET, SIMENGINE_STORAGE, bogacki_shampine_mem *mem);


// Dormand-Prince (ode45) data structures and function declarations
// ============================================================================================================

typedef struct {
  solver_props *props;
  CDATAFORMAT *k1;
  CDATAFORMAT *k2;
  CDATAFORMAT *k3;
  CDATAFORMAT *k4;
  CDATAFORMAT *k5;
  CDATAFORMAT *k6;
  CDATAFORMAT *k7;
  CDATAFORMAT *temp;
  CDATAFORMAT *next_states;
  CDATAFORMAT *z_next_states;
  CDATAFORMAT *cur_timestep;
} dormand_prince_mem;

dormand_prince_mem *SOLVER(dormand_prince, init, TARGET, SIMENGINE_STORAGE, solver_props *props);

int SOLVER(dormand_prince, eval, TARGET, SIMENGINE_STORAGE, dormand_prince_mem *mem, unsigned int modelid);

void SOLVER(dormand_prince, free, TARGET, SIMENGINE_STORAGE, dormand_prince_mem *mem);


// CVODE data structures and function declarations
// ============================================================================================================

typedef struct{
  solver_props *props;
  void *cvmem;
  void *y0;
} cvode_mem;

cvode_mem *SOLVER(cvode, init, TARGET, SIMENGINE_STORAGE, solver_props *props);

int SOLVER(cvode, eval, TARGET, SIMENGINE_STORAGE, cvode_mem *mem, unsigned int modelid);

void SOLVER(cvode, free, TARGET, SIMENGINE_STORAGE, cvode_mem *mem);

#endif // SOLVERS_H
