// Solvers Header File
// Copyright 2009 Simatra Modeling Technologies, L.L.C.

#ifndef SOLVERS_H
#define SOLVERS_H

// Common definitions
#define FALSE 0
#define TRUE 1
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))


// Properties data structure
// ============================================================================================================

//typedef int (*flowptr)(CDATAFORMAT t, const CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT*, CDATAFORMAT* , int);

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
  int first_iteration;
  int statesize;
} solver_props;

// Forward Euler data structures and function declarations
// ============================================================================================================

typedef struct {
  solver_props *props;
  CDATAFORMAT *k1;
} forwardeuler_mem;

forwardeuler_mem *forwardeuler_init(solver_props *props);

int forwardeuler_eval(forwardeuler_mem *mem);

void forwardeuler_free(forwardeuler_mem *mem);


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

rk4_mem *rk4_init(solver_props *props);

int rk4_eval(rk4_mem *mem);

void rk4_free(rk4_mem *mem);


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
} bogacki_shampine_mem;

bogacki_shampine_mem *bogacki_shampine_init(solver_props *props);

int bogacki_shampine_eval(bogacki_shampine_mem *mem);

void bogacki_shampine_free(bogacki_shampine_mem *mem);


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
} dormand_prince_mem;

dormand_prince_mem *dormand_prince_init(solver_props *props);

int dormand_prince_eval(dormand_prince_mem *mem);

void dormand_prince_free(dormand_prince_mem *mem);


// CVODE data structures and function declarations
// ============================================================================================================
/*
typedef struct{
  solver_props *props;
  void *cvmem;
  void *y0;
} cvode_mem;

cvode_mem *cvode_init(solver_props *props);

int cvode_eval(cvode_mem *mem);

void cvode_free(cvode_mem *mem);
*/
#endif // SOLVERS_H
