/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMSolverCrankNicolson.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMSolverCrankNicolson_h
#define __itkFEMSolverCrankNicolson_h

#include "itkFEMSolver.h"
#include "itkFEMElementBase.h"
#include "itkFEMMaterialBase.h"
#include "itkFEMLoadBase.h"
#include "itkFEMLinearSystemWrapperVNL.h"

#include "vnl/vnl_sparse_matrix.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector.h"
#include "vnl/algo/vnl_svd.h"
#include "vnl/algo/vnl_cholesky.h"
#include <vnl/vnl_sparse_matrix_linear_system.h>
#include <vxl/vnl/algo/vnl_lsqr.h>
#include <math.h>


namespace itk {
namespace fem {


/**
 * \class SolverCrankNicolson
 * \brief FEM Solver for time dependent problems; uses Crank-Nicolson implicit discretization scheme.
 *
 * This is the main class used for solving FEM time-dependent problems. 
 * It solves the following problem:
 *
 *      ( M + alpha*dt* K )*U_t=(M - (1.- alpha)*dt* K)* U_{t-1} + dt*(alpha*f_{n+1} + (1-alpha)*f_n) 
 *
 * which is the Crank-Nicolson formulation of the static problem if alpha=0.5.  
 * The static solution is gained if :
 *      rho = 0.0;   alpha = 1.0;  dt = 1.0;  
 * Practically, it is good to set rho to something small (for the itpack solver).
 * The advantage of choosing alpha=0.5 is that the solution is then stable for any
 * choice of time step, dt.  This class inherits and uses most of the Solver class
 * functionality.  One must call AssembleKandM instead of AssembleK and 
 * AssembleFforTimeStep instead of AssembleF.  
 * FIXMEs:  1) Members should be privatized, etc.
 * 2) We should also account for the contribution to the force from essential BCs.
 * Basically there are terms involving   M * (\dot g_b)  and   K * g_b  
 * where g_b is the essential BC vector.
 */
class SolverCrankNicolson : public Solver
{
public:

/*
 * helper initialization function before assembly but after generate GFN.
 */
  void InitializeForSolution(); 
  /**
   * Assemble the master stiffness and mass matrix.  We actually assemble
   * the right hand side and left hand side of the implicit scheme equation.
   */  
  void AssembleKandM();            

  /**
   * Assemble the master force vector at a given time.
   *
   * \param dim This is a parameter that can be passed to the function and is
                normally used with isotropic elements to specify the
                dimension for which the master force vector should be assembled.
   */
  void AssembleFforTimeStep(int dim=0);

  /**
   * Solve for the displacement vector u at a given time.  Update the total solution as well.
   */
  void Solve();

  /**
   * add solution vector u to the corresponding nodal values, which are
   * stored in node objects). This is standard post processing of the solution
   */
  void AddToDisplacements(Float optimum=1.0);
  void AverageLastTwoDisplacements(Float t=0.5);
  void ZeroVector(int which=0);
  void PrintDisplacements(); 
  void PrintForce();
  
  inline int GetNGFN() { return NGFN; }

  /** Set stability step for the solution.  */
  inline void SetAlpha(Float a = 0.5) { m_alpha=a; }

  /** Set time step for the solution. Should be 1/2. */
  inline void SetDeltatT(Float T) { m_deltaT=T; }

  /** Set density constant.  */
  inline void SetRho(Float rho) { m_rho=rho;  }

  /** compute the current state of the right hand side and store the current force 
   *  for the next iteration.
   */
  void RecomputeForceVector(unsigned int index);

  /* Finds a triplet that brackets the energy minimum.  From Numerical Recipes.*/
  void FindBracketingTriplet(Float* a,Float* b,Float* c);
  /** Finds the optimum value between the last two solutions 
    * and sets the current solution to that value.  Uses Evaluate Residual;
    */
  void GoldenSection(Float tol=0.01);
  /* Brents method from Numerical Recipes. */
  Float BrentsMethod(Float tol=0.01,unsigned int MaxIters=25);
  Float EvaluateResidual(Float t=1.0);
  inline Float GSSign(Float a,Float b) { return (b > 0.0 ? fabs(a) : -1.*fabs(a)); }
  inline Float GSMax(Float a,Float b) { return (a > b ? a : b); }

  void SetEnergyToMin(Float xmin);
  inline LinearSystemWrapper* GetLS(){ return m_ls;}

   /**
   * Default constructor which sets the indices for the matrix and vector storage.
   * Time step and other parameters are also initialized.
   */
  SolverCrankNicolson() 
  { 
    m_deltaT=0.5; 
    m_rho=1.; 
    m_alpha=0.5;
    // BUG FIXME NOT SURE IF SOLVER IS USING VECTOR INDEX 1 FOR BCs
    ForceTIndex=0;                        // vector
    ForceTMinus1Index=2;                  // vector
    SolutionVectorTMinus1Index=3;         // vector
    DiffMatrixBySolutionTMinus1Index=4;   // vector
    ForceTotalIndex=5;                    // vector
    SolutionTIndex=0;                   // solution
    TotalSolutionIndex=1;               // solution
    SolutionTMinus1Index=2;       // solution
    SumMatrixIndex=0;                   // matrix
    DifferenceMatrixIndex=1;            // matrix
  }

 
  ~SolverCrankNicolson() { }

  Float m_deltaT;
  Float m_rho;
  Float m_alpha;

  unsigned int ForceTIndex;
  unsigned int ForceTotalIndex;
  unsigned int ForceTMinus1Index;
  unsigned int SolutionTIndex;
  unsigned int SolutionTMinus1Index;
  unsigned int SolutionVectorTMinus1Index;
  unsigned int TotalSolutionIndex;
  unsigned int DifferenceMatrixIndex;
  unsigned int SumMatrixIndex;
  unsigned int DiffMatrixBySolutionTMinus1Index;
  
};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMSolverCrankNicolson_h
