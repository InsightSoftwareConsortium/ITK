/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkFEMSolverCrankNicolson_h
#define itkFEMSolverCrankNicolson_h

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
#include <cmath>

namespace itk
{
namespace fem
{
/**
 * \class SolverCrankNicolson
 * \brief FEM Solver for time dependent problems; uses Crank-Nicolson implicit discretization scheme.
 *
 * This is the main class used for solving FEM time-dependent problems.
 * It solves the following problem:
 *
 * \f[
 *      ( M + \alpha*dt* K )*U_t=(M - (1.- \alpha)*dt* K)* U_{t-1} + dt*(\alpha*f_{n+1} + (1-\alpha)*f_n)
 * \f]
 *
 * which is the Crank-Nicolson formulation of the static problem if \f$\alpha=0.5\f$.
 * The static solution is gained if :
 *      \f$\rho = 0.0\f$;   \f$\alpha = 1.0\f$;  \f$dt = 1.0\f$;
 * Practically, it is good to set rho to something small (for the itpack solver).
 * The advantage of choosing \f$\alpha=0.5\f$ is that the solution is then stable for any
 * choice of time step, dt.  This class inherits and uses most of the Solver class
 * functionality.
 *
 * Updated: The calls to to AssembleKandM (or AssembleK) and
 * AssembleFforTimeStep (or AssembleF) are now handled internally
 * by calling Update().
 *
 * FIXME:
 * 1) We should also account for the contribution to the force from essential BCs.
 * Basically there are terms involving  \f$ M * (\dot g_b) \f$  and  \f$ K * g_b \f$
 * where\f$ g_b\f$ is the essential BC vector.
 * \ingroup ITKFEM
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT SolverCrankNicolson : public Solver<TDimension>
{
public:
  typedef SolverCrankNicolson      Self;
  typedef Solver<TDimension>       Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(SolverCrankNicolson, Solver<TDimension> );

  typedef Element::Float Float;

  /** Get/Set the use of the Mass Matrix for the solution. */
  itkSetMacro(UseMassMatrix, bool);
  itkGetMacro(UseMassMatrix, bool);

  /** Get the number of iterations run for the solver. */
  itkGetConstMacro(Iterations, unsigned int);

  /**
   * Reset the number of iterations for the solver. This
   * will prompt the Solver to Assemble the master stiffness
   * and mass matrix again. This is only generated before the
   * first iteration.
   */
  void ResetIterations(void)
  {
    m_Iterations = 0;
  }

  /**
   * Add solution vector u to the corresponding nodal values, which are
   * stored in node objects. This is standard post processing of the solution
   */
  void AddToDisplacements(Float optimum = 1.0);

  void AverageLastTwoDisplacements(Float t = 0.5);

  void ZeroVector(int which = 0);

  void PrintDisplacements();

  void PrintForce();

  /** Get the index for the current solution. */
  itkGetMacro(TotalSolutionIndex, unsigned int);

  /** Get the index for the previous solution. */
  itkGetMacro(SolutionTMinus1Index, unsigned int);

  /** Set stability step for the solution. Initialized to 0.5. */
  itkSetMacro(Alpha, Float);
  itkGetMacro(Alpha, Float);

  /** Set density constant.  */
  itkSetMacro(Rho, Float);
  itkGetMacro(Rho, Float);

  /** Returns the time step used for dynamic problems. */
  virtual Float GetTimeStep(void) const ITK_OVERRIDE
  {
    return m_TimeStep;
  }

  /**
   * Sets the time step used for dynamic problems.
   *
   * \param dt New time step.
   */
  virtual void SetTimeStep(Float dt) ITK_OVERRIDE
  {
    m_TimeStep = dt;
  }

  /** Compute the current state of the right hand side and store the current force
   * for the next iteration.
   */
  void RecomputeForceVector(unsigned int index);

  /** Finds a triplet that brackets the energy minimum. From Numerical
  * Recipes.*/
  void FindBracketingTriplet(Float *a, Float *b, Float *c);

  /** Finds the optimum value between the last two solutions
   * and sets the current solution to that value.  Uses Evaluate Residual;
   */
  Float GoldenSection(Float tol = 0.01, unsigned int MaxIters = 25);

  /* Brents method from Numerical Recipes. */
  Float BrentsMethod(Float tol = 0.01, unsigned int MaxIters = 25);

  Float EvaluateResidual(Float t = 1.0);

  Float GetDeformationEnergy(Float t = 1.0);

  inline Float GSSign(Float a, Float b)
  {
    return b > 0.0 ? std::fabs(a) : -1. * std::fabs(a);
  }
  inline Float GSMax(Float a, Float b)
  {
    return a > b ? a : b;
  }

  void SetEnergyToMin(Float xmin);

  inline LinearSystemWrapper * GetLinearSystem()
  {
    return this->m_LinearSystem;
  }

  Float GetCurrentMaxSolution()
  {
    return m_CurrentMaxSolution;
  }

  /** Compute and print the minimum and maximum of the total solution
   * and the last solution values. */
  void PrintMinMaxOfSolution();

protected:

  /**
  * Default constructor. Sets the indices for the matrix and vector storage.
  * Time step and other parameters are also initialized.
  */
  SolverCrankNicolson();
  ~SolverCrankNicolson() ITK_OVERRIDE {}

  /** Method invoked by the pipeline in order to trigger the computation of
   * the registration. */
  void GenerateData() ITK_OVERRIDE;

  /**
   * Solve for the displacement vector u at a given time.  Update the total solution as well.
   */
  virtual void RunSolver(void) ITK_OVERRIDE;

  /**
   * Helper initialization function before assembly but after generate GFN.
   */
  void InitializeForSolution();

  /**
   * Assemble the master stiffness and mass matrix. We actually assemble
   * the right hand side and left hand side of the implicit scheme equation.
   * MFCs are applied to K.
   */
  void AssembleKandM();

  /**
   * Assemble the master force vector at a given time.
   *
   * \param dim This is a parameter that can be passed to the function and is
                normally used with isotropic elements to specify the
                dimension for which the master force vector should be assembled.
   */
  void AssembleFforTimeStep(int dim = 0);

  Float m_TimeStep;
  Float m_Rho;
  Float m_Alpha;
  Float m_CurrentMaxSolution;

  bool         m_UseMassMatrix;
  unsigned int m_Iterations;

  unsigned int m_ForceTIndex;
  unsigned int m_ForceTotalIndex;
  unsigned int m_ForceTMinus1Index;
  unsigned int m_SolutionTIndex;
  unsigned int m_SolutionTMinus1Index;
  unsigned int m_SolutionVectorTMinus1Index;
  unsigned int m_TotalSolutionIndex;
  unsigned int m_DifferenceMatrixIndex;
  unsigned int m_SumMatrixIndex;
  unsigned int m_DiffMatrixBySolutionTMinus1Index;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SolverCrankNicolson);

};
}
}  // end namespace itk::fem

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMSolverCrankNicolson.hxx"
#endif

#endif // #ifndef itkFEMSolverCrankNicolson_h
