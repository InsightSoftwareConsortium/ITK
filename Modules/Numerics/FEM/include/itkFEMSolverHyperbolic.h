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

#ifndef itkFEMSolverHyperbolic_h
#define itkFEMSolverHyperbolic_h

#include "itkFEMSolver.h"

namespace itk {
namespace fem {

/**
 * \class SolverHyperbolic
 * \brief Solver class suitable for hyperbolic problems.
 * \ingroup ITKFEM
 *
 * M*ddu + C*du + K*u=F
 */

template <unsigned int TDimension = 3>
class ITK_TEMPLATE_EXPORT SolverHyperbolic : public Solver<TDimension>
{
public:
  typedef SolverHyperbolic         Self;
  typedef Solver<TDimension>       Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SolverHyperbolic, Solver<TDimension> );

  typedef Element::Float Float;

  /** Get/Set Gamma. */
  itkSetMacro(Gamma, Float);
  itkGetMacro(Gamma, Float);

  /** Get/Set Beta. */
  itkSetMacro(Beta, Float);
  itkGetMacro(Beta, Float);

  /** Get/Set Number of Iterations. */
  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetMacro(NumberOfIterations, unsigned int);

  /** Returns the time step used for dynamic problems. */
  virtual Float GetTimeStep(void) const ITK_OVERRIDE
  {
    return this->m_TimeStep;
  }

  /**
   * Sets the time step used for the problems.
   *
   * \param dt New time step.
   */
  virtual void SetTimeStep(Float dt) ITK_OVERRIDE
  {
    this->m_TimeStep = dt;
  }

protected:
  SolverHyperbolic();
  virtual ~SolverHyperbolic() ITK_OVERRIDE {}
  void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /** Initialize the linear system wrapper. */
  virtual void InitializeLinearSystemWrapper(void) ITK_OVERRIDE;

  /**
   * When assembling the element matrix into master matrix, we
   * need to assemble the mass matrix too.
   */
  virtual void AssembleElementMatrix(Element::Pointer e) ITK_OVERRIDE;

  /** Initialize the storage for all master matrices. */
  virtual void InitializeMatrixForAssembly(unsigned int N) ITK_OVERRIDE;

  /**
   * Combine the M, C and K matrices into one big system of linear
   * equations.
   */
  virtual void FinalizeMatrixAfterAssembly( void ) ITK_OVERRIDE;


  /** Method invoked by the pipeline in order to trigger the computation. */
  void  GenerateData() ITK_OVERRIDE;

  /** Solve for the displacement vector u at a given time.
  * Update the total solution as well. */
  virtual void RunSolver(void) ITK_OVERRIDE;

  /** Solve for the displacement vector u for one iteration. */
  void Solve();

  /** Constants that specify where matrices are strored. */
  enum { matrix_K=1, matrix_M=2, matrix_C=3, matrix_tmp=4 };

  /** Constants that specify where vectors are strored. */
  enum { solution_d=0, solution_v=1, solution_a=2};
  enum { vector_dhat=2, vector_vhat=3, vector_ahat=4, vector_tmp=5 };

  Float          m_TimeStep;
  Float          m_Gamma;
  Float          m_Beta;
  unsigned int   m_NumberOfIterations;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SolverHyperbolic);
};

} // end namespace fem
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMSolverHyperbolic.hxx"
#endif


#endif // #ifndef itkFEMSolverHyperbolic_h
