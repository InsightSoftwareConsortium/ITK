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
#ifndef __itkFEMSolverHyperbolic_h
#define __itkFEMSolverHyperbolic_h

#include "itkFEMSolver.h"

namespace itk {
namespace fem {

/**
 * \class SolverHyperbolic
 * \brief Solver class suitable for hyperbolic problems.
 *
 * M*ddu + C*du + K*u=F
 *
 * \ingroup ITK-FEM
 */
class SolverHyperbolic : public Solver
{
public:

  /**
   * Default constructor
   */
  SolverHyperbolic();

  /**
   * Initialize the linear system wrapper.
   */
  virtual void InitializeLinearSystemWrapper(void);

  /**
   * When assembling the element matrix into master matrix, we
   * need to assemble the mass matrix too.
   */
  virtual void AssembleElementMatrix(Element::Pointer e);

  /**
   * Initializes the storasge for all master matrices.
   */
  virtual void InitializeMatrixForAssembly(unsigned int N);

  /**
   * Combines the M, C and K matrices into one big system of linear
   * equations.
   */
  virtual void FinalizeMatrixAfterAssembly( void );

  /**
   * Solves the system for the next time step.
   */
  virtual void Solve( void );

  virtual Float GetTimeStep( void ) const { return m_deltaT; }
  virtual void SetTimeStep(Float dt) { this->m_deltaT=dt; }

  /**
   * Constants that specify, where matrices are strored.
   */
  enum { matrix_K=1, matrix_M=2, matrix_C=3, matrix_tmp=4 };

  /**
   * Constants that specify, where vectors are strored.
   */
  enum { solution_d=0, solution_v=1, solution_a=2};
  enum { vector_dhat=2, vector_vhat=3, vector_ahat=4, vector_tmp=5 };

  Float m_gamma;
  Float m_beta;
  Float m_deltaT;

};

}} // end namespace itk::fem

#endif // #ifndef __itkFEMSolverHyperbolic_h
