/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOnePlusOneEvolutionaryOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __ONEPLUSONEEVOLUTIONARYOPTIMIZER_TXX
#define __ONEPLUSONEEVOLUTIONARYOPTIMIZER_TXX

#include <cmath>

#include "itkOnePlusOneEvolutionaryOptimizer.h"

namespace itk
{

template<class TCostFunction, class TNormalRandomVariateGenerator>
OnePlusOneEvolutionaryOptimizer<TCostFunction, TNormalRandomVariateGenerator>
::OnePlusOneEvolutionaryOptimizer()
{
  m_CostFunction = 0 ;
  m_SpaceDimension = 0 ;
  m_Initialized = false ;
  m_Epsilon = (double) 1e-6  ; 
  m_RandomSeed = 0 ;
  m_VerboseMode = false ;
}

template<class TCostFunction, class TNormalRandomVariateGenerator>
OnePlusOneEvolutionaryOptimizer<TCostFunction, TNormalRandomVariateGenerator>
::~OnePlusOneEvolutionaryOptimizer() 
{
}


template<class TCostFunction, class TNormalRandomVariateGenerator>
void 
OnePlusOneEvolutionaryOptimizer<TCostFunction, TNormalRandomVariateGenerator>
::Initialize(double initialRadius, double grow, double shrink) 
{
  m_MaximumIteration = 20 ;
  m_InitialRadius = initialRadius ;

  if (grow == -1)
    m_GrowFactor = 1.05 ;
  else
    m_GrowFactor = grow ;

  if (shrink == -1)
    m_ShrinkFactor = pow(m_GrowFactor, -0.25) ;
  else
    m_ShrinkFactor = shrink ;

  m_RandomGenerator = NormalRandomVariateGeneratorType::New() ;
  m_Initialized = true ;
}


template<class TCostFunction, class TNormalRandomVariateGenerator>
void
OnePlusOneEvolutionaryOptimizer<TCostFunction, TNormalRandomVariateGenerator>
::SetCostFunction(TCostFunction* energy)
{
  m_CostFunction = energy ;
}


template<class TCostFunction, class TNormalRandomVariateGenerator>
void
OnePlusOneEvolutionaryOptimizer<TCostFunction, TNormalRandomVariateGenerator>
::SetMaximumIteration(int maxIter)
{
  m_MaximumIteration = maxIter ;
}


template<class TCostFunction, class TNormalRandomVariateGenerator>
void
OnePlusOneEvolutionaryOptimizer<TCostFunction, TNormalRandomVariateGenerator>
::SetSpaceDimension(int dimension)
{
  m_SpaceDimension = dimension ;
}

template<class TCostFunction, class TNormalRandomVariateGenerator>
void
OnePlusOneEvolutionaryOptimizer<TCostFunction, TNormalRandomVariateGenerator>
::SetEpsilon(double epsilon)
{
  m_Epsilon = epsilon ;
}

template<class TCostFunction, class TNormalRandomVariateGenerator>
void
OnePlusOneEvolutionaryOptimizer<TCostFunction, TNormalRandomVariateGenerator>
::SetRandomSeed(long seed)
{
  m_RandomSeed = seed ;
}

template<class TCostFunction, class TNormalRandomVariateGenerator>
void
OnePlusOneEvolutionaryOptimizer<TCostFunction, TNormalRandomVariateGenerator>
::SetVerboseMode(bool flag)
{
  m_VerboseMode = flag ;
}

template<class TCostFunction, class TNormalRandomVariateGenerator>
void
OnePlusOneEvolutionaryOptimizer<TCostFunction, TNormalRandomVariateGenerator>
::Run()
  throw (ExceptionObject)
{
  if (m_CostFunction == 0 || !m_Initialized)
    {
      throw ExceptionObject() ;
    }

  // m_Random Seed was originally getpid()

  if (m_RandomSeed == 0)
    m_RandomGenerator->Initialize((long) rand()) ; 
  else
    m_RandomGenerator->Initialize(m_RandomSeed) ; 
  
  int minIteration = 0 ;
  
  double pvalue, cvalue, adjust ;

  int spaceDimension ;

  if (m_SpaceDimension == 0)
    spaceDimension = SpaceDimension ;
  else
    spaceDimension = m_SpaceDimension ;

  vnl_matrix<double> A(spaceDimension, spaceDimension, 0) ;
  vnl_vector<double> parent(this->GetInitialPosition()) ; 

  vnl_vector<double> f_norm(spaceDimension) ;
  vnl_vector<double> child(spaceDimension) ;
  vnl_vector<double> delta(spaceDimension) ;

  for (int i = 0  ; i < spaceDimension ; i++) 
    {
        A(i,i) = m_InitialRadius ;
    }
  //m_BiasField->SetCoefficients(parent) ;

  pvalue = m_CostFunction->GetValue(parent, m_Value) ;

  this->SetCurrentPosition(parent) ;

  int iter ;
  for (iter = 0 ; iter < m_MaximumIteration ; iter++) 
    {
      for (int i=0 ; i < spaceDimension ; i++) 
        {
          f_norm[i] = m_RandomGenerator->GetNormalVariate() ;
        }

      delta  = A * f_norm ;
      child  = parent + delta ;
      cvalue = m_CostFunction->GetValue(child, m_Value) ;
      if (m_VerboseMode)
        {
          std::cout << iter << ": parent: " << pvalue 
                << " child: "<< cvalue << std::endl ;
        }

      if (cvalue < pvalue) 
        {
          minIteration = iter ;
          pvalue = cvalue ;
          
          parent.swap(child) ;                  
          
          adjust = m_GrowFactor ; 
          this->SetCurrentPosition(parent) ;
          
        } 
      else 
        {
          adjust = m_ShrinkFactor ;
        }
      
      // convergence criterion: f-norm of A < epsilon_A
      // Compute double precision sum of absolute values of 
      // a single precision vector
      if (m_VerboseMode)
        {
          if (A.fro_norm() <= m_Epsilon) 
            {
              std::cout << "A f-norm:" << A.fro_norm() << std::endl ;
              break ;
            }
        }
      
      // A += (adjust - 1)/ (f_norm * f_norm) * A * f_norm * f_norm ;
      // Blas_R1_Update(A, A * f_norm, f_norm, 
      //             ((adjust - 1) / Blas_Dot_Prod(f_norm, f_norm)));    
      // = DGER(Fortran)
      //   performs the rank 1 operation
      // A := alpha*x*y' + A,
      // where y' = transpose(y)
      // where alpha is a scalar, x is an m element vector, y is an n element
      // vector and A is an m by n matrix.
      // x = A * f_norm , y = f_norm, alpha = (adjust - 1) / Blas_Dot_Prod(
      // f_norm, f_norm)
      A = A + (adjust - 1.0) * A ;
    }
}

} // end of namespace itk
#endif
