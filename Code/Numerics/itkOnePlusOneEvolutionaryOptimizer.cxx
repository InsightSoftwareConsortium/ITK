/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOnePlusOneEvolutionaryOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkOnePlusOneEvolutionaryOptimizer_cxx
#define _itkOnePlusOneEvolutionaryOptimizer_cxx

#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "vcl/vcl_cmath.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

OnePlusOneEvolutionaryOptimizer
::OnePlusOneEvolutionaryOptimizer()
{
  m_Maximize = false;
  m_Epsilon = (double) 1.5e-4  ; 
  m_RandomGenerator = 0 ;

  m_Initialized = false ;
  m_GrowthFactor = 1.05 ;
  m_ShrinkFactor = pow(m_GrowthFactor, -0.25) ;
  m_InitialRadius = 1.01 ;
  m_MaximumIteration = 100 ;
  m_Stop = false ;
  m_CurrentCost = 0;
  m_CurrentIteration = 0;
}

OnePlusOneEvolutionaryOptimizer
::~OnePlusOneEvolutionaryOptimizer() 
{
}

void 
OnePlusOneEvolutionaryOptimizer
::SetNormalVariateGenerator(NormalVariateGeneratorType* generator)
{
  if ( m_RandomGenerator != generator )
    {
      m_RandomGenerator = generator ;
      this->Modified() ;
    }
}

void 
OnePlusOneEvolutionaryOptimizer
::Initialize(double initialRadius, double grow, double shrink) 
{
  m_MaximumIteration = 20 ;
  m_InitialRadius = initialRadius ;

  if (grow == -1)
    m_GrowthFactor = 1.05 ;
  else
    m_GrowthFactor = grow ;

  if (shrink == -1)
    m_ShrinkFactor = pow(m_GrowthFactor, -0.25) ;
  else
    m_ShrinkFactor = shrink ;
}

void
OnePlusOneEvolutionaryOptimizer
::StartOptimization()
{
  
  if( m_CostFunction.IsNull() )
    {
      return ;
    }

  this->InvokeEvent( StartEvent() );
  m_Stop = false ;

  const unsigned int spaceDimension = m_CostFunction->GetNumberOfParameters();

  vnl_matrix<double> A(spaceDimension, spaceDimension, 0);
  vnl_vector<double> parent(this->GetInitialPosition()); 

  vnl_vector<double> f_norm(spaceDimension);
  vnl_vector<double> child(spaceDimension);
  vnl_vector<double> delta(spaceDimension);
  
  vnl_vector<double> scaledDelta(spaceDimension) ;

  ScalesType scales = this->GetScales() ;

  for(unsigned int i = 0  ; i < spaceDimension ; i++) 
    {
      A(i,i) = m_InitialRadius / scales[i] ;
    }
  //m_BiasField->SetCoefficients(parent) ;


  ParametersType parentPosition( spaceDimension );
  for( unsigned int i=0; i<spaceDimension; i++)
    {
      parentPosition[i] = parent[i];
    }

  itkDebugMacro(<< ": initial position: " << parentPosition) ; 
  double pvalue = m_CostFunction->GetValue(parentPosition);
  this->SetCurrentPosition(parentPosition) ;
  m_CurrentIteration = 0 ;
  for (unsigned int iter = 0 ; iter < m_MaximumIteration ; iter++) 
    {
      if ( m_Stop )
        {
          break ;
        }

      ++m_CurrentIteration ;

      for (unsigned int i=0 ; i < spaceDimension ; i++) 
        {
          f_norm[i] = m_RandomGenerator->GetVariate() ;
        }

      delta  = A * f_norm ;

      child  = parent + delta ;

      ParametersType childPosition( spaceDimension );
      for( unsigned int i=0; i<spaceDimension; i++)
        {
          childPosition[i] = child[i];
        }
      double cvalue = m_CostFunction->GetValue(childPosition);

      itkDebugMacro(<< iter << ": parent: " << pvalue 
                    << " child: "<< cvalue );

      double adjust = m_ShrinkFactor ;
      if(m_Maximize)
      {
        if (cvalue > pvalue) 
        {
          pvalue = cvalue ;
          
          parent.swap(child) ;                  
          
          adjust = m_GrowthFactor ; 
          for( unsigned int i=0; i<spaceDimension; i++)
            {
              parentPosition[i] = parent[i];
            }
          this->SetCurrentPosition(parentPosition) ;
          
        } 
      }
      else
      {
        if (cvalue < pvalue) 
        {
          pvalue = cvalue ;
          
          parent.swap(child) ;                  
          
          adjust = m_GrowthFactor ; 
          for( unsigned int i=0; i<spaceDimension; i++)
            {
              parentPosition[i] = parent[i];
            }
          this->SetCurrentPosition(parentPosition) ;
          
        } 
      }
     

      m_CurrentCost = pvalue ;
      // convergence criterion: f-norm of A < epsilon_A
      // Compute double precision sum of absolute values of 
      // a single precision vector
      double fro_norm = A.fro_norm() ;
      itkDebugMacro(<< "A f-norm:" << fro_norm);
      if (fro_norm <= m_Epsilon) 
        {
          itkDebugMacro(<< "converges at iteration = " << iter) ;
          break ;
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

      //A = A + (adjust - 1.0) * A ;
      double alpha = ((adjust - 1.0) / dot_product(f_norm, f_norm)) ;
      double temp ; 
      for ( unsigned int c = 0 ; c < spaceDimension ; c++ )
        {
          if ( f_norm[c] != 0.0 )
            { 
              temp = alpha * f_norm[c] ;
            }

          for (unsigned int r = 0 ; r < spaceDimension ; r++)
            {
              A(c, r) += alpha * delta[r] * temp ;
            }
        }
      //      A = A + tempA ;

//       double temp = 0.0 ;
//       for (unsigned int r = 0 ; r < spaceDimension ; r++)
//         {
//           temp += x[r] * y[r] ;
//         }
//       A = alpha*temp + A ;
//       itkDebugMacro( << " Weight matrix: " << std::endl << A) ;
      this->InvokeEvent( IterationEvent() );   
      itkDebugMacro(<< "Current position: " << this->GetCurrentPosition()) ;
    }
  this->InvokeEvent( EndEvent() );
  
}
 


/**
 *
 */
void
OnePlusOneEvolutionaryOptimizer
::PrintSelf(std::ostream& os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);

  os << indent << "Random Generator  " << m_RandomGenerator  << std::endl;
  os << indent << "Maximum Iteration " << m_MaximumIteration << std::endl;
  os << indent << "Epsilon           " << m_Epsilon          << std::endl;
  os << indent << "Initial Radius    " << m_InitialRadius    << std::endl;
  os << indent << "Growth Fractor    " << m_GrowthFactor     << std::endl;
  os << indent << "Shrink Fractor    " << m_ShrinkFactor     << std::endl;
  os << indent << "Initialized       " << m_Initialized      << std::endl;
  os << indent << "Current Cost      " << m_CurrentCost << std::endl;
  os << indent << "Current Iteration " << m_CurrentIteration << std::endl;

}

} // end of namespace itk
#endif
