/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConjugateGradientOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkConjugateGradientOptimizer_txx
#define _itkConjugateGradientOptimizer_txx

#include "itkConjugateGradientOptimizer.h"

namespace itk
{

/**
 * Constructor
 */
template <class TCostFunction>
ConjugateGradientOptimizer<TCostFunction>
::ConjugateGradientOptimizer():
  m_ConjugateGradient( m_CostFunctionAdaptor )
{
}


/**
 * Get the Optimizer
 */
template <class TCostFunction>
vnl_conjugate_gradient & 
ConjugateGradientOptimizer<TCostFunction>
::GetOptimizer()
{
  return m_ConjugateGradient;
}



/**
 * Start the optimization
 */
template <class TCostFunction>
void
ConjugateGradientOptimizer<TCostFunction>
::StartOptimization( const ParametersType &  initialValue )
{
  InternalParametersType initialParameters( SpaceDimension );
  VnlCostFunctionAdaptor::ConvertParameters( initialValue, initialParameters );
  m_ConjugateGradient.minimize( initialParameters );
}


} // end namespace itk

#endif
