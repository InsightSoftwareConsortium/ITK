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


namespace itk
{

/**
 * Constructor
 */
template <class TCostFunction>
ConjugateGradientOptimizer<TCostFunction>
::ConjugateGradientOptimizer():
  m_ConjugateGradient( m_CostFunction )
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


} // end namespace itk
