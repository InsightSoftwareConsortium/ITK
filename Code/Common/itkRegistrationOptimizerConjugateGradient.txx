/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizerConjugateGradient.txx
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
template <class TMetric>
RegistrationOptimizerConjugateGradient<TMetric>
::RegistrationOptimizerConjugateGradient():
  m_ConjugateGradient( m_MetricCostFunction )
{
}


/**
 * Get the Optimizer
 */
template <class TMetric>
vnl_conjugate_gradient & 
RegistrationOptimizerConjugateGradient<TMetric>
::GetOptimizer()
{
  return m_ConjugateGradient;
}


} // end namespace itk
