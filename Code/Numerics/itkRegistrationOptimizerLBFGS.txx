/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizerLBFGS.txx
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
RegistrationOptimizerLBFGS<TMetric>
::RegistrationOptimizerLBFGS():
  m_LBFGS( m_MetricCostFunction )
{
}


/**
 * Get the Optimizer
 */
template <class TMetric>
vnl_lbfgs & 
RegistrationOptimizerLBFGS<TMetric>
::GetOptimizer()
{
  return m_LBFGS;
}


} // end namespace itk
