/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizerLevenbergMarquardt.txx
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
RegistrationOptimizerLevenbergMarquardt<TMetric>
::RegistrationOptimizerLevenbergMarquardt():
  m_LevenbergMarquardt( m_MetricCostFunction )
{
}

/**
 * Get the Optimizer
 */
template <class TMetric>
vnl_levenberg_marquardt & 
RegistrationOptimizerLevenbergMarquardt<TMetric>
::GetOptimizer()
{
  return m_LevenbergMarquardt;
}


} // end namespace itk
