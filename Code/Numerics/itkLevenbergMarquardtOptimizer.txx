/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevenbergMarquardtOptimizer.txx
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
LevenbergMarquardtOptimizer<TMetric>
::LevenbergMarquardtOptimizer():
  m_LevenbergMarquardt( m_MetricCostFunction )
{
}

/**
 * Get the Optimizer
 */
template <class TMetric>
vnl_levenberg_marquardt & 
LevenbergMarquardtOptimizer<TMetric>
::GetOptimizer()
{
  return m_LevenbergMarquardt;
}


} // end namespace itk
