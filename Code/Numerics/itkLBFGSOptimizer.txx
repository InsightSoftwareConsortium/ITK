/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLBFGSOptimizer.txx
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
LBFGSOptimizer<TMetric>
::LBFGSOptimizer():
  m_LBFGS( m_MetricCostFunction )
{
}


/**
 * Get the Optimizer
 */
template <class TMetric>
vnl_lbfgs & 
LBFGSOptimizer<TMetric>
::GetOptimizer()
{
  return m_LBFGS;
}


} // end namespace itk
