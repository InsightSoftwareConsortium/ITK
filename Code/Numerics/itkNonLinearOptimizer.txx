/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNonLinearOptimizer.txx
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
NonLinearOptimizer<TMetric>
::NonLinearOptimizer()
{
}


/**
 * Set Metric
 */
template <class TMetric>
void
NonLinearOptimizer<TMetric>
::SetMetric( TMetric * metric ) 
{
  this->m_MetricCostFunction.SetMetric( metric );
}



} // end namespace itk
