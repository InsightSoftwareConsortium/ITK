/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizerNonLinear.txx
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
RegistrationOptimizerNonLinear<TMetric>
::RegistrationOptimizerNonLinear()
{
}


/**
 * Set Metric
 */
template <class TMetric>
void
RegistrationOptimizerNonLinear<TMetric>
::SetMetric( TMetric * metric ) 
{
  this->m_MetricCostFunction.SetMetric( metric );
}



} // end namespace itk
