/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizerAmoeba.txx
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
RegistrationOptimizerAmoeba<TMetric>
::RegistrationOptimizerAmoeba():
  m_Amoeba( m_MetricCostFunction )
{
}


/**
 * Get the Optimizer
 */
template <class TMetric>
vnl_amoeba & 
RegistrationOptimizerAmoeba<TMetric>
::GetOptimizer()
{
  return m_Amoeba;
}


} // end namespace itk
