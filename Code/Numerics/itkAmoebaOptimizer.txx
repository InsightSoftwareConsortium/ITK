/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizer.txx
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
AmoebaOptimizer<TMetric>
::AmoebaOptimizer():
  m_Amoeba( m_MetricCostFunction )
{
}


/**
 * Get the Optimizer
 */
template <class TMetric>
vnl_amoeba & 
AmoebaOptimizer<TMetric>
::GetOptimizer()
{
  return m_Amoeba;
}


} // end namespace itk
