/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptimizer.txx
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
Optimizer<TMetric>
::Optimizer()
{
}


/**
 * Set Metric
 */
template <class TMetric>
void
Optimizer<TMetric>
::SetMetric( TMetric * metric ) 
{
  this->m_Metric = metric;
}



/**
 *  Start the optimization process
 */
template <class TMetric>
void
Optimizer<TMetric>
::StartOptimization( void )
{



}



} // end namespace itk
