/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationOptimizer.txx
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
RegistrationOptimizer<TMetric>
::RegistrationOptimizer()
{
}


/**
 * Set Metric
 */
template <class TMetric>
void
RegistrationOptimizer<TMetric>
::SetMetric( TMetric * metric ) 
{
  this->m_Metric = metric;
}



/**
 *  Start the optimization process
 */
template <class TMetric>
void
RegistrationOptimizer<TMetric>
::StartOptimization( void )
{



}



} // end namespace itk
