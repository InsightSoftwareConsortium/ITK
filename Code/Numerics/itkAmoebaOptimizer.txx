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
#ifndef _itkAmoebaOptimizer_txx
#define _itkAmoebaOptimizer_txx


namespace itk
{

/**
 * Constructor
 */
template <class TCostFunction >
AmoebaOptimizer<TCostFunction>
::AmoebaOptimizer():
  m_Amoeba( m_CostFunction )
{
}


/**
 * Start the optimization
 */
template <class TCostFunction>
void
AmoebaOptimizer<TCostFunction>
::StartOptimization( void )
{
  InternalParametersType initialValue( SpaceDimension );

  VnlCostFunctionAdaptor::ConvertParameters( 
                             GetInitialPosition(), 
                             initialValue );

  m_Amoeba.minimize( initialValue );
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

#endif
