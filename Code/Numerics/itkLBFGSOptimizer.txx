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
template <class TCostFunction>
LBFGSOptimizer<TCostFunction>
::LBFGSOptimizer():
  m_LBFGS( m_CostFunctionAdaptor )
{
}


/**
 * Get the Optimizer
 */
template <class TCostFunction>
vnl_lbfgs & 
LBFGSOptimizer<TCostFunction>
::GetOptimizer()
{
  return m_LBFGS;
}



/**
 * Start the optimization
 */
template <class TCostFunction>
void
LBFGSOptimizer<TCostFunction>
::StartOptimization( VectorType & initialValue )
{
  m_LBFGS.minimize( initialValue );
}



} // end namespace itk
