/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLBFGSOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLBFGSOptimizer_txx
#define _itkLBFGSOptimizer_txx


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
::StartOptimization( void )
{
  InternalParametersType initialParameters( SpaceDimension );
  
  VnlCostFunctionAdaptor::ConvertExternalToInternalParameters( 
                            GetInitialPosition(), 
                            initialParameters     );

  m_LBFGS.minimize( initialParameters );

}



} // end namespace itk

#endif
