/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAmoebaOptimizer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
  m_Amoeba( m_CostFunctionAdaptor )
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

  InternalParametersType initialParameters( SpaceDimension );
  
  VnlCostFunctionAdaptorType::ConvertExternalToInternalParameters( 
                            GetInitialPosition(), 
                            initialParameters     );

  m_Amoeba.minimize( initialParameters );

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
