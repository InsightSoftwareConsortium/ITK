/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageAffineMutualInformationGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToImageAffineMutualInformationGradientDescentRegistration_txx
#define _itkImageToImageAffineMutualInformationGradientDescentRegistration_txx

#include "itkImageToImageAffineMutualInformationGradientDescentRegistration.h"


namespace itk
{

/*
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference, TTarget>
::ImageToImageAffineMutualInformationGradientDescentRegistration()
{

  // initialize the parameter to be the identity transform
  typename ParametersType::Iterator pit = m_Parameters.Begin();

  // initialize the linear part
  for (unsigned int i=0; i<TReference::ImageDimension; i++)
    {
    for (unsigned int j=0; j<TReference::ImageDimension; j++)
      {
      *pit = 0;
      if(i == j)
        {
        *pit = 1;
        }
      ++pit;
      }
    }

  // initialize the offset part
  for (unsigned int i=0; i<TReference::ImageDimension; i++)
    {
    *pit = 0;
    ++pit;
    }
  
  // set default parameters
  m_NumberOfIterations = 1000;
  m_LearningRate = 1.0;
  
}


/*
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageAffineMutualInformationGradientDescentRegistration()
{
}


template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference,  TTarget>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Learning rate: " << m_LearningRate << std::endl;
  os << indent << "No. iterations: " << m_NumberOfIterations << std::endl;
}


/*
 * Starts the Registration Process
 */
template <class TReference, class TTarget>
void
ImageToImageAffineMutualInformationGradientDescentRegistration<TReference, TTarget>
::StartRegistration( void )
{


  typename OptimizerType::Pointer optimizer;
  optimizer = this->GetOptimizer();

  optimizer->SetCostFunction( this->GetMetric() );

  // setup the optimizer
  optimizer->MaximizeOn();
  optimizer->SetLearningRate( m_LearningRate );
  optimizer->SetNumberOfIterations( m_NumberOfIterations );
  optimizer->SetInitialPosition( m_Parameters );

  try
    {
    // do the optimization
    optimizer->StartOptimization();
    }
  catch( ExceptionObject& err )
    {
      // An error has occurred in the optimization.
      // Update the parameters
      m_Parameters = optimizer->GetCurrentPosition();

      // Pass exception to caller
      throw err;
    }


  // get the results
  m_Parameters = optimizer->GetCurrentPosition();
  

}


} // end namespace itk


#endif
