/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageRigidMutualInformationGradientDescentRegistration.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToImageRigidMutualInformationGradientDescentRegistration_txx
#define _itkImageToImageRigidMutualInformationGradientDescentRegistration_txx

#ifdef COMPILE_LONG_NAMES_CLASSES

#include "itkImageToImageRigidMutualInformationGradientDescentRegistration.h"


namespace itk
{

/*
 * Constructor
 */
template <class TReference, class TTarget>
ImageToImageRigidMutualInformationGradientDescentRegistration<TReference, TTarget>
::ImageToImageRigidMutualInformationGradientDescentRegistration()
{

  // initialize the parameter to be the identity transform
  m_Parameters.Fill( 0.0 );
  m_Parameters[3] = 1.0;

  // set default parameters
  m_NumberOfIterations = 1000;
  m_LearningRate = 1.0;

}

/*
 * Destructor
 */
template <class TReference, class TTarget>
ImageToImageRigidMutualInformationGradientDescentRegistration<TReference,  TTarget>
::~ImageToImageRigidMutualInformationGradientDescentRegistration()
{
}


template <class TReference, class TTarget>
void
ImageToImageRigidMutualInformationGradientDescentRegistration<TReference,  TTarget>
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
int
ImageToImageRigidMutualInformationGradientDescentRegistration<TReference, TTarget>
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
  catch( ExceptionObject &err )
    {
      // An error has occurred in the optimization.
      // Update the parameters
      m_Parameters = optimizer->GetCurrentPosition();

      // Pass exception to caller
      throw err;
    }

  // get the results
  m_Parameters = optimizer->GetCurrentPosition();

  return 0;

}


} // end namespace itk


#endif
#endif
