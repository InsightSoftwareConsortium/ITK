/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegistrationMethod.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageRegistrationMethod_txx
#define _itkImageRegistrationMethod_txx

#include "itkImageRegistrationMethod.h"


namespace itk
{

/**
 * Constructor
 */
template < typename TFixedImage, typename TMovingImage >
ImageRegistrationMethod<TFixedImage,TMovingImage>
::ImageRegistrationMethod()
{

  m_FixedImage   = 0; // has to be provided by the user.
  m_MovingImage  = 0; // has to be provided by the user.
  m_Transform    = 0; // has to be provided by the user.
  m_Interpolator = 0; // has to be provided by the user.
  m_Metric       = 0; // has to be provided by the user.
  m_Optimizer    = 0; // has to be provided by the user.


  m_InitialTransformParameters = ParametersType(1);
  m_LastTransformParameters = ParametersType(1);

  m_InitialTransformParameters.Fill( 0.0f );
  m_LastTransformParameters.Fill( 0.0f );

}


/**
 * Initialize by setting the interconnects between components. 
 */
template < typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod<TFixedImage,TMovingImage>
::Initialize() throw (ExceptionObject)
{

  if( !m_FixedImage )
    {
    itkExceptionMacro(<<"FixedImage is not present");
    }

  if( !m_MovingImage )
    {
    itkExceptionMacro(<<"MovingImage is not present");
    }

  if ( !m_Metric )
    {
    itkExceptionMacro(<<"Transform is not present" );
    }

  if ( !m_Optimizer )
    {
    itkExceptionMacro(<<"Optimizer is not present" );
    }

  if( !m_Transform )
    {
    itkExceptionMacro(<<"Transform is not present");
    }

  if( !m_Interpolator )
    {
    itkExceptionMacro(<<"Interpolator is not present");
    }

  // Setup the metric
  m_Metric->SetMovingImage( m_MovingImage );
  m_Metric->SetFixedImage( m_FixedImage );
  m_Metric->SetTransform( m_Transform );
  m_Metric->SetInterpolator( m_Interpolator );
  m_Metric->Initialize();

  // Setup the optimizer
  m_Optimizer->SetCostFunction( m_Metric );

  // Validate initial transform parameters
  if ( m_InitialTransformParameters.Size() != 
    m_Transform->GetNumberOfParameters() )
    {
    itkExceptionMacro(<<"Size mismatch between initial parameter and transform"); 
    }

  m_Optimizer->SetInitialPosition( m_InitialTransformParameters );

}


/**
 * Starts the Registration Process
 */
template < typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod<TFixedImage,TMovingImage>
::StartRegistration( void )
{ 

  try
    {
    // initialize the interconnects between components
    this->Initialize();
    }
  catch( ExceptionObject& err )
    {
    m_LastTransformParameters = ParametersType(0 );

    // pass exception to caller
    throw err;
    }


  try
    {
    // do the optimization
    m_Optimizer->StartOptimization();
    }
  catch( ExceptionObject& err )
    {
      // An error has occurred in the optimization.
      // Update the parameters
      m_LastTransformParameters = m_Optimizer->GetCurrentPosition();

      // Pass exception to caller
      throw err;
    }


  // get the results
  m_LastTransformParameters = m_Optimizer->GetCurrentPosition();

}


/**
 * PrintSelf
 */
template < typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Metric: " << m_Metric.GetPointer() << std::endl;
  os << indent << "Optimizer: " << m_Optimizer.GetPointer() << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Fixed Image " << m_FixedImage.GetPointer() << std::endl;
  os << indent << "Moving Image " << m_MovingImage.GetPointer() << std::endl;
  os << indent << "Initial Transform Parameters " << m_InitialTransformParameters << std::endl;
  os << indent << "Last    Transform Parameters " << m_LastTransformParameters << std::endl;
}




} // end namespace itk


#endif
