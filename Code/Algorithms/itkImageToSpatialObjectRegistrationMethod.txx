/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToSpatialObjectRegistrationMethod.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToSpatialObjectRegistrationMethod_txx
#define _itkImageToSpatialObjectRegistrationMethod_txx

#include "itkImageToSpatialObjectRegistrationMethod.h"

namespace itk
{

/** Constructor */
template < typename TFixedImage, typename TMovingSpatialObject >
ImageToSpatialObjectRegistrationMethod<TFixedImage,TMovingSpatialObject>
::ImageToSpatialObjectRegistrationMethod()
{
  this->SetNumberOfRequiredOutputs( 1 );  // for the Transform

  m_FixedImage   = 0; // has to be provided by the user.
  m_MovingSpatialObject   = 0; // has to be provided by the user.
  m_Transform    = 0; // has to be provided by the user.
  m_Interpolator = 0; // has to be provided by the user.
  m_Metric       = 0; // has to be provided by the user.
  m_Optimizer    = 0; // has to be provided by the user.


  m_InitialTransformParameters = ParametersType(1);
  m_LastTransformParameters = ParametersType(1);

  m_InitialTransformParameters.Fill( 0.0f );
  m_LastTransformParameters.Fill( 0.0f );


  TransformOutputPointer transformDecorator = 
                 static_cast< TransformOutputType * >( 
                                  this->MakeOutput(0).GetPointer() );

  this->ProcessObject::SetNthOutput( 0, transformDecorator.GetPointer() );
}


/** Initialize by setting the interconnects between components.  */
template < typename TFixedImage, typename TMovingSpatialObject >
void
ImageToSpatialObjectRegistrationMethod<TFixedImage,TMovingSpatialObject>
::Initialize() throw (ExceptionObject)
{
     
  if( !m_FixedImage )
    {
    itkExceptionMacro(<<"FixedImage is not present");
    }

  if( !m_MovingSpatialObject )
    {
    itkExceptionMacro(<<"MovingSpatialObject is not present");
    }

  if ( !m_Metric )
    {
    itkExceptionMacro(<<"Metric is not present" );
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

  m_Interpolator->SetInputImage( m_FixedImage );

  // Setup the metric
  m_Metric->SetFixedImage( m_FixedImage );
  m_Metric->SetMovingSpatialObject( m_MovingSpatialObject );
  m_Metric->SetTransform( m_Transform );
  m_Metric->SetInterpolator( m_Interpolator );
  m_Metric->Initialize();

  // Setup the optimizer
  m_Optimizer->SetCostFunction( m_Metric );

  // Validate initial transform parameters
  if ( m_InitialTransformParameters.Size() != m_Metric->GetNumberOfParameters() )
    {
    itkWarningMacro( << " WARNING : Size mismatch between initial parameter and transform" );
    itkWarningMacro( << "Resizing m_InitialTransformParameters to  " << m_Transform->GetNumberOfParameters() );
    m_InitialTransformParameters.set_size(m_Transform->GetNumberOfParameters());
    m_InitialTransformParameters.Fill( 0.0f );
    }

  if ( m_LastTransformParameters.Size() != m_Transform->GetNumberOfParameters() )
    {
    m_LastTransformParameters.set_size(m_Transform->GetNumberOfParameters());
    m_LastTransformParameters.Fill( 0.0f );
    }

  m_Optimizer->SetInitialPosition( m_InitialTransformParameters );

  //
  // Connect the transform to the Decorator.
  //
  TransformOutputType * transformOutput =  
     static_cast< TransformOutputType * >( this->ProcessObject::GetOutput(0) );

  transformOutput->Set( m_Transform.GetPointer() );
}


/** Starts the Registration Process */
template < typename TFixedImage, typename TMovingSpatialObject >
void
ImageToSpatialObjectRegistrationMethod<TFixedImage,TMovingSpatialObject>
::StartRegistration( void )
{ 
  // StartRegistration is an old API from before
  // the RegistrationMethod was a subclass of ProcessObject.
  // Historically, one could call StartRegistration() instead of
  // calling Update().  However, when called directly by the user, the
  // inputs to the RegistrationMethod may not be up to date.  This
  // may cause an unexpected behavior.
  //
  // Since we cannot eliminate StartRegistration for backward
  // compability reasons, we check whether StartRegistration was
  // called directly or whether Update() (which in turn called 
  // StartRegistration()).
  if (!m_Updating)
    {
    this->Update();
    }
  else
    {
    try
      {
      // initialize the interconnects between components
      this->Initialize();
      }
    catch( ExceptionObject& err )
      {
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

}


/** PrintSelf */
template < typename TFixedImage, typename TMovingSpatialObject >
void
ImageToSpatialObjectRegistrationMethod<TFixedImage,TMovingSpatialObject>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Metric: " << m_Metric.GetPointer() << std::endl;
  os << indent << "Optimizer: " << m_Optimizer.GetPointer() << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Fixed Image: " << m_FixedImage.GetPointer() << std::endl;
  os << indent << "Moving SpatialObject: " << m_MovingSpatialObject.GetPointer() << std::endl;
  os << indent << "Initial Transform Parameters: " << m_InitialTransformParameters << std::endl;
  os << indent << "Last    Transform Parameters: " << m_LastTransformParameters << std::endl;
}


/*
 * Generate Data
 */
template < typename TFixedImage, typename TMovingSpatialObject >
void
ImageToSpatialObjectRegistrationMethod<TFixedImage,TMovingSpatialObject>
::GenerateData()
{
  this->StartRegistration();
}



/*
 *  Get Output
 */
template < typename TFixedImage, typename TMovingSpatialObject >
const typename ImageToSpatialObjectRegistrationMethod<TFixedImage,TMovingSpatialObject>::TransformOutputType *
ImageToSpatialObjectRegistrationMethod<TFixedImage,TMovingSpatialObject>
::GetOutput() const
{
  return static_cast< const TransformOutputType * >( this->ProcessObject::GetOutput(0) );
}



template < typename TFixedImage, typename TMovingSpatialObject >
DataObject::Pointer
ImageToSpatialObjectRegistrationMethod<TFixedImage,TMovingSpatialObject>
::MakeOutput(unsigned int output)
{
  switch (output)
    {
    case 0:
      return static_cast<DataObject*>(TransformOutputType::New().GetPointer());
      break;
    default:
      itkExceptionMacro("MakeOutput request for an output number larger than the expected number of outputs");
      return 0;
    }
}


/**
 *
 */
template < typename TFixedImage, typename TMovingSpatialObject >
unsigned long
ImageToSpatialObjectRegistrationMethod<TFixedImage,TMovingSpatialObject>
::GetMTime() const
{
  unsigned long mtime = Superclass::GetMTime();
  unsigned long m;


  // Some of the following should be removed once ivars are put in the
  // input and output lists
  
  if (m_Transform)
    {
    m = m_Transform->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_Interpolator)
    {
    m = m_Interpolator->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_Metric)
    {
    m = m_Metric->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_Optimizer)
    {
    m = m_Optimizer->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_FixedImage)
    {
    m = m_FixedImage->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_MovingSpatialObject)
    {
    m = m_MovingSpatialObject->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  return mtime;
  
}


} // end namespace itk


#endif
