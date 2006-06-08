/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToImageRegistrationMethod.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPointSetToImageRegistrationMethod_txx
#define _itkPointSetToImageRegistrationMethod_txx

#include "itkPointSetToImageRegistrationMethod.h"


namespace itk
{

/*
 * Constructor
 */
template < typename TFixedPointSet, typename TMovingImage >
PointSetToImageRegistrationMethod<TFixedPointSet,TMovingImage>
::PointSetToImageRegistrationMethod()
{
  this->SetNumberOfRequiredOutputs( 1 );  // for the Transform

  m_FixedPointSet   = 0; // has to be provided by the user.
  m_MovingImage     = 0; // has to be provided by the user.
  m_Transform       = 0; // has to be provided by the user.
  m_Interpolator    = 0; // has to be provided by the user.
  m_Metric          = 0; // has to be provided by the user.
  m_Optimizer       = 0; // has to be provided by the user.


  m_InitialTransformParameters = ParametersType(1);
  m_LastTransformParameters = ParametersType(1);

  m_InitialTransformParameters.Fill( 0.0f );
  m_LastTransformParameters.Fill( 0.0f );

  TransformOutputPointer transformDecorator = 
                 static_cast< TransformOutputType * >( 
                                  this->MakeOutput(0).GetPointer() );

  this->ProcessObject::SetNthOutput( 0, transformDecorator.GetPointer() );
}


/*
 * Set the initial transform parameters
 */
template < typename TFixedPointSet, typename TMovingImage >
void
PointSetToImageRegistrationMethod<TFixedPointSet,TMovingImage>
::SetInitialTransformParameters( const ParametersType & param )
{
  m_InitialTransformParameters = param;
  this->Modified();
}



/*
 * Initialize by setting the interconnects between components. 
 */
template < typename TFixedPointSet, typename TMovingImage >
void
PointSetToImageRegistrationMethod<TFixedPointSet,TMovingImage>
::Initialize() throw (ExceptionObject)
{

  if( !m_FixedPointSet )
    {
    itkExceptionMacro(<<"FixedPointSet is not present");
    }

  if( !m_MovingImage )
    {
    itkExceptionMacro(<<"MovingImage is not present");
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

  // Setup the metric
  m_Metric->SetMovingImage(   m_MovingImage );
  m_Metric->SetFixedPointSet( m_FixedPointSet );
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

  //
  // Connect the transform to the Decorator.
  //
  TransformOutputType * transformOutput =  
     static_cast< TransformOutputType * >( this->ProcessObject::GetOutput(0) );

  transformOutput->Set( m_Transform.GetPointer() );
}


/*
 * Starts the Registration Process
 */
template < typename TFixedPointSet, typename TMovingImage >
void
PointSetToImageRegistrationMethod<TFixedPointSet,TMovingImage>
::StartRegistration( void )
{ 

  // StartRegistration is an old API from before
  // ImageRegistrationMethod was a subclass of ProcessObject.
  // Historically, one could call StartRegistration() instead of
  // calling Update().  However, when called directly by the user, the
  // inputs to ImageRegistrationMethod may not be up to date.  This
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
      m_LastTransformParameters = ParametersType(1);
      m_LastTransformParameters.Fill( 0.0f );
      
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
    
    m_Transform->SetParameters( m_LastTransformParameters );
    }
}


/*
 * PrintSelf
 */
template < typename TFixedPointSet, typename TMovingImage >
void
PointSetToImageRegistrationMethod<TFixedPointSet,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Metric: " << m_Metric.GetPointer() << std::endl;
  os << indent << "Optimizer: " << m_Optimizer.GetPointer() << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Fixed Image: " << m_FixedPointSet.GetPointer() << std::endl;
  os << indent << "Moving Image: " << m_MovingImage.GetPointer() << std::endl;
  os << indent << "Initial Transform Parameters: " << m_InitialTransformParameters << std::endl;
  os << indent << "Last    Transform Parameters: " << m_LastTransformParameters << std::endl;
}


/*
 * Generate Data
 */
template < typename TFixedPointSet, typename TMovingImage >
void
PointSetToImageRegistrationMethod<TFixedPointSet,TMovingImage>
::GenerateData()
{
  this->StartRegistration();
}


/*
 *  Get Output
 */
template < typename TFixedPointSet, typename TMovingImage >
const typename PointSetToImageRegistrationMethod<TFixedPointSet,TMovingImage>::TransformOutputType *
PointSetToImageRegistrationMethod<TFixedPointSet,TMovingImage>
::GetOutput() const
{
  return static_cast< const TransformOutputType * >( this->ProcessObject::GetOutput(0) );
}



template < typename TFixedPointSet, typename TMovingImage >
DataObject::Pointer
PointSetToImageRegistrationMethod<TFixedPointSet,TMovingImage>
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
template < typename TFixedPointSet, typename TMovingImage >
unsigned long
PointSetToImageRegistrationMethod<TFixedPointSet,TMovingImage>
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

  if (m_FixedPointSet)
    {
    m = m_FixedPointSet->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_MovingImage)
    {
    m = m_MovingImage->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  return mtime;
  
}


} // end namespace itk


#endif
