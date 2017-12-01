/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageRegistrationMethod_hxx
#define itkImageRegistrationMethod_hxx

#include "itkImageRegistrationMethod.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TFixedImage, typename TMovingImage >
ImageRegistrationMethod< TFixedImage, TMovingImage >
::ImageRegistrationMethod()
{
  this->SetNumberOfRequiredOutputs(1);    // for the Transform

  m_FixedImage   = ITK_NULLPTR; // has to be provided by the user.
  m_MovingImage  = ITK_NULLPTR; // has to be provided by the user.
  m_Transform    = ITK_NULLPTR; // has to be provided by the user.
  m_Interpolator = ITK_NULLPTR; // has to be provided by the user.
  m_Metric       = ITK_NULLPTR; // has to be provided by the user.
  m_Optimizer    = ITK_NULLPTR; // has to be provided by the user.

  m_InitialTransformParameters = ParametersType(1);
  m_LastTransformParameters = ParametersType(1);

  m_InitialTransformParameters.Fill(0.0f);
  m_LastTransformParameters.Fill(0.0f);

  m_FixedImageRegionDefined = false;

  TransformOutputPointer transformDecorator =
    itkDynamicCastInDebugMode< TransformOutputType * >(this->MakeOutput(0).GetPointer() );

  this->ProcessObject::SetNthOutput( 0, transformDecorator.GetPointer() );

  this->SetNumberOfThreads( this->GetMultiThreader()->GetNumberOfThreads() );
}

/**
 *
 */
template< typename TFixedImage, typename TMovingImage >
ModifiedTimeType
ImageRegistrationMethod< TFixedImage, TMovingImage >
::GetMTime() const
{
  ModifiedTimeType mtime = Superclass::GetMTime();
  ModifiedTimeType m;

  // Some of the following should be removed once ivars are put in the
  // input and output lists

  if ( m_Transform )
    {
    m = m_Transform->GetMTime();
    mtime = ( m > mtime ? m : mtime );
    }

  if ( m_Interpolator )
    {
    m = m_Interpolator->GetMTime();
    mtime = ( m > mtime ? m : mtime );
    }

  if ( m_Metric )
    {
    m = m_Metric->GetMTime();
    mtime = ( m > mtime ? m : mtime );
    }

  if ( m_Optimizer )
    {
    m = m_Optimizer->GetMTime();
    mtime = ( m > mtime ? m : mtime );
    }

  if ( m_FixedImage )
    {
    m = m_FixedImage->GetMTime();
    mtime = ( m > mtime ? m : mtime );
    }

  if ( m_MovingImage )
    {
    m = m_MovingImage->GetMTime();
    mtime = ( m > mtime ? m : mtime );
    }

  return mtime;
}

/*
 * Set the initial transform parameters
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod< TFixedImage, TMovingImage >
::SetInitialTransformParameters(const ParametersType & param)
{
  m_InitialTransformParameters = param;
  this->Modified();
}

/**

 * Set the region of the fixed image to be considered for registration
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod< TFixedImage, TMovingImage >
::SetFixedImageRegion(const FixedImageRegionType & region)
{
  m_FixedImageRegion = region;
  m_FixedImageRegionDefined = true;
  this->Modified();
}

/**
 * Initialize by setting the interconnects between components.
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod< TFixedImage, TMovingImage >
::Initialize()
{
  if ( !m_FixedImage )
    {
    itkExceptionMacro(<< "FixedImage is not present");
    }

  if ( !m_MovingImage )
    {
    itkExceptionMacro(<< "MovingImage is not present");
    }

  if ( !m_Metric )
    {
    itkExceptionMacro(<< "Metric is not present");
    }

  if ( !m_Optimizer )
    {
    itkExceptionMacro(<< "Optimizer is not present");
    }

  if ( !m_Transform )
    {
    itkExceptionMacro(<< "Transform is not present");
    }

  //
  // Connect the transform to the Decorator.
  //
  TransformOutputType *transformOutput =
    static_cast< TransformOutputType * >( this->ProcessObject::GetOutput(0) );

  transformOutput->Set( m_Transform.GetPointer() );

  if ( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator is not present");
    }

  // Setup the metric
  this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
  this->m_Metric->SetNumberOfThreads( this->GetNumberOfThreads() );
  m_Metric->SetMovingImage(m_MovingImage);
  m_Metric->SetFixedImage(m_FixedImage);
  m_Metric->SetTransform(m_Transform);
  m_Metric->SetInterpolator(m_Interpolator);

  if ( m_FixedImageRegionDefined )
    {
    m_Metric->SetFixedImageRegion(m_FixedImageRegion);
    }
  else
    {
    m_Metric->SetFixedImageRegion( m_FixedImage->GetBufferedRegion() );
    }

  m_Metric->Initialize();

  // Setup the optimizer
  m_Optimizer->SetCostFunction(m_Metric);

  // Validate initial transform parameters
  if ( m_InitialTransformParameters.Size() !=
       m_Transform->GetNumberOfParameters() )
    {
    itkExceptionMacro(<< "Size mismatch between initial parameters and transform."
                      << "Expected " << m_Transform->GetNumberOfParameters() << " parameters and received "
                      <<  m_InitialTransformParameters.Size() << " parameters");
    }

  m_Optimizer->SetInitialPosition(m_InitialTransformParameters);
}

/**
 * Starts the Optimization process
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod< TFixedImage, TMovingImage >
::StartOptimization(void)
{
  try
    {
    // do the optimization
    m_Optimizer->StartOptimization();
    }
  catch ( ExceptionObject & err )
    {
    // An error has occurred in the optimization.
    // Update the parameters
    m_LastTransformParameters = m_Optimizer->GetCurrentPosition();

    // Pass exception to caller
    throw err;
    }

  // get the results
  m_LastTransformParameters = m_Optimizer->GetCurrentPosition();
  m_Transform->SetParameters(m_LastTransformParameters);
}

/**
 * PrintSelf
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod< TFixedImage, TMovingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Metric: " << m_Metric.GetPointer() << std::endl;
  os << indent << "Optimizer: " << m_Optimizer.GetPointer() << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Fixed Image: " << m_FixedImage.GetPointer() << std::endl;
  os << indent << "Moving Image: " << m_MovingImage.GetPointer() << std::endl;
  os << indent << "Fixed Image Region Defined: " << m_FixedImageRegionDefined << std::endl;
  os << indent << "Fixed Image Region: " << m_FixedImageRegion << std::endl;
  os << indent << "Initial Transform Parameters: " << m_InitialTransformParameters << std::endl;
  os << indent << "Last    Transform Parameters: " << m_LastTransformParameters << std::endl;
}

/*
 * Generate Data
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod< TFixedImage, TMovingImage >
::GenerateData()
{
  ParametersType empty(1);
  empty.Fill(0.0);
  try
    {
    // initialize the interconnects between components
    this->Initialize();
    }
  catch ( ExceptionObject & err )
    {
    m_LastTransformParameters = empty;

    // pass exception to caller
    throw err;
    }

  this->StartOptimization();
}

/**
 *  Get Output
 */
template< typename TFixedImage, typename TMovingImage >
const typename ImageRegistrationMethod< TFixedImage, TMovingImage >::TransformOutputType *
ImageRegistrationMethod< TFixedImage, TMovingImage >
::GetOutput() const
{
  return static_cast< const TransformOutputType * >( this->ProcessObject::GetOutput(0) );
}

template< typename TFixedImage, typename TMovingImage >
DataObject::Pointer
ImageRegistrationMethod< TFixedImage, TMovingImage >
::MakeOutput(DataObjectPointerArraySizeType output)
{
  switch ( output )
    {
    case 0:
      return TransformOutputType::New().GetPointer();
      break;
    default:
      itkExceptionMacro("MakeOutput request for an output number larger than the expected number of outputs");
      return ITK_NULLPTR;
    }
}

template< typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod< TFixedImage, TMovingImage >
::SetFixedImage(const FixedImageType *fixedImage)
{
  itkDebugMacro("setting Fixed Image to " << fixedImage);

  if ( this->m_FixedImage.GetPointer() != fixedImage )
    {
    this->m_FixedImage = fixedImage;

    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput( 0,
                                      const_cast< FixedImageType * >( fixedImage ) );

    this->Modified();
    }
}

template< typename TFixedImage, typename TMovingImage >
void
ImageRegistrationMethod< TFixedImage, TMovingImage >
::SetMovingImage(const MovingImageType *movingImage)
{
  itkDebugMacro("setting Moving Image to " << movingImage);

  if ( this->m_MovingImage.GetPointer() != movingImage )
    {
    this->m_MovingImage = movingImage;

    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput( 1,
                                      const_cast< MovingImageType * >( movingImage ) );

    this->Modified();
    }
}

} // end namespace itk
#endif
