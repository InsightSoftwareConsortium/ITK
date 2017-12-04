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
#ifndef itkImageToSpatialObjectRegistrationMethod_hxx
#define itkImageToSpatialObjectRegistrationMethod_hxx

#include "itkImageToSpatialObjectRegistrationMethod.h"

namespace itk
{

template< typename TFixedImage, typename TMovingSpatialObject >
ImageToSpatialObjectRegistrationMethod< TFixedImage, TMovingSpatialObject >
::ImageToSpatialObjectRegistrationMethod()
{
  this->SetNumberOfRequiredOutputs(1); // for the Transform

  m_FixedImage          = ITK_NULLPTR; // has to be provided by the user
  m_MovingSpatialObject = ITK_NULLPTR; // has to be provided by the user
  m_Transform           = ITK_NULLPTR; // has to be provided by the user
  m_Interpolator        = ITK_NULLPTR; // has to be provided by the user
  m_Metric              = ITK_NULLPTR; // has to be provided by the user
  m_Optimizer           = ITK_NULLPTR; // has to be provided by the user

  m_InitialTransformParameters = ParametersType(1);
  m_LastTransformParameters = ParametersType(1);

  m_InitialTransformParameters.Fill(0.0f);
  m_LastTransformParameters.Fill(0.0f);

  TransformOutputPointer transformDecorator =
    itkDynamicCastInDebugMode< TransformOutputType * >(this->MakeOutput(0).GetPointer() );

  this->ProcessObject::SetNthOutput( 0, transformDecorator.GetPointer() );
}

template< typename TFixedImage, typename TMovingSpatialObject >
void
ImageToSpatialObjectRegistrationMethod< TFixedImage, TMovingSpatialObject >
::Initialize()
{
  if ( !m_FixedImage )
    {
    itkExceptionMacro(<< "FixedImage is not present");
    }

  if ( !m_MovingSpatialObject )
    {
    itkExceptionMacro(<< "MovingSpatialObject is not present");
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

  if ( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator is not present");
    }

  m_Interpolator->SetInputImage(m_FixedImage);

  // Set up the metric
  m_Metric->SetFixedImage(m_FixedImage);
  m_Metric->SetMovingSpatialObject(m_MovingSpatialObject);
  m_Metric->SetTransform(m_Transform);
  m_Metric->SetInterpolator(m_Interpolator);
  m_Metric->Initialize();

  // Set up the optimizer
  m_Optimizer->SetCostFunction(m_Metric);

  // Validate initial transform parameters
  if ( m_InitialTransformParameters.Size() != m_Metric->GetNumberOfParameters() )
    {
    itkWarningMacro(<< " WARNING : Size mismatch between initial parameter and transform");
    itkWarningMacro( << "Resizing m_InitialTransformParameters to  " << m_Transform->GetNumberOfParameters() );
    m_InitialTransformParameters.set_size( m_Transform->GetNumberOfParameters() );
    m_InitialTransformParameters.Fill(0.0f);
    }

  if ( m_LastTransformParameters.Size() != m_Transform->GetNumberOfParameters() )
    {
    m_LastTransformParameters.set_size( m_Transform->GetNumberOfParameters() );
    m_LastTransformParameters.Fill(0.0f);
    }

  m_Optimizer->SetInitialPosition(m_InitialTransformParameters);

  // Connect the transform to the Decorator
  TransformOutputType *transformOutput =
    static_cast< TransformOutputType * >( this->ProcessObject::GetOutput(0) );

  transformOutput->Set( m_Transform.GetPointer() );
}

template< typename TFixedImage, typename TMovingSpatialObject >
void
ImageToSpatialObjectRegistrationMethod< TFixedImage, TMovingSpatialObject >
::GenerateData()
{
  try
    {
    // Initialize the interconnects between components
    this->Initialize();
    }
  catch ( ExceptionObject & err )
    {
    // Pass exception to caller
    throw err;
    }

  try
    {
    // Do the optimization
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

  // Get the results
  m_LastTransformParameters = m_Optimizer->GetCurrentPosition();
}

template< typename TFixedImage, typename TMovingSpatialObject >
const typename ImageToSpatialObjectRegistrationMethod< TFixedImage, TMovingSpatialObject >::TransformOutputType *
ImageToSpatialObjectRegistrationMethod< TFixedImage, TMovingSpatialObject >
::GetOutput() const
{
  return static_cast< const TransformOutputType * >( this->ProcessObject::GetOutput(0) );
}

template< typename TFixedImage, typename TMovingSpatialObject >
DataObject::Pointer
ImageToSpatialObjectRegistrationMethod< TFixedImage, TMovingSpatialObject >
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

template< typename TFixedImage, typename TMovingSpatialObject >
ModifiedTimeType
ImageToSpatialObjectRegistrationMethod< TFixedImage, TMovingSpatialObject >
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

  if ( m_MovingSpatialObject )
    {
    m = m_MovingSpatialObject->GetMTime();
    mtime = ( m > mtime ? m : mtime );
    }

  return mtime;
}

template< typename TFixedImage, typename TMovingSpatialObject >
void
ImageToSpatialObjectRegistrationMethod< TFixedImage, TMovingSpatialObject >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Metric: " << m_Metric.GetPointer() << std::endl;
  os << indent << "Optimizer: " << m_Optimizer.GetPointer() << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Fixed Image: " << m_FixedImage.GetPointer() << std::endl;
  os << indent << "Moving SpatialObject: " << m_MovingSpatialObject.GetPointer() << std::endl;
  os << indent << "Initial Transform Parameters: " << m_InitialTransformParameters << std::endl;
  os << indent << "Last    Transform Parameters: " << m_LastTransformParameters << std::endl;
}

} // end namespace itk

#endif
