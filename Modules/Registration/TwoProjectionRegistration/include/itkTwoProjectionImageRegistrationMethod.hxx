/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkTwoProjectionImageRegistrationMethod_hxx
#define itkTwoProjectionImageRegistrationMethod_hxx

#include "itkTwoProjectionImageRegistrationMethod.h"


namespace itk
{

template <typename TFixedImage, typename TMovingImage>
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::TwoProjectionImageRegistrationMethod()
{
  this->SetNumberOfRequiredOutputs(1); // for the Transform

  m_FixedImage1 = nullptr;   // has to be provided by the user.
  m_FixedImage2 = nullptr;   // has to be provided by the user.
  m_MovingImage = nullptr;   // has to be provided by the user.
  m_Transform = nullptr;     // has to be provided by the user.
  m_Interpolator1 = nullptr; // has to be provided by the user.
  m_Interpolator2 = nullptr; // has to be provided by the user.
  m_Metric = nullptr;        // has to be provided by the user.
  m_Optimizer = nullptr;     // has to be provided by the user.


  m_InitialTransformParameters = ParametersType(1);
  m_LastTransformParameters = ParametersType(1);

  m_InitialTransformParameters.Fill(0.0f);
  m_LastTransformParameters.Fill(0.0f);

  m_FixedImageRegionDefined1 = false;
  m_FixedImageRegionDefined2 = false;


  TransformOutputPointer transformDecorator = static_cast<TransformOutputType *>(this->MakeOutput(0).GetPointer());

  this->ProcessObject::SetNthOutput(0, transformDecorator.GetPointer());
}


template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::SetInitialTransformParameters(
  const ParametersType & param)
{
  m_InitialTransformParameters = param;
  this->Modified();
}


/*
 * Set the region of the fixed image 1 to be considered for registration
 */
template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::SetFixedImageRegion1(
  const FixedImageRegionType & region1)
{
  m_FixedImageRegion1 = region1;
  m_FixedImageRegionDefined1 = true;
}

/*
 * Set the region of the fixed image 2 to be considered for registration
 */
template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::SetFixedImageRegion2(
  const FixedImageRegionType & region2)
{
  m_FixedImageRegion2 = region2;
  m_FixedImageRegionDefined2 = true;
}


/*
 * Initialize by setting the interconnects between components.
 */
template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::Initialize()
{

  if (!m_FixedImage1)
  {
    itkExceptionMacro(<< "FixedImage1 is not present");
  }

  if (!m_FixedImage2)
  {
    itkExceptionMacro(<< "FixedImage2 is not present");
  }

  if (!m_MovingImage)
  {
    itkExceptionMacro(<< "MovingImage is not present");
  }

  if (!m_Metric)
  {
    itkExceptionMacro(<< "Metric is not present");
  }

  if (!m_Optimizer)
  {
    itkExceptionMacro(<< "Optimizer is not present");
  }

  if (!m_Transform)
  {
    itkExceptionMacro(<< "Transform is not present");
  }

  //
  // Connect the transform to the Decorator.
  //
  auto * transformOutput = static_cast<TransformOutputType *>(this->ProcessObject::GetOutput(0));

  transformOutput->Set(m_Transform.GetPointer());


  if (!m_Interpolator1)
  {
    itkExceptionMacro(<< "Interpolator1 is not present");
  }

  if (!m_Interpolator2)
  {
    itkExceptionMacro(<< "Interpolator2 is not present");
  }

  // Store user-defined image origin
  // typename FixedImageType::PointType fixedOrigin1 = m_FixedImage1->GetOrigin();
  // typename FixedImageType::PointType fixedOrigin2 = m_FixedImage2->GetOrigin();

  // Setup the metric
  m_Metric->SetMovingImage(m_MovingImage);
  m_Metric->SetFixedImage1(m_FixedImage1);
  m_Metric->SetFixedImage2(m_FixedImage2);
  m_Metric->SetTransform(m_Transform);
  m_Metric->SetInterpolator1(m_Interpolator1);
  m_Metric->SetInterpolator2(m_Interpolator2);

  if (m_FixedImageRegionDefined1)
  {
    m_Metric->SetFixedImageRegion1(m_FixedImageRegion1);
  }
  else
  {
    m_Metric->SetFixedImageRegion1(m_FixedImage1->GetBufferedRegion());
  }

  if (m_FixedImageRegionDefined2)
  {
    m_Metric->SetFixedImageRegion2(m_FixedImageRegion2);
  }
  else
  {
    m_Metric->SetFixedImageRegion2(m_FixedImage2->GetBufferedRegion());
  }

  m_Metric->Initialize();

  // Recover user-defined image origin
  /*  const short Dimension = GetImageDimension<FixedImageType>::ImageDimension;
    double fixedOrg1[Dimension];
    for(int i = 0; i < Dimension; i++)
    fixedOrg1[i] = fixedOrigin1[i];
    m_FixedImage1->SetOrigin(fixedOrg1);
    m_Metric->GetFixedImage1()->SetOrigin(fixedOrg1);

    double fixedOrg2[Dimension];
    for(int i = 0; i < Dimension; i++)
    fixedOrg2[i] = fixedOrigin2[i];
    m_FixedImage2->SetOrigin(fixedOrg2);
    m_Metric->GetFixedImage2()->SetOrigin(fixedOrg2);
  */
  // Setup the optimizer
  m_Optimizer->SetCostFunction(m_Metric);

  // Validate initial transform parameters
  if (m_InitialTransformParameters.Size() != m_Transform->GetNumberOfParameters())
  {
    itkExceptionMacro(<< "Size mismatch between initial parameter and transform");
  }

  m_Optimizer->SetInitialPosition(m_InitialTransformParameters);
}


/*
 * Starts the Registration Process
 */
template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::StartRegistration()
{

  ParametersType empty(1);
  empty.Fill(0.0);
  try
  {
    // initialize the interconnects between components
    this->Initialize();
  }
  catch (ExceptionObject & err)
  {
    m_LastTransformParameters = empty;

    // pass exception to caller
    throw err;
  }

  this->StartOptimization();
}


/*
 * Starts the Optimization process
 */
template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::StartOptimization()
{
  try
  {
    // do the optimization
    m_Optimizer->StartOptimization();
  }
  catch (ExceptionObject & err)
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


template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Metric: " << m_Metric.GetPointer() << std::endl;
  os << indent << "Optimizer: " << m_Optimizer.GetPointer() << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator 1: " << m_Interpolator1.GetPointer() << std::endl;
  os << indent << "Interpolator 2: " << m_Interpolator2.GetPointer() << std::endl;
  os << indent << "Fixed Image 1: " << m_FixedImage1.GetPointer() << std::endl;
  os << indent << "Fixed Image 2: " << m_FixedImage2.GetPointer() << std::endl;
  os << indent << "Moving Image: " << m_MovingImage.GetPointer() << std::endl;
  os << indent << "Fixed Image 1 Region Defined: " << m_FixedImageRegionDefined1 << std::endl;
  os << indent << "Fixed Image 2 Region Defined: " << m_FixedImageRegionDefined2 << std::endl;
  os << indent << "Fixed Image 1 Region: " << m_FixedImageRegion1 << std::endl;
  os << indent << "Fixed Image 2 Region: " << m_FixedImageRegion2 << std::endl;
  os << indent << "Initial Transform Parameters: " << m_InitialTransformParameters << std::endl;
  os << indent << "Last    Transform Parameters: " << m_LastTransformParameters << std::endl;
}


template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::GenerateData()
{
  this->StartRegistration();
}


template <typename TFixedImage, typename TMovingImage>
const typename TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::TransformOutputType *
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::GetOutput() const
{
  return static_cast<const TransformOutputType *>(this->ProcessObject::GetOutput(0));
}


template <typename TFixedImage, typename TMovingImage>
DataObject::Pointer
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::MakeOutput(DataObjectPointerArraySizeType output)
{
  switch (output)
  {
    case 0:
      return static_cast<DataObject *>(TransformOutputType::New().GetPointer());
      break;
    default:
      itkExceptionMacro("MakeOutput request for an output number larger than the expected number of outputs");
      return nullptr;
  }
}


template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::SetFixedImage1(const FixedImageType * fixedImage1)
{
  itkDebugMacro("setting Fixed Image 1 to " << fixedImage1);

  if (this->m_FixedImage1.GetPointer() != fixedImage1)
  {
    this->m_FixedImage1 = fixedImage1;

    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput(0, const_cast<FixedImageType *>(fixedImage1));

    this->Modified();
  }
}


template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::SetFixedImage2(const FixedImageType * fixedImage2)
{
  itkDebugMacro("setting Fixed Image 2 to " << fixedImage2);

  if (this->m_FixedImage2.GetPointer() != fixedImage2)
  {
    this->m_FixedImage2 = fixedImage2;

    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput(0, const_cast<FixedImageType *>(fixedImage2));

    this->Modified();
  }
}


template <typename TFixedImage, typename TMovingImage>
void
TwoProjectionImageRegistrationMethod<TFixedImage, TMovingImage>::SetMovingImage(const MovingImageType * movingImage)
{
  itkDebugMacro("setting Moving Image to " << movingImage);

  if (this->m_MovingImage.GetPointer() != movingImage)
  {
    this->m_MovingImage = movingImage;

    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput(1, const_cast<MovingImageType *>(movingImage));

    this->Modified();
  }
}

} // end namespace itk

#endif
