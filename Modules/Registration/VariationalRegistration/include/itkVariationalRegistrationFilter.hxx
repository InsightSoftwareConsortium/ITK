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
#ifndef itkVariationalRegistrationFilter_hxx
#define itkVariationalRegistrationFilter_hxx
#include "itkVariationalRegistrationFilter.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::VariationalRegistrationFilter()
{
  this->SetNumberOfRequiredInputs(2);

  // Primary input is optional in this filter
  this->RemoveRequiredInputName("Primary");

  this->SetNumberOfIterations(10);

  m_StopRegistrationFlag = false;
  m_SmoothDisplacementField = true;
  m_SmoothUpdateField = false;

  // Initialize with default regularizer.
  m_Regularizer = DefaultRegularizerType::New();

  // Initialize with default registration function.
  this->SetDifferenceFunction(DefaultRegistrationFunctionType::New());
}

/*
 * Set the fixed image.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::SetFixedImage(const FixedImageType * ptr)
{
  this->ProcessObject::SetNthInput(1, const_cast<FixedImageType *>(ptr));
}

/*
 * Get the fixed image.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
const typename VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::FixedImageType *
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::GetFixedImage() const
{
  return dynamic_cast<const FixedImageType *>(this->ProcessObject::GetInput(1));
}

/*
 * Set the moving image.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::SetMovingImage(
  const MovingImageType * ptr)
{
  this->ProcessObject::SetNthInput(2, const_cast<MovingImageType *>(ptr));
}

/*
 * Get the moving image.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
const typename VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::MovingImageType *
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::GetMovingImage() const
{
  return dynamic_cast<const MovingImageType *>(this->ProcessObject::GetInput(2));
}

/*
 * Set the mask image.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::SetMaskImage(const MaskImageType * ptr)
{
  this->ProcessObject::SetNthInput(3, const_cast<MaskImageType *>(ptr));
}

/*
 * Get the mask image.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
const typename VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::MaskImageType *
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::GetMaskImage() const
{
  return dynamic_cast<const MaskImageType *>(this->ProcessObject::GetInput(3));
}

/*
 * Checks if the required inputs are set (FixedImage and MovingImage).
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
std::vector<SmartPointer<DataObject>>::size_type
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::GetNumberOfValidRequiredInputs() const
{
  typename std::vector<SmartPointer<DataObject>>::size_type num = 0;

  if (this->GetFixedImage())
  {
    num++;
  }

  if (this->GetMovingImage())
  {
    num++;
  }

  return num;
}

/*
 * Generate output information. If initial field is set, use this for
 * generation, else use the fixed image.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::GenerateOutputInformation()
{
  typename DataObject::Pointer output;

  if (this->GetInput(0))
  {
    // Initial deformation field is set.
    // Copy information from initial field.
    this->Superclass::GenerateOutputInformation();
  }
  else if (this->GetFixedImage())
  {
    // Initial deformation field is not set.
    // Copy information from the fixed image.
    for (unsigned int idx = 0; idx < this->GetNumberOfIndexedOutputs(); ++idx)
    {
      output = this->GetOutput(idx);
      if (output)
      {
        output->CopyInformation(this->GetFixedImage());
      }
    }
  }
}

/*
 *
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::GenerateInputRequestedRegion()
{
  // call the superclass's implementation
  Superclass::GenerateInputRequestedRegion();

  // request the largest possible region for the moving image
  MovingImagePointer movingPtr = const_cast<MovingImageType *>(this->GetMovingImage());

  if (movingPtr)
  {
    movingPtr->SetRequestedRegionToLargestPossibleRegion();
  }

  // just propagate up the output requested region for
  // the fixed image and initial deformation field.
  FixedImagePointer        fixedPtr = const_cast<FixedImageType *>(this->GetFixedImage());
  DisplacementFieldPointer inputPtr = const_cast<DisplacementFieldType *>(this->GetInput());
  DisplacementFieldPointer outputPtr = this->GetOutput();

  if (inputPtr)
  {
    inputPtr->SetRequestedRegion(outputPtr->GetRequestedRegion());
  }

  if (fixedPtr)
  {
    fixedPtr->SetRequestedRegion(outputPtr->GetRequestedRegion());
  }
}

/*
 * Override the default implementation for the case when no initial deformation
 * field is set. In this case, the output is filled with zero vectors.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::CopyInputToOutput()
{
  typename Superclass::InputImageType::ConstPointer inputPtr = this->GetInput();

  if (inputPtr)
  {
    this->Superclass::CopyInputToOutput();
  }
  else
  {
    typename Superclass::PixelType zeros;
    for (unsigned int j = 0; j < ImageDimension; j++)
    {
      zeros[j] = 0;
    }

    typename OutputImageType::Pointer output = this->GetOutput();

    ImageRegionIterator<OutputImageType> out(output, output->GetRequestedRegion());

    out.GoToBegin();
    while (!out.IsAtEnd())
    {
      out.Value() = zeros;
      ++out;
    }
  }
}

/**
 * Checks whether the DifferenceFunction is of type DemonsRegistrationFunction.
 * It throws and exception, if it is not.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
typename VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::RegistrationFunctionType *
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::DownCastDifferenceFunctionType()
{
  auto * rfp = dynamic_cast<RegistrationFunctionType *>(this->GetDifferenceFunction().GetPointer());

  if (!rfp)
  {
    itkExceptionMacro(<< "Could not cast difference function to RegistrationFunctionType");
  }

  return rfp;
}

/**
 * Checks whether the DifferenceFunction is of type DemonsRegistrationFunction.
 * It throws and exception, if it is not.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
const typename VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::RegistrationFunctionType *
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::DownCastDifferenceFunctionType() const
{
  const auto * rfp = dynamic_cast<const RegistrationFunctionType *>(this->GetDifferenceFunction().GetPointer());

  if (!rfp)
  {
    itkExceptionMacro(<< "Could not cast difference function to SymmetricDemonsRegistrationFunction");
  }

  return rfp;
}

/*
 * Get the metric value from the difference function
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
double
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::GetMetric() const
{
  const RegistrationFunctionType * rfp = this->DownCastDifferenceFunctionType();
  return rfp->GetMetric();
}

/*
 * Initialize flags
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::Initialize()
{
  // Initialize Superclass
  this->Superclass::Initialize();

  // Set StopRegistrationFlag false
  m_StopRegistrationFlag = false;
}

/*
 * Set the function state values before each iteration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::InitializeIteration()
{
  MovingImageConstPointer movingPtr = this->GetMovingImage();
  FixedImageConstPointer  fixedPtr = this->GetFixedImage();
  MaskImageConstPointer   maskImage = this->GetMaskImage();

  if (!movingPtr || !fixedPtr)
  {
    itkExceptionMacro(<< "Fixed and/or moving image not set");
  }

  // Initialize registration function.
  RegistrationFunctionType * rfp = this->DownCastDifferenceFunctionType();

  rfp->SetFixedImage(fixedPtr);
  rfp->SetMovingImage(movingPtr);
  rfp->SetDisplacementField(this->GetDisplacementField());

  if (maskImage)
  {
    rfp->SetMaskImage(maskImage);
  }

  // Call superclass method.
  this->Superclass::InitializeIteration();
}

/**
 * Get the metric value from the difference function
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::ApplyUpdate(const TimeStepType & dt)
{
  // If fluid-like registration is performed, smooth the update field.
  if (this->GetSmoothUpdateField())
  {
    m_Regularizer->SetInput(this->GetUpdateBuffer());
    m_Regularizer->GetOutput()->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
    m_Regularizer->Update();

    this->GetUpdateBuffer()->Graft(m_Regularizer->GetOutput());
  }

  // Adds update field to output (deformation field).
  this->Superclass::ApplyUpdate(dt);

  // If diffusion-like registration is performed, smooth the output
  // (= deformation field).
  if (this->GetSmoothDisplacementField())
  {
    m_Regularizer->SetInput(this->GetOutput());
    m_Regularizer->GetOutput()->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
    m_Regularizer->Update();

    this->GetOutput()->Graft(m_Regularizer->GetOutput());
  }

  // Get metric from registration function.
  const RegistrationFunctionType * rfp = this->DownCastDifferenceFunctionType();
  this->SetRMSChange(rfp->GetRMSChange());
}

/*
 * Print status information
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::PrintSelf(std::ostream & os,
                                                                                        Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Regularizer: ";
  os << m_Regularizer.GetPointer() << std::endl;

  os << indent << "StopRegistrationFlag: ";
  os << m_StopRegistrationFlag << std::endl;

  os << indent << "SmoothDisplacementField: ";
  os << m_SmoothDisplacementField << std::endl;
  os << indent << "SmoothUpdateField: ";
  os << m_SmoothUpdateField << std::endl;
}

} // end namespace itk

#endif
