/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkVariationalSymmetricDiffeomorphicRegistrationFilter_hxx
#define itkVariationalSymmetricDiffeomorphicRegistrationFilter_hxx
#include "itkVariationalSymmetricDiffeomorphicRegistrationFilter.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
VariationalSymmetricDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::
  VariationalSymmetricDiffeomorphicRegistrationFilter()
{
  m_InverseExponentiator = FieldExponentiatorType::New();
  m_InverseExponentiator->ComputeInverseOn();
}

/*
 * Initialize flags
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalSymmetricDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::Initialize()
{
  // Check sizes of moving and fixed image.
  const FixedImageType *  fixim = this->GetFixedImage();
  const MovingImageType * movim = this->GetMovingImage();

  if (fixim == nullptr || movim == nullptr)
  {
    itkExceptionMacro(<< "A fixed and a moving image are required");
  }

  if (fixim->GetLargestPossibleRegion() != movim->GetLargestPossibleRegion())
  {
    itkExceptionMacro(<< "Registering images that have different sizes is not supported yet.");
  }

  if ((fixim->GetSpacing() - movim->GetSpacing()).GetNorm() > 1e-10)
  {
    itkExceptionMacro(<< "Registering images that have different spacing is not supported yet.");
  }

  if ((fixim->GetOrigin() - movim->GetOrigin()).GetNorm() > 1e-10)
  {
    itkExceptionMacro(<< "Registering images that have different origins is not supported yet.");
  }

  // Allocate inverse deformation field.
  m_InverseDisplacementField = DisplacementFieldType::New();
  m_InverseDisplacementField->CopyInformation(this->GetOutput());
  m_InverseDisplacementField->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
  m_InverseDisplacementField->SetBufferedRegion(this->GetOutput()->GetBufferedRegion());
  m_InverseDisplacementField->Allocate();

  if (this->GetInput())
  {
    // Calculate velocity field exponential.
    this->CalcInverseDeformationFromVelocityField(this->GetInput());
  }
  else
  {
    // Initialize deformation field with zero vectors.
    typename TDisplacementField::PixelType zeros;
    zeros.Fill(0.0);
    m_InverseDisplacementField->FillBuffer(zeros);
  }

  // Allocate backward update buffer.
  m_BackwardUpdateBuffer = UpdateBufferType::New();
  m_BackwardUpdateBuffer->CopyInformation(this->GetOutput());
  m_BackwardUpdateBuffer->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
  m_BackwardUpdateBuffer->SetBufferedRegion(this->GetOutput()->GetBufferedRegion());
  m_BackwardUpdateBuffer->Allocate();

  // Initialize superclass.
  this->Superclass::Initialize();
}

/*
 * Set the function state values before each iteration
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalSymmetricDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::
  InitializeBackwardIteration()
{
  MovingImageConstPointer                    movingPtr = this->GetMovingImage();
  FixedImageConstPointer                     fixedPtr = this->GetFixedImage();
  typename Superclass::MaskImageConstPointer maskImage = this->GetMaskImage();

  // Initialize registration function.
  RegistrationFunctionType * rfp = this->DownCastDifferenceFunctionType();

  rfp->SetFixedImage(movingPtr);
  rfp->SetMovingImage(fixedPtr);
  rfp->SetDisplacementField(this->GetModifiableInverseDisplacementField());

  if (maskImage)
  {
    rfp->SetMaskImage(maskImage);
  }

  rfp->InitializeIteration();
}

template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
typename VariationalSymmetricDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::
  TimeStepType
  VariationalSymmetricDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::CalculateChange()
{
  TimeStepType dt;

  // Call super class method for forward iteration.
  dt = this->Superclass::CalculateChange();

  // Swap pixel container since CalculateChange() always stores update
  // buffer in m_UpdateBuffer.
  typename UpdateBufferType::PixelContainerPointer swap;

  swap = this->GetUpdateBuffer()->GetPixelContainer();
  this->GetUpdateBuffer()->SetPixelContainer(this->GetModifiableBackwardUpdateBuffer()->GetPixelContainer());
  this->GetModifiableBackwardUpdateBuffer()->SetPixelContainer(swap);

  // Initialize backward iteration.
  this->InitializeBackwardIteration();

  // Call super class method for backward iteration.
  dt += this->Superclass::CalculateChange();

  // Swap back pixel container to have backward buffer
  // stored in m_BackwardUpdateBuffer.
  swap = this->GetUpdateBuffer()->GetPixelContainer();
  this->GetUpdateBuffer()->SetPixelContainer(this->GetModifiableBackwardUpdateBuffer()->GetPixelContainer());
  this->GetModifiableBackwardUpdateBuffer()->SetPixelContainer(swap);

  // Return mean time step.
  return 0.5 * dt;
}

template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalSymmetricDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::ApplyUpdate(
  const TimeStepType & dt)
{
  // Calculate velocity field
  this->Superclass::ApplyUpdate(dt);

  // Calculate deformation field from velocity field exponential
  this->CalcInverseDeformationFromVelocityField(this->GetVelocityField());
}

template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalSymmetricDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::ThreadedApplyUpdate(
  const TimeStepType &     dt,
  const ThreadRegionType & regionToProcess,
  unsigned int)
{
  ImageRegionIterator<UpdateBufferType> f(this->GetUpdateBuffer(), regionToProcess);
  ImageRegionIterator<UpdateBufferType> b(m_BackwardUpdateBuffer, regionToProcess);
  ImageRegionIterator<OutputImageType>  o(this->GetOutput(), regionToProcess);

  using PixelType = typename OutputImageType::PixelType;

  f.GoToBegin();
  b.GoToBegin();
  o.GoToBegin();

  TimeStepType dtHalf = dt * 0.5;

  while (!o.IsAtEnd())
  {
    o.Value() += static_cast<PixelType>((f.Value() - b.Value()) * dtHalf);
    ++o;
    ++f;
    ++b;
  }
}

/*
 * Calculates the inverse deformation field by calculating the exponential
 * of the negative velocity field
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalSymmetricDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::
  CalcInverseDeformationFromVelocityField(const DisplacementFieldType * velocityField)
{
  m_InverseExponentiator->SetInput(velocityField);
  m_InverseExponentiator->AutomaticNumberOfIterationsOff();
  m_InverseExponentiator->SetMaximumNumberOfIterations(this->GetNumberOfExponentiatorIterations());

  // Graft output of exponentiator.
  m_InverseExponentiator->GraftOutput(m_InverseDisplacementField);

  // Update and mark as modified.
  m_InverseExponentiator->Update();
  m_InverseDisplacementField->Modified();
}

/*
 * Print status information
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalSymmetricDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
