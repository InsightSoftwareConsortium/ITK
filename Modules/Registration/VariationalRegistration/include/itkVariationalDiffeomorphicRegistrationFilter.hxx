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
#ifndef itkVariationalDiffeomorphicRegistrationFilter_hxx
#define itkVariationalDiffeomorphicRegistrationFilter_hxx
#include "itkVariationalDiffeomorphicRegistrationFilter.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
VariationalDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::
  VariationalDiffeomorphicRegistrationFilter()
{
  // Create new exponential field calculator.
  m_Exponentiator = FieldExponentiatorType::New();

  // Initialize exponentiator iterations.
  m_NumberOfExponentiatorIterations = 4;
}

/*
 * Set the mask image.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::SetInitialDisplacementField(
  DisplacementFieldType * /*ptr*/)
{
  // TODO Calculate logarithm from initial deformation field?

  itkWarningMacro(<< "Initial deformation field cannot be set to "
                  << "itk::VariationalDiffeomorphicRegistrationFilter. "
                  << "Use SetInitialVelocityField() instead!");
}

/*
 * Initialize flags
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::Initialize()
{
  // Allocate deformation field.
  m_DisplacementField = DisplacementFieldType::New();
  m_DisplacementField->CopyInformation(this->GetVelocityField());
  m_DisplacementField->SetRequestedRegion(this->GetVelocityField()->GetRequestedRegion());
  m_DisplacementField->SetBufferedRegion(this->GetVelocityField()->GetBufferedRegion());
  m_DisplacementField->Allocate();

  if (this->GetInput())
  {
    // Calculate velocity field exponential.
    this->CalcDeformationFromVelocityField(this->GetInput());
  }
  else
  {
    // Initialize deformation field with zero vectors.
    typename TDisplacementField::PixelType zeros;
    for (unsigned int j = 0; j < ImageDimension; j++)
    {
      zeros[j] = 0;
    }

    ImageRegionIterator<OutputImageType> defIterator(m_DisplacementField, m_DisplacementField->GetRequestedRegion());

    while (!defIterator.IsAtEnd())
    {
      defIterator.Value() = zeros;
      ++defIterator;
    }
  }

  this->Superclass::Initialize();
}

/**
 * Get the metric value from the difference function
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::ApplyUpdate(
  const TimeStepType & dt)
{
  // Calculate velocity field
  this->Superclass::ApplyUpdate(dt);
  this->GetVelocityField()->Modified();

  // Calculate deformation field from velocity field exponential
  this->CalcDeformationFromVelocityField(this->GetVelocityField());
}

/*
 * Calculates the deformation field by calculating the exponential
 * of the velocity field
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::
  CalcDeformationFromVelocityField(const DisplacementFieldType * velocityField)
{
  m_Exponentiator->SetInput(velocityField);
  m_Exponentiator->AutomaticNumberOfIterationsOff();
  m_Exponentiator->SetMaximumNumberOfIterations(this->GetNumberOfExponentiatorIterations());

  // Graft output of exponentiator.
  m_Exponentiator->GraftOutput(m_DisplacementField);

  // Update and mark as modified.
  m_Exponentiator->Update();
  m_DisplacementField->Modified();
}

/*
 * Print status information
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalDiffeomorphicRegistrationFilter<TFixedImage, TMovingImage, TDisplacementField>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
