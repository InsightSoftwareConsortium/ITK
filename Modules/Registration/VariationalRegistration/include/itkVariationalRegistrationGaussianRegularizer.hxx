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
#ifndef itkVariationalRegistrationGaussianRegularizer_hxx
#define itkVariationalRegistrationGaussianRegularizer_hxx
#include "itkVariationalRegistrationGaussianRegularizer.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"

#include "itkGaussianOperator.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TDisplacementField>
VariationalRegistrationGaussianRegularizer<TDisplacementField>::VariationalRegistrationGaussianRegularizer()
{
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    m_StandardDeviations[j] = 1.0;
  }

  m_MaximumError = 0.1;
  m_MaximumKernelWidth = 30;
}

/**
 * Set the standard deviations.
 */
template <typename TDisplacementField>
void
VariationalRegistrationGaussianRegularizer<TDisplacementField>::SetStandardDeviations(double value)
{
  StandardDeviationsType sigma;
  sigma.Fill(value);

  SetStandardDeviations(sigma);
}

/**
 * Generate data by applying Gaussian regularization independently
 * on each component of the field
 */
template <typename TDisplacementField>
void
VariationalRegistrationGaussianRegularizer<TDisplacementField>::GenerateData()
{
  // Allocate the output image
  this->AllocateOutputs();

  // Initialize and allocate data
  this->Initialize();

  DisplacementFieldConstPointer field = this->GetInput();

  using VectorType = typename DisplacementFieldType::PixelType;
  using ScalarType = typename VectorType::ValueType;
  using OperatorType = GaussianOperator<ScalarType, ImageDimension>;
  using SmootherType = VectorNeighborhoodOperatorImageFilter<DisplacementFieldType, DisplacementFieldType>;

  OperatorType                   opers[ImageDimension];
  typename SmootherType::Pointer smoothers[ImageDimension];

  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    // smooth along this dimension
    opers[j].SetDirection(j);
    typename StandardDeviationsType::ValueType variance = itk::Math::sqr(this->GetStandardDeviations()[j]);
    if (this->GetUseImageSpacing())
    {
      // TODO Considering image spacing in a multi resolution setting leads to
      // very small sigmas and therefore insufficient regularization. Think of
      // a better way?
      itkWarningMacro("Image spacing is not considered during Gaussian "
                      "regularization!");
      opers[j].SetVariance(variance);

      // if( this->GetInput()->GetSpacing()[j] == 0.0 )
      //   {
      //   itkExceptionMacro(<< "Pixel spacing cannot be zero");
      //   }
      // // convert the variance from physical units to pixels
      // const double s = this->GetInput()->GetSpacing()[j];
      // opers[j].SetVariance( variance / itk::Math::sqr(s) );
    }
    else
    {
      opers[j].SetVariance(variance);
    }
    opers[j].SetMaximumError(this->GetMaximumError());
    opers[j].SetMaximumKernelWidth(this->GetMaximumKernelWidth());
    opers[j].CreateDirectional();

    smoothers[j] = SmootherType::New();
    smoothers[j]->SetOperator(opers[j]);
    smoothers[j]->ReleaseDataFlagOn();

    if (j > 0)
    {
      smoothers[j]->SetInput(smoothers[j - 1]->GetOutput());
    }
  }
  smoothers[0]->SetInput(field);
  smoothers[ImageDimension - 1]->GetOutput()->SetRequestedRegion(field->GetBufferedRegion());
  smoothers[ImageDimension - 1]->Update();

  this->GraftOutput(smoothers[ImageDimension - 1]->GetOutput());
}

/*
 * Initialize flags
 */
template <typename TDisplacementField>
void
VariationalRegistrationGaussianRegularizer<TDisplacementField>::Initialize()
{
  this->Superclass::Initialize();
}

/*
 * Print status information
 */
template <typename TDisplacementField>
void
VariationalRegistrationGaussianRegularizer<TDisplacementField>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Standard deviations: [" << m_StandardDeviations[0];
  for (unsigned int j = 1; j < ImageDimension; j++)
  {
    os << ", " << m_StandardDeviations[j];
  }
  os << "]" << std::endl;

  os << indent << "MaximumError: ";
  os << m_MaximumError << std::endl;
  os << indent << "MaximumKernelWidth: ";
  os << m_MaximumKernelWidth << std::endl;
}

} // end namespace itk

#endif
