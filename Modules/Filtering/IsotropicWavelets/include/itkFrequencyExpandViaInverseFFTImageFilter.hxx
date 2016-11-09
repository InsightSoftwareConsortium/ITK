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
#ifndef itkFrequencyExpandViaInverseFFTImageFilter_hxx
#define itkFrequencyExpandViaInverseFFTImageFilter_hxx

#include <itkFrequencyExpandViaInverseFFTImageFilter.h>
#include <itkObjectFactory.h>
#include <itkNumericTraits.h>
#include <itkProgressReporter.h>
#include "itkInd2Sub.h"
#include <itkPasteImageFilter.h>
#include <itkImageRegionConstIterator.h>
#include <itkGaussianSpatialFunction.h>
#include <itkFrequencyImageRegionIteratorWithIndex.h>

namespace itk
{
/**
 * Default constructor
 */
template <typename TImageType>
FrequencyExpandViaInverseFFTImageFilter<TImageType>::FrequencyExpandViaInverseFFTImageFilter()
{
  // Set default factors to 1
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    m_ExpandFactors[j] = 1;
  }
  m_InverseFFT = InverseFFTFilterType::New();
  m_ForwardFFT = ForwardFFTFilterType::New();
  m_Expander = ExpandFilterType::New();
}

/**
 * Standard "PrintSelf" method
 */
template <typename TImageType>
void
FrequencyExpandViaInverseFFTImageFilter<TImageType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  unsigned int j;
  os << indent << "ExpandFactors: [";
  for (j = 0; j < ImageDimension - 1; j++)
  {
    os << m_ExpandFactors[j] << ", ";
  }
  os << m_ExpandFactors[j] << "]" << std::endl;
}

/**
 * Set expand factors from a single unsigned int
 */
template <typename TImageType>
void
FrequencyExpandViaInverseFFTImageFilter<TImageType>::SetExpandFactors(const unsigned int factor)
{
  unsigned int j;

  for (j = 0; j < ImageDimension; j++)
  {
    if (factor != m_ExpandFactors[j])
    {
      break;
    }
  }
  if (j < ImageDimension)
  {
    this->Modified();
    for (j = 0; j < ImageDimension; j++)
    {
      m_ExpandFactors[j] = factor;
      if (m_ExpandFactors[j] < 1)
      {
        m_ExpandFactors[j] = 1;
      }
    }
  }
}

/**
 * Implementation Detail:
 * The implementation calculate the number of different regions in an image,
 * depending on the dimension:
 * numberOfRegions = 2^dim (positive and negative frequencies per dim)
 * then uses function to convert a linear array of regions [0, ..., numberOfRegions - 1]
 * to binary subindices (only two options: positive or negative region)
 * In 3D: numberOfRegions = Nr = 2^3 = 8
 * sizeOfSubindices = [2,2,2]
 * Region = 0       -----> Ind2Sub(   0, [2,2,2]) = [0,0,0]
 * Region = 1       -----> Ind2Sub(   1, [2,2,2]) = [1,0,0]
 * Region = Nr - 1  -----> Ind2Sub(Nr-1, [2,2,2]) = [1,1,1]
 * So, if the result of Ind2Sub is 0 we paste the positive frequencies, if 1, negative freq
 */
template <typename TImageType>
void
FrequencyExpandViaInverseFFTImageFilter<TImageType>::GenerateData()
{
  typename ImageType::Pointer outputPtr = this->GetOutput();

  m_InverseFFT->SetInput(this->GetInput());
  /**** Upsample in spatial domain ***/
  m_Expander->SetInput(m_InverseFFT->GetOutput());
  m_Expander->SetExpandFactors(this->m_ExpandFactors);

  m_ForwardFFT->SetInput(m_Expander->GetOutput());
  m_ForwardFFT->GraftOutput(outputPtr);
  m_ForwardFFT->Update();
  this->GraftOutput(m_ForwardFFT->GetOutput());
}

/**
 * GenerateInputRequesteRegion
 */
template <typename TImageType>
void
FrequencyExpandViaInverseFFTImageFilter<TImageType>::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  ImagePointer inputPtr = const_cast<TImageType *>(this->GetInput());
  ImagePointer outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  // We need to compute the input requested region (size and start index)
  unsigned int                           i;
  const typename TImageType::SizeType &  outputRequestedRegionSize = outputPtr->GetRequestedRegion().GetSize();
  const typename TImageType::IndexType & outputRequestedRegionStartIndex = outputPtr->GetRequestedRegion().GetIndex();

  typename TImageType::SizeType  inputRequestedRegionSize;
  typename TImageType::IndexType inputRequestedRegionStartIndex;

  /**
   * inputRequestedSize = (outputRequestedSize / ExpandFactor) + 1)
   * The extra 1 above is to take care of edge effects when streaming.
   */
  for (i = 0; i < TImageType::ImageDimension; i++)
  {
    inputRequestedRegionSize[i] =
      (SizeValueType)std::ceil((double)outputRequestedRegionSize[i] / (double)m_ExpandFactors[i]) + 1;

    inputRequestedRegionStartIndex[i] =
      (SizeValueType)std::floor((double)outputRequestedRegionStartIndex[i] / (double)m_ExpandFactors[i]);
  }

  typename TImageType::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize(inputRequestedRegionSize);
  inputRequestedRegion.SetIndex(inputRequestedRegionStartIndex);

  // Make sure the requested region is within largest possible.
  inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion());

  // Set the input requested region.
  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

/**
 * GenerateOutputInformation
 */
template <typename TImageType>
void
FrequencyExpandViaInverseFFTImageFilter<TImageType>::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  ImagePointer inputPtr = const_cast<TImageType *>(this->GetInput());
  ImagePointer outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  // We need to compute the output spacing, the output image size, and the
  // output image start index
  const typename TImageType::SpacingType & inputSpacing = inputPtr->GetSpacing();
  const typename TImageType::SizeType &    inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImageType::IndexType &   inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();
  const typename TImageType::PointType &   inputOrigin = inputPtr->GetOrigin();

  typename TImageType::SpacingType outputSpacing;
  typename TImageType::SizeType    outputSize;
  typename TImageType::IndexType   outputStartIndex;
  typename TImageType::PointType   outputOrigin;

  typename TImageType::SpacingType inputOriginShift;

  for (unsigned int i = 0; i < TImageType::ImageDimension; i++)
  {
    outputSpacing[i] = inputSpacing[i] / (float)m_ExpandFactors[i];
    outputSize[i] = inputSize[i] * (SizeValueType)m_ExpandFactors[i];
    outputStartIndex[i] = inputStartIndex[i] * (IndexValueType)m_ExpandFactors[i];
    const double fraction = (double)(m_ExpandFactors[i] - 1) / (double)m_ExpandFactors[i];
    inputOriginShift[i] = -(inputSpacing[i] / 2.0) * fraction;
  }

  const typename TImageType::DirectionType inputDirection = inputPtr->GetDirection();
  const typename TImageType::SpacingType   outputOriginShift = inputDirection * inputOriginShift;

  outputOrigin = inputOrigin + outputOriginShift;

  outputPtr->SetSpacing(outputSpacing);
  outputPtr->SetOrigin(outputOrigin);

  typename TImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}
} // end namespace itk

#endif
