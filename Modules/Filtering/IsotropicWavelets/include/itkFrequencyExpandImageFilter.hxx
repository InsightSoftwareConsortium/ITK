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
#ifndef itkFrequencyExpandImageFilter_hxx
#define itkFrequencyExpandImageFilter_hxx

#include <itkProgressReporter.h>
#include "itkInd2Sub.h"
#include <itkPasteImageFilter.h>

namespace itk
{
/**
 * Default constructor
 */
template <typename TImageType>
FrequencyExpandImageFilter<TImageType>::FrequencyExpandImageFilter()
{
  // Set default factors to 1
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    m_ExpandFactors[j] = 2;
  }
}

/**
 * Standard "PrintSelf" method
 */
template <typename TImageType>
void
FrequencyExpandImageFilter<TImageType>::PrintSelf(std::ostream & os, Indent indent) const
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
FrequencyExpandImageFilter<TImageType>::SetExpandFactors(const unsigned int factor)
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
 * Assume Expand Factor = 2
 * Input image is pasted 2^ImageDimension times to form the outputImage.
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
FrequencyExpandImageFilter<TImageType>::GenerateData()
{
  const ImageType * inputPtr = this->GetInput();
  ImagePointer      outputPtr = this->GetOutput();

  // complex is initialized to zero directly.

  this->AllocateOutputs();
  outputPtr->FillBuffer(0);
  typename TImageType::SizeType  inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  typename TImageType::IndexType inputOriginIndex = inputPtr->GetLargestPossibleRegion().GetIndex();
  typename TImageType::SizeType  outputSize = outputPtr->GetLargestPossibleRegion().GetSize();

  const typename TImageType::IndexType indexRequested = outputPtr->GetLargestPossibleRegion().GetIndex();

  // Manage ImageDimension array linearly:{{{
  FixedArray<unsigned int, ImageDimension> nsizes;
  unsigned int                             numberOfRegions = 1;
  for (unsigned int dim = 0; dim < ImageDimension; ++dim)
  {
    nsizes[dim] = 2;
    numberOfRegions *= nsizes[dim];
  }
  FixedArray<unsigned int, ImageDimension> subIndices;
  /// }}}

  // Prepare filter to paste the different regions into output.
  using PasteFilterType = itk::PasteImageFilter<ImageType>;
  typename PasteFilterType::Pointer pasteFilter = PasteFilterType::New();
  pasteFilter->SetSourceImage(inputPtr);
  pasteFilter->SetDestinationImage(outputPtr);
  pasteFilter->InPlaceOn();

  using RegionType = typename ImageType::RegionType;
  ProgressReporter progress(this, 0, numberOfRegions);
  for (unsigned int n = 0; n < numberOfRegions; ++n)
  {
    subIndices = Ind2Sub<ImageDimension>(n, nsizes);
    RegionType                    zoneRegion;
    typename ImageType::SizeType  zoneSize = inputSize;
    typename ImageType::IndexType inputIndex = inputOriginIndex;
    typename ImageType::IndexType outputIndex = indexRequested;
    // We have to avoid break symmetry and the hermitian property.
    // So the output of a ComplexInverseFFT will generate complex images with non-zero imaginary part.
    for (unsigned int dim = 0; dim < ImageDimension; ++dim)
    {
      if (subIndices[dim] == 0) // positive frequencies
      {
        outputIndex[dim] = indexRequested[dim];
      }
      else // negative frequencies
      {
        outputIndex[dim] = indexRequested[dim] + outputSize[dim] - zoneSize[dim];
      }
    }
    zoneRegion.SetIndex(inputIndex);
    zoneRegion.SetSize(zoneSize);
    itkDebugMacro(<< "n:" << n << " region: " << zoneRegion);

    pasteFilter->SetSourceRegion(zoneRegion);
    pasteFilter->SetDestinationIndex(outputIndex);
    if (n == numberOfRegions - 1) // Graft the output.
    {
      pasteFilter->GraftOutput(outputPtr);
      pasteFilter->Update();
      this->GraftOutput(pasteFilter->GetOutput());
    }
    else // update output
    {
      pasteFilter->Update();
      outputPtr = pasteFilter->GetOutput();
    }
    progress.CompletedPixel();
  }
}

/**
 * GenerateInputRequesteRegion
 */
template <typename TImageType>
void
FrequencyExpandImageFilter<TImageType>::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  auto *             inputPtr = const_cast<TImageType *>(this->GetInput());
  const TImageType * outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro(inputPtr != nullptr);
  itkAssertInDebugAndIgnoreInReleaseMacro(outputPtr);

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
FrequencyExpandImageFilter<TImageType>::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  const TImageType * inputPtr = this->GetInput();
  TImageType *       outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro(inputPtr);
  itkAssertInDebugAndIgnoreInReleaseMacro(outputPtr != nullptr);

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

  // typename TImageType::SpacingType inputOriginShift;
  for (unsigned int i = 0; i < TImageType::ImageDimension; i++)
  {
    outputSpacing[i] = inputSpacing[i] / m_ExpandFactors[i];
    outputSize[i] = inputSize[i] * static_cast<SizeValueType>(m_ExpandFactors[i]);
    outputStartIndex[i] = inputStartIndex[i];
    // outputStartIndex[i] = inputStartIndex[i] * (IndexValueType)m_ExpandFactors[i];
    // const double fraction = (double)( m_ExpandFactors[i] - 1 ) / (double)m_ExpandFactors[i];
    // inputOriginShift[i] = -( inputSpacing[i] / 2.0 ) * fraction;
  }

  // const typename TImageType::DirectionType inputDirection    = inputPtr->GetDirection();
  // const typename TImageType::SpacingType   outputOriginShift = inputDirection * inputOriginShift;
  //
  // outputOrigin = inputOrigin + outputOriginShift;
  outputOrigin = inputOrigin;

  outputPtr->SetSpacing(outputSpacing);
  outputPtr->SetOrigin(outputOrigin);

  typename TImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}
} // end namespace itk

#endif
