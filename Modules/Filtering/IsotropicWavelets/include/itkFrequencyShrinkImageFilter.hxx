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
#ifndef itkFrequencyShrinkImageFilter_hxx
#define itkFrequencyShrinkImageFilter_hxx

#include <itkFrequencyShrinkImageFilter.h>
#include <itkImageScanlineIterator.h>
#include <itkProgressReporter.h>
#include <numeric>
#include <functional>
#include "itkInd2Sub.h"
#include <itkPasteImageFilter.h>

namespace itk
{

template <class TImageType>
FrequencyShrinkImageFilter<TImageType>::FrequencyShrinkImageFilter()
{
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    m_ShrinkFactors[j] = 1;
  }
}

template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Shrink Factor: ";
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    os << m_ShrinkFactors[j] << " ";
  }
  os << std::endl;
}

template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::SetShrinkFactors(unsigned int factor)
{
  unsigned int j;

  for (j = 0; j < ImageDimension; j++)
  {
    if (factor != m_ShrinkFactors[j])
    {
      break;
    }
  }
  if (j < ImageDimension)
  {
    this->Modified();
    for (j = 0; j < ImageDimension; j++)
    {
      m_ShrinkFactors[j] = factor;
      if (m_ShrinkFactors[j] < 1)
      {
        m_ShrinkFactors[j] = 1;
      }
    }
  }
}

template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::SetShrinkFactor(unsigned int i, unsigned int factor)
{
  if (m_ShrinkFactors[i] == factor)
  {
    return;
  }

  this->Modified();
  m_ShrinkFactors[i] = factor;
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
template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::GenerateData()
{
  // Get the input and output pointers
  const ImageType *           inputPtr = this->GetInput();
  typename ImageType::Pointer outputPtr = this->GetOutput();
  this->AllocateOutputs();
  // outputPtr->SetBufferedRegion(outputPtr->GetLargestPossibleRegion());
  outputPtr->FillBuffer(0);

  typename TImageType::SizeType inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  typename TImageType::SizeType outputSize = outputPtr->GetLargestPossibleRegion().GetSize();
  typename TImageType::SizeType lowFreqsOfInput;
  for (unsigned int dim = 0; dim < TImageType::ImageDimension; ++dim)
  {
    lowFreqsOfInput[dim] = Math::Floor<SizeValueType>(outputSize[dim] / 2.0);
  }

  const typename TImageType::IndexType indexRequested = outputPtr->GetLargestPossibleRegion().GetIndex();
  // Manage ImageDimension array linearly:{{{
  std::array<unsigned int, ImageDimension> nsizes;
  unsigned int                             numberOfRegions = 1;
  for (unsigned int dim = 0; dim < ImageDimension; ++dim)
  {
    nsizes[dim] = 2;
    numberOfRegions *= nsizes[dim];
  }
  std::array<unsigned int, ImageDimension> subIndices;
  /// }}}

  // Prepare filter to paste the different regions into output.
  typedef itk::PasteImageFilter<ImageType> PasteFilterType;
  typename PasteFilterType::Pointer        pasteFilter = PasteFilterType::New();
  pasteFilter->SetSourceImage(inputPtr);
  pasteFilter->SetDestinationImage(outputPtr);
  pasteFilter->InPlaceOn();

  typedef typename ImageType::RegionType RegionType;
  ProgressReporter                       progress(this, 0, numberOfRegions);

  for (unsigned int n = 0; n < numberOfRegions; ++n)
  {
    subIndices = itk::Ind2Sub<ImageDimension>(n, nsizes);
    RegionType                     zoneRegion;
    typename ImageType::SizeType   zoneSize;
    typename TImageType::IndexType inputIndex = indexRequested;
    typename TImageType::IndexType outputIndex = indexRequested;
    // Note that lowFreqsOfInput is inputSize/2 if outputSize is even, (outputSize - 1)/2 if odd.
    for (unsigned int dim = 0; dim < ImageDimension; ++dim)
    {
      if (subIndices[dim] == 0) // positive frequencies
      {
        zoneSize[dim] = lowFreqsOfInput[dim] + 1;
        inputIndex[dim] = 0;
        outputIndex[dim] = 0;
      }
      else // negative frequencies
      {
        zoneSize[dim] = lowFreqsOfInput[dim] - 1;
        inputIndex[dim] = inputSize[dim] - zoneSize[dim];
        outputIndex[dim] = outputSize[dim] - zoneSize[dim];
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

template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  ImagePointer inputPtr = const_cast<TImageType *>(this->GetInput());
  ImagePointer outputPtr = this->GetOutput();

  // The filter chops high frequencys [0 1...H,H-1 H-2...1].
  // We need the whole input image, indepently of the RequestedRegion.
  inputPtr->SetRequestedRegion(inputPtr->GetLargestPossibleRegion());
}

template <class TImageType>
void
FrequencyShrinkImageFilter<TImageType>::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  ImageConstPointer inputPtr = this->GetInput();
  ImagePointer      outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  // Compute the output spacing, the output image size, and the
  // output image start index
  const typename TImageType::SpacingType & inputSpacing = inputPtr->GetSpacing();
  const typename TImageType::SizeType &    inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImageType::IndexType &   inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  ContinuousIndex<double, ImageDimension> inputIndexOutputOrigin;

  typename TImageType::SpacingType outputSpacing(inputSpacing);
  typename TImageType::SizeType    outputSize;
  typename TImageType::PointType   outputOrigin;
  typename TImageType::IndexType   outputStartIndex;

  for (unsigned int i = 0; i < TImageType::ImageDimension; i++)
  {
    outputSpacing[i] *= m_ShrinkFactors[i];

    inputIndexOutputOrigin[i] = 0.5 * (m_ShrinkFactors[i] - 1);

    outputStartIndex[i] = Math::Ceil<SizeValueType>(inputStartIndex[i] / static_cast<double>(m_ShrinkFactors[i]));

    // Round down so that all output pixels fit input input region
    outputSize[i] = Math::Floor<SizeValueType>(
      static_cast<double>(inputSize[i] - outputStartIndex[i] * m_ShrinkFactors[i] + inputStartIndex[i]) /
      static_cast<double>(m_ShrinkFactors[i]));

    if (outputSize[i] < 1)
    {
      itkExceptionMacro("InputImage is too small! An output pixel does not map to a whole input bin.");
    }
  }

  inputPtr->TransformContinuousIndexToPhysicalPoint(inputIndexOutputOrigin, outputOrigin);

  outputPtr->SetSpacing(outputSpacing);
  outputPtr->SetOrigin(outputOrigin);

  // Set region
  typename TImageType::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}

} // end namespace itk

#endif
