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
#ifndef itkShrinkDecimateImageFilter_hxx
#define itkShrinkDecimateImageFilter_hxx

#include "itkImageScanlineIterator.h"
#include <numeric>
#include <functional>

namespace itk
{
template <class TInputImage, class TOutputImage>
ShrinkDecimateImageFilter<TInputImage, TOutputImage>::ShrinkDecimateImageFilter()
{
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    m_ShrinkFactors[j] = 1;
  }

  this->DynamicMultiThreadingOn();
}

template <class TInputImage, class TOutputImage>
void
ShrinkDecimateImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Shrink Factor: ";
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    os << m_ShrinkFactors[j] << " ";
  }
  os << std::endl;
}

template <class TInputImage, class TOutputImage>
void
ShrinkDecimateImageFilter<TInputImage, TOutputImage>::SetShrinkFactors(unsigned int factor)
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

template <class TInputImage, class TOutputImage>
void
ShrinkDecimateImageFilter<TInputImage, TOutputImage>::SetShrinkFactor(unsigned int i, unsigned int factor)
{
  if (m_ShrinkFactors[i] == factor)
  {
    return;
  }

  this->Modified();
  m_ShrinkFactors[i] = factor;
}

/**
 * ThreadedGenerateData
 */
template <typename TInputImage, typename TOutputImage>
void
ShrinkDecimateImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  // Get the input and output pointers
  OutputImagePointer     outputPtr = this->GetOutput();
  const InputImageType * inputPtr = this->GetInput();

  // Iterator for walking the output
  using OutputIterator = ImageScanlineIterator<TOutputImage>;

  OutputIterator outIt(outputPtr, outputRegionForThread);

  // Report progress on a per scanline basis
  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if (size0 == 0)
  {
    return;
  }

  // const typename OutputImageType::IndexType outputOriginIndex = outputPtr->GetLargestPossibleRegion().GetIndex();
  // const typename InputImageType::IndexType  inputOriginIndex  = inputPtr->GetLargestPossibleRegion().GetIndex();
  // Walk the output region, and interpolate the input image
  while (!outIt.IsAtEnd())
  {
    while (!outIt.IsAtEndOfLine())
    {
      const typename OutputImageType::IndexType outputIndex = outIt.GetIndex();
      // Determine the input pixel location associated with this output
      // pixel at the start of the scanline.
      //
      // Don't need to check for division by zero because the factors are
      // clamped to be minimum for 1.
      typename InputImageType::IndexType inputIndex;
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        // inputIndex[j] = (outputIndex[j] -  outputOriginIndex[j]) * m_ShrinkFactors[j] + inputOriginIndex[j];
        inputIndex[j] = outputIndex[j] * m_ShrinkFactors[j];
      }
      outIt.Set(static_cast<typename TOutputImage::PixelType>(inputPtr->GetPixel(inputIndex)));
      ++outIt;
    }

    outIt.NextLine();
  }
}

template <class TInputImage, class TOutputImage>
void
ShrinkDecimateImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  auto *               inputPtr = const_cast<TInputImage *>(this->GetInput());
  const TOutputImage * outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro(inputPtr != nullptr);
  itkAssertInDebugAndIgnoreInReleaseMacro(outputPtr);

  // Compute the input requested region (size and start index)
  // Use the image transformations to insure an input requested region
  // that will provide the proper range
  unsigned int                             i;
  const typename TOutputImage::SizeType &  outputRequestedRegionSize = outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType & outputRequestedRegionStartIndex = outputPtr->GetRequestedRegion().GetIndex();

  // Convert the factor for convenient multiplication
  typename TOutputImage::SizeType factorSize;
  for (i = 0; i < TInputImage::ImageDimension; i++)
  {
    factorSize[i] = m_ShrinkFactors[i];
  }

  InputIndexType   inputRequestedRegionIndex;
  OutputOffsetType offsetIndex;

  typename TInputImage::SizeType inputRequestedRegionSize;

  // Use this index to compute the offset everywhere in this class
  OutputIndexType outputIndex = outputPtr->GetLargestPossibleRegion().GetIndex();

  // We wish to perform the following mapping of outputIndex to
  // inputIndex on all points in our region
  typename TOutputImage::PointType tempPoint;
  outputPtr->TransformIndexToPhysicalPoint(outputIndex, tempPoint);
  InputIndexType inputIndex{ inputPtr->TransformPhysicalPointToIndex(tempPoint) };

  // Given that the size is scaled by a constant factor eq:
  // inputIndex = outputIndex * factorSize
  // is equivalent up to a fixed offset which we now compute
  OffsetValueType zeroOffset = 0;
  for (i = 0; i < TInputImage::ImageDimension; i++)
  {
    offsetIndex[i] = inputIndex[i] - outputIndex[i] * m_ShrinkFactors[i];
    // It is plausible that due to small amounts of loss of numerical
    // precision that the offset it negative, this would cause sampling
    // out of out region, this is insurance against that possibility
    offsetIndex[i] = std::max(zeroOffset, offsetIndex[i]);
  }

  inputRequestedRegionIndex = outputRequestedRegionStartIndex * factorSize + offsetIndex;
  // originally this was
  // inputRequestedRegionSize = outputRequestedRegionSize * factorSize;
  // but since we don't sample edge to edge, we can reduce the size
  for (i = 0; i < TInputImage::ImageDimension; ++i)
  {
    inputRequestedRegionSize[i] = (outputRequestedRegionSize[i] - 1) * factorSize[i] + 1;
  }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetIndex(inputRequestedRegionIndex);
  inputRequestedRegion.SetSize(inputRequestedRegionSize);
  inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion());

  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

template <class TInputImage, class TOutputImage>
void
ShrinkDecimateImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  const TInputImage * inputPtr = this->GetInput();
  TOutputImage *      outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro(inputPtr);
  itkAssertInDebugAndIgnoreInReleaseMacro(outputPtr != nullptr);

  // Compute the output spacing, the output image size, and the
  // output image start index
  const typename TInputImage::SpacingType & inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType &    inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType &   inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  typename TOutputImage::SpacingType outputSpacing(inputSpacing);
  typename TOutputImage::SizeType    outputSize;
  typename TOutputImage::IndexType   outputStartIndex;
  for (unsigned int i = 0; i < TOutputImage::ImageDimension; i++)
  {
    outputSpacing[i] *= m_ShrinkFactors[i];

    // Round down so that all output pixels fit input input region
    outputSize[i] = static_cast<SizeValueType>(
      std::floor(static_cast<double>(inputSize[i]) / static_cast<double>(m_ShrinkFactors[i])));

    if (outputSize[i] < 1)
    {
      itkExceptionMacro("InputImage is too small! An output pixel does not map to a whole input bin.");
    }

    // Because of the later origin shift this starting index is not
    // critical
    outputStartIndex[i] = static_cast<IndexValueType>(
      std::ceil(static_cast<double>(inputStartIndex[i]) / static_cast<double>(m_ShrinkFactors[i])));
  }

  outputPtr->SetSpacing(outputSpacing);

  // Compute origin offset
  // The physical center's of the input and output should be the same
  ContinuousIndex<SpacePrecisionType, TOutputImage::ImageDimension> inputCenterIndex;
  ContinuousIndex<SpacePrecisionType, TOutputImage::ImageDimension> outputCenterIndex;
  for (unsigned int i = 0; i < TOutputImage::ImageDimension; i++)
  {
    inputCenterIndex[i] = inputStartIndex[i] + (inputSize[i] - 1) / 2.0;
    outputCenterIndex[i] = outputStartIndex[i] + (outputSize[i] - 1) / 2.0;
  }

  typename TOutputImage::PointType inputCenterPoint;
  typename TOutputImage::PointType outputCenterPoint;
  inputPtr->TransformContinuousIndexToPhysicalPoint(inputCenterIndex, inputCenterPoint);
  outputPtr->TransformContinuousIndexToPhysicalPoint(outputCenterIndex, outputCenterPoint);

  const typename TOutputImage::PointType & inputOrigin = inputPtr->GetOrigin();
  typename TOutputImage::PointType         outputOrigin;
  outputOrigin = inputOrigin + (inputCenterPoint - outputCenterPoint);
  outputPtr->SetOrigin(outputOrigin);

  // Set region
  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}
} // end namespace itk

#endif
