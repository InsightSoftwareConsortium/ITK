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
#ifndef itkShrinkDecimateImageFilter_hxx
#define itkShrinkDecimateImageFilter_hxx

#include "itkShrinkDecimateImageFilter.h"
#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"
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
ShrinkDecimateImageFilter<TInputImage, TOutputImage>::ThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread,
  ThreadIdType                  threadId)
{
  // Get the input and output pointers
  OutputImagePointer     outputPtr = this->GetOutput();
  const InputImageType * inputPtr = this->GetInput();

  // Iterator for walking the output
  typedef ImageScanlineIterator<TOutputImage> OutputIterator;

  OutputIterator outIt(outputPtr, outputRegionForThread);

  // Report progress on a per scanline basis
  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if (size0 == 0)
  {
    return;
  }
  const size_t     numberOfLinesToProcess = outputRegionForThread.GetNumberOfPixels() / size0;
  ProgressReporter progress(this, threadId, static_cast<SizeValueType>(numberOfLinesToProcess));

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
        // inputIndex[j] = (outputIndex[j] -  outputOriginIndex[j]) * m_ShrinkFactors[j] + inputOriginIndex[j] ;
        inputIndex[j] = outputIndex[j] * m_ShrinkFactors[j];
      }
      outIt.Set(static_cast<typename TOutputImage::PixelType>(inputPtr->GetPixel(inputIndex)));
      ++outIt;
    }
    outIt.NextLine();
    progress.CompletedPixel();
  }
}

template <class TInputImage, class TOutputImage>
void
ShrinkDecimateImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer  inputPtr = const_cast<TInputImage *>(this->GetInput());
  OutputImagePointer outputPtr = this->GetOutput();

  // Compute the input requested region (size and start index)
  // Use the image transformations to insure an input requested region
  // that will provide the proper range
  const typename TOutputImage::SizeType &  outputRequestedRegionSize = outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType & outputRequestedRegionStartIndex = outputPtr->GetRequestedRegion().GetIndex();

  typename TInputImage::IndexType inputIndex0;
  typename TInputImage::SizeType  inputSize;

  for (unsigned int i = 0; i < TInputImage::ImageDimension; ++i)
  {
    inputIndex0[i] = outputRequestedRegionStartIndex[i] * m_ShrinkFactors[i];
    inputSize[i] = outputRequestedRegionSize[i] * m_ShrinkFactors[i];
  }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetIndex(inputIndex0);
  inputRequestedRegion.SetSize(inputSize);

  // actually if we need to crop an exceptions should be thrown!
  // inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() );

  if (!inputPtr->GetLargestPossibleRegion().IsInside(inputRequestedRegion.GetIndex()) ||
      !inputPtr->GetLargestPossibleRegion().IsInside(inputRequestedRegion.GetUpperIndex()))
  {
    itkExceptionMacro("Unexpected error calculating RR");
  }

  itkDebugMacro("InputRequestedRegion: " << inputRequestedRegion);
  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

template <class TInputImage, class TOutputImage>
void
ShrinkDecimateImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  InputImageConstPointer inputPtr = this->GetInput();
  OutputImagePointer     outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  // Compute the output spacing, the output image size, and the
  // output image start index
  const typename TInputImage::SpacingType & inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType &    inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType &   inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  ContinuousIndex<double, ImageDimension> inputIndexOutputOrigin;

  typename TOutputImage::SpacingType outputSpacing(inputSpacing);
  typename TOutputImage::SizeType    outputSize;
  typename TOutputImage::PointType   outputOrigin;
  typename TOutputImage::IndexType   outputStartIndex;

  for (unsigned int i = 0; i < TOutputImage::ImageDimension; i++)
  {
    outputSpacing[i] *= m_ShrinkFactors[i];

    inputIndexOutputOrigin[i] = 0.5 * (m_ShrinkFactors[i] - 1);

    outputStartIndex[i] = Math::Ceil<SizeValueType>(inputStartIndex[i] / static_cast<double>(m_ShrinkFactors[i]));

    // Round down so that all output pixels fit input input region
    outputSize[i] = Math::Floor<SizeValueType>(
      (double)(inputSize[i] - outputStartIndex[i] * m_ShrinkFactors[i] + inputStartIndex[i]) /
      (double)m_ShrinkFactors[i]);

    if (outputSize[i] < 1)
    {
      itkExceptionMacro("InputImage is too small! An output pixel does not map to a whole input bin.");
    }
  }

  inputPtr->TransformContinuousIndexToPhysicalPoint(inputIndexOutputOrigin, outputOrigin);

  outputPtr->SetSpacing(outputSpacing);
  outputPtr->SetOrigin(outputOrigin);

  // Set region
  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}
} // end namespace itk

#endif
