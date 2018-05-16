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
#ifndef itkFrequencyShrinkViaInverseFFTImageFilter_hxx
#define itkFrequencyShrinkViaInverseFFTImageFilter_hxx

#include <itkFrequencyShrinkViaInverseFFTImageFilter.h>

namespace itk
{
template <class TImageType>
FrequencyShrinkViaInverseFFTImageFilter<TImageType>::FrequencyShrinkViaInverseFFTImageFilter()
{
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    m_ShrinkFactors[j] = 1;
  }
  m_InverseFFT = InverseFFTFilterType::New();
  m_ForwardFFT = ForwardFFTFilterType::New();
  m_Shrinker = ShrinkFilterType::New();
  m_ChangeInformation = ChangeInformationFilterType::New();
}

template <class TImageType>
void
FrequencyShrinkViaInverseFFTImageFilter<TImageType>::PrintSelf(std::ostream & os, Indent indent) const
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
FrequencyShrinkViaInverseFFTImageFilter<TImageType>::SetShrinkFactors(unsigned int factor)
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
FrequencyShrinkViaInverseFFTImageFilter<TImageType>::SetShrinkFactor(unsigned int i, unsigned int factor)
{
  if (m_ShrinkFactors[i] == factor)
  {
    return;
  }

  this->Modified();
  m_ShrinkFactors[i] = factor;
}

/**
 * Perform an expensive
 */
template <class TImageType>
void
FrequencyShrinkViaInverseFFTImageFilter<TImageType>::GenerateData()
{
  // Get the input and output pointers
  typename ImageType::Pointer outputPtr = this->GetOutput();

  m_InverseFFT->SetInput(this->GetInput());
  /**** Downsample in spatial domain ***/
  m_Shrinker->SetInput(m_InverseFFT->GetOutput());
  m_Shrinker->SetShrinkFactors(this->m_ShrinkFactors);

  m_ForwardFFT->SetInput(m_Shrinker->GetOutput());
  // Metadata of the output of pipeline is not what we need. We set it to the OutputInformation.
  m_ChangeInformation->SetInput(m_ForwardFFT->GetOutput());
  m_ChangeInformation->ChangeOriginOn();
  m_ChangeInformation->ChangeSpacingOn();
  m_ChangeInformation->SetOutputOrigin(outputPtr->GetOrigin());
  m_ChangeInformation->SetOutputSpacing(outputPtr->GetSpacing());
  m_ChangeInformation->GraftOutput(outputPtr);
  m_ChangeInformation->Update();
  this->GraftOutput(m_ChangeInformation->GetOutput());
}

template <class TImageType>
void
FrequencyShrinkViaInverseFFTImageFilter<TImageType>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  auto * inputPtr = const_cast<TImageType *>(this->GetInput());

  itkAssertInDebugAndIgnoreInReleaseMacro(inputPtr != nullptr);

  // The filter chops high frequencys [0 1...H,H-1 H-2...1].
  // We need the whole input image, indepently of the RequestedRegion.
  inputPtr->SetRequestedRegion(inputPtr->GetLargestPossibleRegion());
}

template <class TImageType>
void
FrequencyShrinkViaInverseFFTImageFilter<TImageType>::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  const TImageType * inputPtr = this->GetInput();
  TImageType *       outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro(inputPtr);
  itkAssertInDebugAndIgnoreInReleaseMacro(outputPtr != nullptr);

  // Compute the output spacing, the output image size, and the
  // output image start index
  const typename TImageType::SpacingType & inputSpacing = inputPtr->GetSpacing();
  const typename TImageType::SizeType &    inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImageType::IndexType &   inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();
  const typename TImageType::PointType &   inputOrigin = inputPtr->GetOrigin();

  // ContinuousIndex<double,ImageDimension> inputIndexOutputOrigin;

  typename TImageType::SpacingType outputSpacing(inputSpacing);
  typename TImageType::SizeType    outputSize;
  typename TImageType::PointType   outputOrigin;
  typename TImageType::IndexType   outputStartIndex;
  // TODO Check if you want to modify metada in this filter.
  for (unsigned int i = 0; i < TImageType::ImageDimension; i++)
  {
    outputSpacing[i] *= m_ShrinkFactors[i];
    outputStartIndex[i] = inputStartIndex[i];
    outputSize[i] =
      Math::Floor<SizeValueType>(static_cast<double>(inputSize[i]) / static_cast<double>(m_ShrinkFactors[i]));

    if (outputSize[i] < 1)
    {
      itkExceptionMacro("InputImage is too small! An output pixel does not map to a whole input bin.");
    }
  }

  // inputPtr->TransformContinuousIndexToPhysicalPoint(inputIndexOutputOrigin, outputOrigin);
  outputOrigin = inputOrigin;

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
