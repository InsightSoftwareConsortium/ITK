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
#ifndef itkFrequencyExpandViaInverseFFTImageFilter_hxx
#define itkFrequencyExpandViaInverseFFTImageFilter_hxx


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
  m_ChangeInformation = ChangeInformationFilterType::New();
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
  // m_ForwardFFT->GraftOutput(outputPtr);
  m_ForwardFFT->Update();
  // this->GraftOutput(m_ForwardFFT->GetOutput());
  // outputPtr = m_ForwardFFT->GetOutput();
  // Metadata of the output of pipeline is not what we need. We set it to the OutputInformation.
  m_ChangeInformation->SetInput(m_ForwardFFT->GetOutput());
  m_ChangeInformation->ChangeOriginOn();
  m_ChangeInformation->ChangeSpacingOn();
  m_ChangeInformation->SetOutputOrigin(outputPtr->GetOrigin());
  m_ChangeInformation->SetOutputSpacing(outputPtr->GetSpacing());
  m_ChangeInformation->GraftOutput(outputPtr);
  m_ChangeInformation->Update();
  this->GraftOutput(m_ChangeInformation->GetOutput());

  // ImageAlgorithm::Copy(m_ForwardFFT->GetOutput(), outputPtr.GetPointer(),
  //   outputPtr->GetRequestedRegion(),
  //   outputPtr->GetRequestedRegion() );
}

/**
 * GenerateInputRequestedRegion
 */
template <typename TImageType>
void
FrequencyExpandViaInverseFFTImageFilter<TImageType>::GenerateInputRequestedRegion()
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
    inputRequestedRegionSize[i] = static_cast<SizeValueType>(
      std::ceil(static_cast<double>(outputRequestedRegionSize[i]) / static_cast<double>(m_ExpandFactors[i])) + 1);

    inputRequestedRegionStartIndex[i] = static_cast<SizeValueType>(
      std::floor(static_cast<double>(outputRequestedRegionStartIndex[i]) / static_cast<double>(m_ExpandFactors[i])));
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

  for (unsigned int i = 0; i < TImageType::ImageDimension; i++)
  {
    outputSpacing[i] = inputSpacing[i] / m_ExpandFactors[i];
    outputSize[i] = inputSize[i] * static_cast<SizeValueType>(m_ExpandFactors[i]);
    outputStartIndex[i] = inputStartIndex[i];
  }

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
