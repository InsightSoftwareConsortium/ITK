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
#ifndef itkCropImageFilter_hxx
#define itkCropImageFilter_hxx


namespace itk
{
template <typename TInputImage, typename TOutputImage>
void
CropImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  const TInputImage * inputPtr = this->GetInput();

  if (!inputPtr)
  {
    return;
  }

  // Compute the new region size
  SizeType             sz;
  OutputImageIndexType idx;

  InputImageSizeType  input_sz = inputPtr->GetLargestPossibleRegion().GetSize();
  InputImageIndexType input_idx = inputPtr->GetLargestPossibleRegion().GetIndex();

  for (unsigned int i = 0; i < InputImageDimension; ++i)
  {
    idx[i] = input_idx[i] + m_LowerBoundaryCropSize[i];

    assert(input_sz[i] >= (m_UpperBoundaryCropSize[i] + m_LowerBoundaryCropSize[i]));
    sz[i] = input_sz[i] - (m_UpperBoundaryCropSize[i] + m_LowerBoundaryCropSize[i]);
  }

  const OutputImageRegionType croppedRegion(idx, sz);

  // Set extraction region in the superclass
  this->SetExtractionRegion(croppedRegion);

  Superclass::GenerateOutputInformation();
}


template <typename TInputImage, typename TOutputImage>
void
CropImageFilter<TInputImage, TOutputImage>::VerifyInputInformation() const
{
  Superclass::VerifyInputInformation();

  const TInputImage * inputPtr = this->GetInput();

  InputImageSizeType input_sz = inputPtr->GetLargestPossibleRegion().GetSize();

  for (unsigned int i = 0; i < InputImageDimension; ++i)
  {
    if (input_sz[i] < (m_UpperBoundaryCropSize[i] + m_LowerBoundaryCropSize[i]))
    {
      itkExceptionMacro("The input image's size " << input_sz << " is less than the total of the crop size!");
    }
  }
}

template <typename TInputImage, typename TOutputImage>
void
CropImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "UpperBoundaryCropSize: " << m_UpperBoundaryCropSize << std::endl;
  os << indent << "LowerBoundaryCropSize: " << m_LowerBoundaryCropSize << std::endl;
}
} // end namespace itk

#endif
