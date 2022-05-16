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
#ifndef itkHalfHermitianToRealInverseFFTImageFilter_hxx
#define itkHalfHermitianToRealInverseFFTImageFilter_hxx


namespace itk
{
template <typename TInputImage, typename TOutputImage>
HalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>::HalfHermitianToRealInverseFFTImageFilter()
{
  this->ActualXDimensionIsOddOff();
}

template <typename TInputImage, typename TOutputImage>
void
HalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  typename InputImageType::ConstPointer inputPtr = this->GetInput();
  typename OutputImageType::Pointer     outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  // This is all based on the same function in itk::ShrinkImageFilter.
  // ShrinkImageFilter also modifies the image spacing, but spacing
  // has no meaning in the result of an FFT. For an IFFT, since the
  // spacing is propagated to the complex result, we can use the spacing
  // from the input to propagate back to the output.
  const typename InputImageType::SizeType &  inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename InputImageType::IndexType & inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  typename OutputImageType::SizeType  outputSize;
  typename OutputImageType::IndexType outputStartIndex;

  // In 4.3.4 of the FFTW documentation, they indicate the size of
  // of a real-to-complex FFT is N * N ... + (N /2+1)
  //                              1   2        d
  // complex numbers.
  // Going from complex to real, you know the output is at least
  // twice the size in the last dimension as the input, but it might
  // be 2*size+1.  Consequently, you need to check whether the actual
  // X dimension is even or odd.
  outputSize[0] = (inputSize[0] - 1) * 2;
  if (this->GetActualXDimensionIsOdd())
  {
    outputSize[0]++;
  }

  outputStartIndex[0] = inputStartIndex[0];

  for (unsigned int i = 1; i < OutputImageType::ImageDimension; ++i)
  {
    outputSize[i] = inputSize[i];
    outputStartIndex[i] = inputStartIndex[i];
  }
  const typename OutputImageType::RegionType outputLargestPossibleRegion(outputStartIndex, outputSize);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}

template <typename TInputImage, typename TOutputImage>
void
HalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  typename InputImageType::Pointer inputPtr = const_cast<InputImageType *>(this->GetInput());
  if (inputPtr)
  {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
  }
}

template <typename TInputImage, typename TOutputImage>
void
HalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()->SetRequestedRegion(this->GetOutput()->GetLargestPossibleRegion());
}

template <typename TInputImage, typename TOutputImage>
SizeValueType
HalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>::GetSizeGreatestPrimeFactor() const
{
  return 2;
}

} // namespace itk
#endif
