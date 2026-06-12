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
#ifndef itkPocketFFTInverse1DFFTImageFilter_hxx
#define itkPocketFFTInverse1DFFTImageFilter_hxx


#include "itkInverse1DFFTImageFilter.hxx"
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkIndent.h"
#include "itkMetaDataObject.h"
#include "itkMacro.h"
#include "itkPocketFFTCommon.h"

#include <vector>

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
PocketFFTInverse1DFFTImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  this->AllocateOutputs();

  // get pointers to the input and output
  const typename Superclass::InputImageType * input = this->GetInput();
  typename Superclass::OutputImageType *      output = this->GetOutput();

  const typename Superclass::InputImageType::SizeType & inputSize = input->GetRequestedRegion().GetSize();

  const unsigned int direction = this->GetDirection();
  const unsigned int vectorSize = inputSize[direction];

  MultiThreaderBase * multiThreader = this->GetMultiThreader();
  multiThreader->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  multiThreader->template ParallelizeImageRegionRestrictDirection<TOutputImage::ImageDimension>(
    direction,
    output->GetRequestedRegion(),
    [input, output, direction, vectorSize](const typename OutputImageType::RegionType & lambdaRegion) {
      ImageLinearConstIteratorWithIndex inputIt(input, lambdaRegion);
      ImageLinearIteratorWithIndex      outputIt(output, lambdaRegion);

      inputIt.SetDirection(direction);
      outputIt.SetDirection(direction);

      using OutputPixelType = typename TOutputImage::PixelType;
      std::vector<std::complex<OutputPixelType>>                    inputBuffer(vectorSize);
      typename std::vector<std::complex<OutputPixelType>>::iterator inputBufferIt = inputBuffer.begin();
      // fft is done in-place
      typename std::vector<std::complex<OutputPixelType>>::iterator outputBufferIt = inputBuffer.begin();

      // for every fft line
      for (inputIt.GoToBegin(), outputIt.GoToBegin(); !inputIt.IsAtEnd(); outputIt.NextLine(), inputIt.NextLine())
      {
        // copy the input line into our buffer
        inputIt.GoToBeginOfLine();
        inputBufferIt = inputBuffer.begin();
        while (!inputIt.IsAtEndOfLine())
        {
          *inputBufferIt = inputIt.Get();
          ++inputIt;
          ++inputBufferIt;
        }

        // do the transform
        PocketFFTCommon::Transform1D(
          inputBuffer.data(), vectorSize, false, OutputPixelType{ 1 } / static_cast<OutputPixelType>(vectorSize));

        // copy the output from the buffer into our line
        outputBufferIt = inputBuffer.begin();
        outputIt.GoToBeginOfLine();
        while (!outputIt.IsAtEndOfLine())
        {
          outputIt.Set(outputBufferIt->real());
          ++outputIt;
          ++outputBufferIt;
        }
      }
    },
    this);
}

} // end namespace itk

#endif
