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
#ifndef itkNoiseImageFilter_hxx
#define itkNoiseImageFilter_hxx

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkOffset.h"
#include "itkTotalProgressReporter.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
NoiseImageFilter<TInputImage, TOutputImage>::NoiseImageFilter()
{
  this->DynamicMultiThreadingOn();
  this->ThreaderUpdateProgressOff();
}

template <typename TInputImage, typename TOutputImage>
void
NoiseImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  ZeroFluxNeumannBoundaryCondition<InputImageType> nbc;

  // Allocate output
  const typename OutputImageType::Pointer     output = this->GetOutput();
  const typename InputImageType::ConstPointer input = this->GetInput();

  // Find the data-set boundary "faces"
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>                              bC;
  const typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList =
    bC(input, outputRegionForThread, this->GetRadius());

  TotalProgressReporter progress(this, output->GetRequestedRegion().GetNumberOfPixels());

  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for (const auto & face : faceList)
  {
    ConstNeighborhoodIterator<InputImageType> bit =
      ConstNeighborhoodIterator<InputImageType>(this->GetRadius(), input, face);
    const unsigned int neighborhoodSize = bit.Size();
    InputRealType      num = static_cast<InputRealType>(bit.Size());

    ImageRegionIterator<OutputImageType> it = ImageRegionIterator<OutputImageType>(output, face);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    while (!bit.IsAtEnd())
    {
      InputRealType sum = InputRealType{};
      InputRealType sumOfSquares = InputRealType{};
      for (unsigned int i = 0; i < neighborhoodSize; ++i)
      {
        InputRealType value = static_cast<InputRealType>(bit.GetPixel(i));
        sum += value;
        sumOfSquares += (value * value);
      }

      // calculate the standard deviation value
      InputRealType var = (sumOfSquares - (sum * sum / num)) / (num - 1.0);
      it.Set(static_cast<OutputPixelType>(std::sqrt(var)));

      ++bit;
      ++it;
      progress.CompletedPixel();
    }
  }
}
} // end namespace itk

#endif
