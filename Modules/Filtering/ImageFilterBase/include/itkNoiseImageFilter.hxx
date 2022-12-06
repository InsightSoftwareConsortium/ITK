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
  unsigned int i;

  ZeroFluxNeumannBoundaryCondition<InputImageType> nbc;

  ConstNeighborhoodIterator<InputImageType> bit;
  ImageRegionIterator<OutputImageType>      it;

  // Allocate output
  typename OutputImageType::Pointer     output = this->GetOutput();
  typename InputImageType::ConstPointer input = this->GetInput();

  // Find the data-set boundary "faces"
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>                        bC;
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList =
    bC(input, outputRegionForThread, this->GetRadius());

  TotalProgressReporter progress(this, output->GetRequestedRegion().GetNumberOfPixels());

  InputRealType value;
  InputRealType sum;
  InputRealType sumOfSquares;
  InputRealType var;
  InputRealType num;

  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for (const auto & face : faceList)
  {
    bit = ConstNeighborhoodIterator<InputImageType>(this->GetRadius(), input, face);
    unsigned int neighborhoodSize = bit.Size();
    num = static_cast<InputRealType>(bit.Size());

    it = ImageRegionIterator<OutputImageType>(output, face);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    while (!bit.IsAtEnd())
    {
      sum = NumericTraits<InputRealType>::ZeroValue();
      sumOfSquares = NumericTraits<InputRealType>::ZeroValue();
      for (i = 0; i < neighborhoodSize; ++i)
      {
        value = static_cast<InputRealType>(bit.GetPixel(i));
        sum += value;
        sumOfSquares += (value * value);
      }

      // calculate the standard deviation value
      var = (sumOfSquares - (sum * sum / num)) / (num - 1.0);
      it.Set(static_cast<OutputPixelType>(std::sqrt(var)));

      ++bit;
      ++it;
      progress.CompletedPixel();
    }
  }
}
} // end namespace itk

#endif
