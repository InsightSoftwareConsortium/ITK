/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkEigenToMeasureImageFilter_hxx
#define itkEigenToMeasureImageFilter_hxx

#include "itkEigenToMeasureImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
void
EigenToMeasureImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(const OutputImageRegionType & regionForThread)
{
  const InputImageType *        inputPtr = this->GetInput(0);
  OutputImageType *             outputPtr = this->GetOutput(0);
  const MaskSpatialObjectType * maskPointer = this->GetMask();
  typename InputImageType::PointType point;

  /* Setup iterator */
  ImageRegionConstIteratorWithIndex<TInputImage> inputIt(inputPtr, regionForThread);
  ImageRegionIterator<OutputImageType>           outputIt(outputPtr, regionForThread);

  while (!inputIt.IsAtEnd())
  {
    inputPtr->TransformIndexToPhysicalPoint(inputIt.GetIndex(), point);
    if ((!maskPointer) || (maskPointer->IsInsideInObjectSpace(point)))
    {
      outputIt.Set(this->ProcessPixel(inputIt.Get()));
    }
    else
    {
      outputIt.Set(NumericTraits<OutputImagePixelType>::Zero);
    }

    ++inputIt;
    ++outputIt;
  }
}

} // namespace itk

#endif /* itkEigenToMeasureImageFilter_hxx */
