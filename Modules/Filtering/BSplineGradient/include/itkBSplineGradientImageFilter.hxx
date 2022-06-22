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
#ifndef itkBSplineGradientImageFilter_hxx
#define itkBSplineGradientImageFilter_hxx


#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

template <typename TInputImage, typename TOutputValueType, typename TCoordRep, typename TCoefficientType>
BSplineGradientImageFilter<TInputImage, TOutputValueType, TCoordRep, TCoefficientType>::BSplineGradientImageFilter() =
  default;


template <typename TInputImage, typename TOutputValueType, typename TCoordRep, typename TCoefficientType>
void
BSplineGradientImageFilter<TInputImage, TOutputValueType, TCoordRep, TCoefficientType>::GenerateInputRequestedRegion()
{
  // this filter requires the all of the input image to be in
  // the buffer
  InputImagePointer inputPtr = const_cast<InputImageType *>(this->GetInput());

  if (inputPtr)
  {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
  }
}


template <typename TInputImage, typename TOutputValueType, typename TCoordRep, typename TCoefficientType>
void
BSplineGradientImageFilter<TInputImage, TOutputValueType, TCoordRep, TCoefficientType>::EnlargeOutputRequestedRegion(
  DataObject * output)
{
  // this filter requires the all of the output image to be in
  // the buffer
  OutputImageType * imgData;

  imgData = dynamic_cast<OutputImageType *>(output);
  if (imgData)
  {
    imgData->SetRequestedRegionToLargestPossibleRegion();
  }
}


template <typename TInputImage, typename TOutputValueType, typename TCoordRep, typename TCoefficientType>
void
BSplineGradientImageFilter<TInputImage, TOutputValueType, TCoordRep, TCoefficientType>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  if (!outputPtr)
  {
    return;
  }

  // this filter requires the all of the input image to be in
  // the buffer
  InputImagePointer       inputPtr = const_cast<TInputImage *>(this->GetInput());
  InterpolatorPointerType interpolator = InterpolatorType::New();
  // This will calculate the coefficients.
  interpolator->SetInputImage(inputPtr);

  using IteratorType = typename itk::ImageRegionIteratorWithIndex<OutputImageType>;
  typename OutputImageType::IndexType            index;
  typename InterpolatorType::ContinuousIndexType contIndex;
  unsigned int                                   i;
  IteratorType                                   it(outputPtr, outputRegionForThread);

  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    index = it.GetIndex();
    for (i = 0; i < ImageDimension; ++i)
    {
      contIndex[i] = static_cast<CoordRepType>(index[i]);
    }
    it.Set(interpolator->EvaluateDerivativeAtContinuousIndex(contIndex));
  }
}

} // end namespace itk

#endif
