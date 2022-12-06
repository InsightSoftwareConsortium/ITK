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
#ifndef itkNeighborhoodOperatorImageFilter_hxx
#define itkNeighborhoodOperatorImageFilter_hxx


#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkTotalProgressReporter.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage, typename TOperatorValueType>
void
NeighborhoodOperatorImageFilter<TInputImage, TOutputImage, TOperatorValueType>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer inputPtr = const_cast<TInputImage *>(this->GetInput());

  if (!inputPtr)
  {
    return;
  }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius(m_Operator.GetRadius());

  // crop the input requested region at the input's largest possible region
  if (inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()))
  {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
  }
  else
  {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
  }
}

template <typename TInputImage, typename TOutputImage, typename TOperatorValueType>
void
NeighborhoodOperatorImageFilter<TInputImage, TOutputImage, TOperatorValueType>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  using BFC = NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>;
  using FaceListType = typename BFC::FaceListType;

  NeighborhoodInnerProduct<InputImageType, OperatorValueType, ComputingPixelType> smartInnerProduct;
  BFC                                                                             faceCalculator;

  OutputImageType *      output = this->GetOutput();
  const InputImageType * input = this->GetInput();

  // Break the input into a series of regions.  The first region is free
  // of boundary conditions, the rest with boundary conditions. Note,
  // we pass in the input image and the OUTPUT requested region. We are
  // only concerned with centering the neighborhood operator at the
  // pixels that correspond to output pixels.
  FaceListType faceList = faceCalculator(input, outputRegionForThread, m_Operator.GetRadius());

  ImageRegionIterator<OutputImageType> it;

  TotalProgressReporter progress(this, output->GetRequestedRegion().GetNumberOfPixels());

  // Process non-boundary region and each of the boundary faces.
  // These are N-d regions which border the edge of the buffer.
  ConstNeighborhoodIterator<InputImageType> bit;
  for (const auto & face : faceList)
  {
    bit = ConstNeighborhoodIterator<InputImageType>(m_Operator.GetRadius(), input, face);
    bit.OverrideBoundaryCondition(m_BoundsCondition);
    it = ImageRegionIterator<OutputImageType>(output, face);
    bit.GoToBegin();
    while (!bit.IsAtEnd())
    {
      it.Value() = static_cast<typename OutputImageType::PixelType>(smartInnerProduct(bit, m_Operator));
      ++bit;
      ++it;
      progress.CompletedPixel();
    }
  }
}
} // end namespace itk

#endif
