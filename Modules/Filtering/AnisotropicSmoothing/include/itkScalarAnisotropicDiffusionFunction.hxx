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
#ifndef itkScalarAnisotropicDiffusionFunction_hxx
#define itkScalarAnisotropicDiffusionFunction_hxx

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkDerivativeOperator.h"

namespace itk
{
template <typename TImage>
void
ScalarAnisotropicDiffusionFunction<TImage>::CalculateAverageGradientMagnitudeSquared(TImage * ip)
{
  using RNI_type = ConstNeighborhoodIterator<TImage>;
  using SNI_type = ConstNeighborhoodIterator<TImage>;
  using BFC_type = NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TImage>;
  using AccumulateType = typename NumericTraits<PixelType>::AccumulateType;

  RNI_type                                      iterator_list[ImageDimension];
  SNI_type                                      face_iterator_list[ImageDimension];
  DerivativeOperator<PixelType, ImageDimension> operator_list[ImageDimension];

  // Set up the derivative operators, one for each dimension
  typename RNI_type::RadiusType radius;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    operator_list[i].SetOrder(1);
    operator_list[i].SetDirection(i);
    operator_list[i].CreateDirectional();
    radius[i] = operator_list[i].GetRadius()[i];
  }

  // Get the various region "faces" that are on the data set boundary.
  BFC_type                        bfc;
  typename BFC_type::FaceListType faceList = bfc(ip, ip->GetRequestedRegion(), radius);

  // Now do the actual processing
  auto accumulator = AccumulateType{};
  auto counter = SizeValueType{};

  // First process the non-boundary region

  // Instead of maintaining a single N-d neighborhood of pointers,
  // we maintain a list of 1-d neighborhoods along each axial direction.
  // This is more efficient for higher dimensions.
  auto          fit = faceList.begin();
  SizeValueType Stride[ImageDimension];
  SizeValueType Center[ImageDimension];
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    iterator_list[i] = RNI_type(operator_list[i].GetRadius(), ip, *fit);
    iterator_list[i].GoToBegin();
    Center[i] = iterator_list[i].Size() / 2;
    Stride[i] = iterator_list[i].GetStride(i);
  }
  while (!iterator_list[0].IsAtEnd())
  {
    ++counter;
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      PixelRealType val =
        iterator_list[i].GetPixel(Center[i] + Stride[i]) - iterator_list[i].GetPixel(Center[i] - Stride[i]);
      const PixelRealType tempval = val / -2.0f;
      val = tempval * this->m_ScaleCoefficients[i];
      accumulator += val * val;
      ++iterator_list[i];
    }
  }

  // Go on to the next region(s).  These are on the boundary faces.
  ++fit;
  ZeroFluxNeumannBoundaryCondition<TImage> bc;
  while (fit != faceList.end())
  {
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      face_iterator_list[i] = SNI_type(operator_list[i].GetRadius(), ip, *fit);
      face_iterator_list[i].OverrideBoundaryCondition(&bc);
      face_iterator_list[i].GoToBegin();
      Center[i] = face_iterator_list[i].Size() / 2;
      Stride[i] = face_iterator_list[i].GetStride(i);
    }

    while (!face_iterator_list[0].IsAtEnd())
    {
      ++counter;
      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        PixelRealType val =
          face_iterator_list[i].GetPixel(Center[i] + Stride[i]) - face_iterator_list[i].GetPixel(Center[i] - Stride[i]);
        const PixelRealType tempval = val / -2.0f;
        val = tempval * this->m_ScaleCoefficients[i];
        accumulator += val * val;
        ++face_iterator_list[i];
      }
    }
    ++fit;
  }

  this->SetAverageGradientMagnitudeSquared(static_cast<double>(accumulator / counter));
}
} // end namespace itk

#endif
