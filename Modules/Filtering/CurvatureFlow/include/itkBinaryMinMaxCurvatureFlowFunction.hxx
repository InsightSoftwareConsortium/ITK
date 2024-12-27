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
#ifndef itkBinaryMinMaxCurvatureFlowFunction_hxx
#define itkBinaryMinMaxCurvatureFlowFunction_hxx

#include "itkMath.h"
#include "itkNeighborhoodInnerProduct.h"

namespace itk
{

template <typename TImage>
BinaryMinMaxCurvatureFlowFunction<TImage>::BinaryMinMaxCurvatureFlowFunction()
{
  m_Threshold = 0.0;
}

template <typename TImage>
auto
BinaryMinMaxCurvatureFlowFunction<TImage>::ComputeUpdate(const NeighborhoodType & it,
                                                         void *                   globalData,
                                                         const FloatOffsetType &  offset) -> PixelType
{
  using CurvatureFlowFunctionType = CurvatureFlowFunction<TImage>;
  const PixelType update = this->CurvatureFlowFunctionType::ComputeUpdate(it, globalData, offset);

  if (update == 0.0)
  {
    return update;
  }

  const NeighborhoodInnerProduct<ImageType> innerProduct;
  const PixelType                           avgValue = innerProduct(it, this->m_StencilOperator);

  if (avgValue < m_Threshold)
  {
    return (std::min(update, PixelType{}));
  }

  return (std::max(update, PixelType{}));
}
} // end namespace itk

#endif
