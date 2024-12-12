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
#ifndef itkSumOfSquaresImageFunction_hxx
#define itkSumOfSquaresImageFunction_hxx


#include "itkShapedImageNeighborhoodRange.h"

namespace itk
{

template <typename TInputImage, typename TCoordinate>
SumOfSquaresImageFunction<TInputImage, TCoordinate>::SumOfSquaresImageFunction()
{
  this->m_NeighborhoodRadius = 1;
  this->m_NeighborhoodSize = 1;
}

template <typename TInputImage, typename TCoordinate>
auto
SumOfSquaresImageFunction<TInputImage, TCoordinate>::EvaluateAtIndex(const IndexType & index) const -> RealType
{
  auto sumOfSquares = RealType{};

  const InputImageType * const image = this->GetInputImage();

  if (image == nullptr)
  {
    return (NumericTraits<RealType>::max());
  }

  if (!this->IsInsideBuffer(index))
  {
    return (NumericTraits<RealType>::max());
  }

  const ShapedImageNeighborhoodRange<const InputImageType> neighborhoodRange(*image, index, m_NeighborhoodOffsets);

  // Walk the neighborhood
  for (const InputPixelType pixelValue : neighborhoodRange)
  {
    const auto value = static_cast<RealType>(pixelValue);
    sumOfSquares += value * value;
  }

  return (sumOfSquares);
}

template <typename TInputImage, typename TCoordinate>
void
SumOfSquaresImageFunction<TInputImage, TCoordinate>::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "NeighborhoodRadius: " << this->m_NeighborhoodRadius << std::endl;
  os << indent << "NeighborhoodSize: " << this->m_NeighborhoodSize << std::endl;
}

} // end namespace itk

#endif
