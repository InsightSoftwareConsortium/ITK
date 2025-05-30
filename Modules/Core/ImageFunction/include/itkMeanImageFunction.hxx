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
#ifndef itkMeanImageFunction_hxx
#define itkMeanImageFunction_hxx


#include "itkImage.h"
#include "itkShapedImageNeighborhoodRange.h"

namespace itk
{

template <typename TInputImage, typename TCoordinate>
MeanImageFunction<TInputImage, TCoordinate>::MeanImageFunction()

  = default;

template <typename TInputImage, typename TCoordinate>
auto
MeanImageFunction<TInputImage, TCoordinate>::EvaluateAtIndex(const IndexType & index) const -> RealType
{
  RealType sum{};

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
    sum += static_cast<RealType>(pixelValue);
  }

  return sum / static_cast<double>(neighborhoodRange.size());
}


template <typename TInputImage, typename TCoordinate>
void
MeanImageFunction<TInputImage, TCoordinate>::SetNeighborhoodRadius(const unsigned int radius)
{
  if (m_NeighborhoodRadius != radius)
  {
    m_NeighborhoodOffsets = GenerateRectangularImageNeighborhoodOffsets(ImageSizeType::Filled(radius));
    m_NeighborhoodRadius = radius;
    this->Modified();
  }
}


template <typename TInputImage, typename TCoordinate>
void
MeanImageFunction<TInputImage, TCoordinate>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NeighborhoodRadius: " << m_NeighborhoodRadius << std::endl;
}
} // end namespace itk

#endif
