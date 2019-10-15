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

#ifndef itkImageNeighborhoodOffsets_h
#define itkImageNeighborhoodOffsets_h

#include "itkRectangularImageNeighborhoodShape.h"
#include <vector>

namespace itk
{

/** Generates the offsets for a neighborhood of the specified shape. */
template <typename TImageNeighborhoodShape>
std::vector<Offset<TImageNeighborhoodShape::ImageDimension>>
GenerateImageNeighborhoodOffsets(const TImageNeighborhoodShape & shape)
{
  std::vector<Offset<TImageNeighborhoodShape::ImageDimension>> offsets(shape.GetNumberOfOffsets());
  shape.FillOffsets(offsets.data());
  return offsets;
}


/** Generates the offsets for a hyperrectangular (box shaped) neighborhood. */
template <unsigned VImageDimension>
std::vector<Offset<VImageDimension>>
GenerateRectangularImageNeighborhoodOffsets(const Size<VImageDimension> & radius)
{
  const RectangularImageNeighborhoodShape<VImageDimension> shape(radius);
  return GenerateImageNeighborhoodOffsets(shape);
}

} // namespace itk
#endif
