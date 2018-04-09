/*=========================================================================
*
*  Copyright Insight Software Consortium
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

#include <algorithm> // For transform.
#include <cassert>
#include <numeric>  // For accumulate.
#include <vector>

#include "itkOffset.h"
#include "itkSize.h"

namespace itk
{
namespace Experimental
{

/** Fills the specified buffer with offsets for a hyperrectangular (box shaped)
 * neighborhood. */
template <unsigned VImageDimension>
void FillHyperrectangularImageNeighborhoodOffsets(
  Offset<VImageDimension>* const offsets,
  const std::size_t numberOfOffsets,
  const Size<VImageDimension>& radius) ITK_NOEXCEPT
{
  if (numberOfOffsets > 0)
  {
    assert(offsets != nullptr);
    Offset<VImageDimension> offset;

    std::transform(radius.begin(), radius.end(), offset.begin(), [](const SizeValueType radiusValue)
    {
      return -static_cast<OffsetValueType>(radiusValue);
    });

    for (std::size_t i = 0; i < numberOfOffsets; ++i)
    {
      offsets[i] = offset;

      for (unsigned dimensionIndex = 0; dimensionIndex < VImageDimension; ++dimensionIndex)
      {
        OffsetValueType& offsetValue = offset[dimensionIndex];

        ++offsetValue;

        if (offsetValue <= static_cast<OffsetValueType>(radius[dimensionIndex]))
        {
          break;
        }
        offsetValue = -static_cast<OffsetValueType>(radius[dimensionIndex]);
      }
    }
  }
}


/** Generates the offsets for a hyperrectangular (box shaped) neighborhood. */
template <unsigned VImageDimension>
std::vector<Offset<VImageDimension> > GenerateHyperrectangularImageNeighborhoodOffsets(const Size<VImageDimension>& radius)
{
  using OffsetType = Offset<VImageDimension>;

  const std::size_t numberOfOffsets =
    std::accumulate(radius.begin(), radius.end(), std::size_t{ 1 },
    [](const std::size_t accumulatedProduct, const SizeValueType radiusValue)
  {
    return ((2 * radiusValue) + 1) * accumulatedProduct;
  });

  std::vector<OffsetType> offsets(numberOfOffsets);
  FillHyperrectangularImageNeighborhoodOffsets(offsets.data(), numberOfOffsets, radius);
  return offsets;
}


} // namespace Experimental
} // namespace itk

#endif
