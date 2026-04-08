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
#ifndef itkBresenhamLine_hxx
#define itkBresenhamLine_hxx

#include "itkPoint.h"
#include "itkMath.h"

#include <algorithm>
#include <iterator>

namespace itk
{
template <unsigned int VDimension>
auto
BresenhamLine<VDimension>::BuildLine(LType Direction, IdentifierType length) -> OffsetArray
{
  Direction.Normalize();

  // The dimension with the largest absolute component
  const unsigned int maxDistanceDimension = std::distance(
    Direction.Begin(), std::max_element(Direction.Begin(), Direction.End(), [](const auto a, const auto b) {
      return itk::Math::Absolute(a) < itk::Math::Absolute(b);
    }));

  // compute actual line length because the shorter distance
  // the larger deviation due to rounding to integers
  const IdentifierType mainDirectionLen = length - 1;
  const double         euclideanLineLen = mainDirectionLen / itk::Math::Absolute(Direction[maxDistanceDimension]);

  // we are going to start at 0
  constexpr IndexType StartIndex{ 0 };
  IndexType           LastIndex;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    // round to closest voxel center to minimize the deviation from Direction
    LastIndex[i] = Math::RoundHalfIntegerUp<IndexValueType>(euclideanLineLen * Direction[i]);
  }

  const IndexArray indices = this->BuildLine(StartIndex, LastIndex);
  OffsetArray      offsets;
  offsets.reserve(indices.size());
  for (const IndexType & index : indices)
  {
    offsets.push_back(index - StartIndex);
  }

  return offsets;
}

template <unsigned int VDimension>
auto
BresenhamLine<VDimension>::BuildLine(IndexType p0, IndexType p1) -> IndexArray
{
  // Integer-only N-dimensional Bresenham, guaranteeing exact start and end
  // points. This avoids floating-point direction conversion entirely.
  // Reference: classic Bresenham extended to N-D as used by scikit-image,
  // OpenCV, and VTK.

  // Compute displacements and find the dominant axis
  IndexType      absDeltas;
  IndexType      step; // +1 or -1 per dimension
  IndexValueType maxAbsDelta = 0;
  unsigned int   mainAxis = 0;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    const IndexValueType delta = p1[i] - p0[i];
    step[i] = (delta >= 0) ? 1 : -1;
    absDeltas[i] = static_cast<IndexValueType>(itk::Math::Absolute(delta));

    if (absDeltas[i] > maxAbsDelta)
    {
      maxAbsDelta = absDeltas[i];
      mainAxis = i;
    }
  }

  // Number of pixels = steps along dominant axis + 1
  const IdentifierType numPixels = static_cast<IdentifierType>(maxAbsDelta) + 1;

  IndexArray indices;
  indices.reserve(numPixels);

  // Error accumulators for each secondary axis (2 * absDelta[i] per step,
  // overflow when accumulated error >= maxAbsDelta)
  auto accumulateError = MakeFilled<IndexType>(0);
  auto currentIndex = p0;

  for (IdentifierType s = 0; s < numPixels; ++s)
  {
    indices.push_back(currentIndex);

    // Advance along each dimension
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      if (i == mainAxis)
      {
        currentIndex[i] += step[i];
      }
      else
      {
        accumulateError[i] += 2 * absDeltas[i];
        if (accumulateError[i] >= maxAbsDelta)
        {
          currentIndex[i] += step[i];
          accumulateError[i] -= 2 * maxAbsDelta;
        }
      }
    }
  }

  return indices;
}

} // namespace itk

#endif
