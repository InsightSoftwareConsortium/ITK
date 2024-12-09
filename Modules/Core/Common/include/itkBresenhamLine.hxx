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

namespace itk
{
template <unsigned int VDimension>
auto
BresenhamLine<VDimension>::BuildLine(LType Direction, IdentifierType length) -> OffsetArray
{
  // copied from the line iterator
  /** Variables that drive the Bresenham-Algorithm */

  Direction.Normalize();
  // we are going to start at 0
  constexpr IndexType StartIndex = { { 0 } };
  IndexType           LastIndex;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    LastIndex[i] = (IndexValueType)(length * Direction[i]);
  }
  // Find the dominant direction
  IndexValueType maxDistance = 0;
  unsigned int   maxDistanceDimension = 0;
  // Increment for the error for each step. Two times the difference between
  // start and end
  IndexType incrementError;

  // Direction of increment. -1 or 1
  IndexType overflowIncrement;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    auto distance = static_cast<long>(itk::Math::abs(LastIndex[i]));
    if (distance > maxDistance)
    {
      maxDistance = distance;
      maxDistanceDimension = i;
    }
    incrementError[i] = 2 * distance;
    overflowIncrement[i] = (LastIndex[i] < 0 ? -1 : 1);
  }

  // The dimension with the largest difference between start and end
  const unsigned int mainDirection = maxDistanceDimension;
  // If enough is accumulated for a dimension, the index has to be
  // incremented. Will be the number of pixels in the line
  auto maximalError = MakeFilled<IndexType>(maxDistance);
  // After an overflow, the accumulated error is reduced again. Will be
  // two times the number of pixels in the line
  auto reduceErrorAfterIncrement = MakeFilled<IndexType>(2 * maxDistance);
  // Accumulated error for the other dimensions
  auto accumulateError = MakeFilled<IndexType>(0);

  OffsetArray result(length);
  auto        currentImageIndex = MakeFilled<IndexType>(0);
  result[0] = currentImageIndex - StartIndex;
  unsigned int steps = 1;
  while (steps < length)
  {
    // This part is from ++ in LineConstIterator
    // We need to modify accumulateError, currentImageIndex, isAtEnd
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      if (i == mainDirection)
      {
        currentImageIndex[i] += overflowIncrement[i];
      }
      else
      {
        accumulateError[i] += incrementError[i];
        if (accumulateError[i] >= maximalError[i])
        {
          currentImageIndex[i] += overflowIncrement[i];
          accumulateError[i] -= reduceErrorAfterIncrement[i];
        }
      }
    }

    result[steps] = currentImageIndex - StartIndex; // produce an offset

    ++steps;
  }
  return (result);
}

template <unsigned int VDimension>
auto
BresenhamLine<VDimension>::BuildLine(IndexType p0, IndexType p1) -> IndexArray
{
  itk::Point<float, VDimension> point0;
  itk::Point<float, VDimension> point1;
  IdentifierType                maxDistance = 0;
  for (unsigned int i = 0; i < VDimension; ++i)
  {
    point0[i] = p0[i];
    point1[i] = p1[i];
    const IdentifierType distance = itk::Math::abs(p0[i] - p1[i]) + 1;
    if (distance > maxDistance)
    {
      maxDistance = distance;
    }
  }

  OffsetArray offsets = this->BuildLine(point1 - point0, maxDistance + 1);

  IndexArray indices;
  indices.reserve(offsets.size()); // we might not have to use the last one
  for (unsigned int i = 0; i < offsets.size(); ++i)
  {
    indices.push_back(p0 + offsets[i]);
    if (indices.back() == p1)
    {
      break;
    }
  }
  return indices;
}

} // namespace itk

#endif
