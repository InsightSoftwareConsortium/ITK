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
#ifndef itkContinuousIndex_h
#define itkContinuousIndex_h

#include "itkPoint.h"
#include "itkIndex.h"

#include <type_traits> // For is_floating_point.

namespace itk
{
/** \class ContinuousIndex
 * \brief A templated class holding a point in n-Dimensional image space.
 *
 * ContinuousIndex is a templated class that holds a set of coordinates
 * (components).
 * The template parameter TCoordRep can be any floating point type (float, double).
 * The VIndexDimension defines the number of  components in the continuous
 * index array.
 *
 * \sa Point
 * \sa Index
 *
 * \ingroup ImageAccess
 * \ingroup ImageObjects
 *
 * \ingroup ITKCommon
 */
template <typename TCoordRep = double, unsigned int VIndexDimension = 2>
class ContinuousIndex : public Point<TCoordRep, VIndexDimension>
{
  static_assert(std::is_floating_point<TCoordRep>::value,
                "The coordinates of a continuous index must be represented by floating point numbers.");

public:
  /** Standard class type aliases. */
  using Self = ContinuousIndex;
  using Superclass = Point<TCoordRep, VIndexDimension>;

  /** ValueType can be used to declare a variable that is the same type
   * as a data element held in an Point.   */
  using ValueType = TCoordRep;
  using CoordRepType = TCoordRep;

  /** Dimension of the Space */
  static constexpr unsigned int IndexDimension = VIndexDimension;

  /** Corresponding discrete index type */
  using IndexType = Index<VIndexDimension>;

  /** The Array type from which this Vector is derived. */
  using BaseArray = typename Superclass::BaseArray;
  using Iterator = typename BaseArray::Iterator;
  using ConstIterator = typename BaseArray::ConstIterator;

  /** Constructors */
  ContinuousIndex() = default;
  ContinuousIndex(const ContinuousIndex &) = default;
  ContinuousIndex(ContinuousIndex &&) = default;
  ContinuousIndex &
  operator=(const ContinuousIndex &) = default;
  ContinuousIndex &
  operator=(ContinuousIndex &&) = default;
  ~ContinuousIndex() = default;

  /** Pass-through constructor to the Point base class. */
  ContinuousIndex(const ValueType r[IndexDimension])
    : Superclass(r)
  {}

  /** Construct from discrete index type */
  ContinuousIndex(const IndexType & index)
  {
    for (unsigned int i = 0; i < VIndexDimension; i++)
    {
      (*this)[i] = static_cast<TCoordRep>(index[i]);
    }
  }
};
} // namespace itk

#endif
