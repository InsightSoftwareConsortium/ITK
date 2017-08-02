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

#ifndef itkGTestTypedefsAndConstructors_h
#define itkGTestTypedefsAndConstructors_h


#include "gtest/gtest.h"

#include "itkImageBase.h"


namespace itk
{

namespace GTest
{

namespace TypedefsAndConstructors
{

/** \namespace itk::GTest::TypedefsAndConstructors::Dimension2
 * \brief Contains commonly used typedefs for working with 2-D Images,
 * and utility functions to construct commonly used objects.
 */
namespace Dimension2
{

const unsigned int Dimension = 2;

typedef itk::ImageBase< Dimension> ImageBaseType;


typedef ImageBaseType::SizeType      SizeType;
typedef ImageBaseType::IndexType     IndexType;
typedef ImageBaseType::PointType     PointType;
typedef ImageBaseType::DirectionType DirectionType;
typedef ImageBaseType::SpacingType   VectorType;


inline static PointType MakePoint(PointType::ValueType p1,
                           PointType::ValueType p2)
{
  const PointType::ValueType a[] = {p1,p2};
  PointType point(a);
  return point;
}

inline static VectorType MakeVector(VectorType::ValueType v1,
                             VectorType::ValueType v2)
{
  const VectorType::ValueType a[] = {v1,v2};
  VectorType vector(a);
  return vector;
}

inline static IndexType MakeIndex(IndexType::IndexValueType i1,
                           IndexType::IndexValueType i2)
{
  IndexType idx = {{i1,i2}};
  return idx;
}

inline static SizeType MakeSize(SizeType::SizeValueType s1,
                         SizeType::SizeValueType s2)
{
  SizeType size = {{s1,s2}};
  return size;
}

}  // end namespace Dimension2


/** \namespace itk::GTest::TypedefsAndConstructors::Dimension3
 * \brief Contains commonly used typedefs for working with 3-D Images,
 * and utility functions to construct commonly used objects.
 */
namespace Dimension3
{

const unsigned int Dimension = 3;

typedef itk::ImageBase< Dimension> ImageBaseType;


typedef ImageBaseType::SizeType      SizeType;
typedef ImageBaseType::IndexType     IndexType;
typedef ImageBaseType::PointType     PointType;
typedef ImageBaseType::DirectionType DirectionType;
typedef ImageBaseType::SpacingType   VectorType;


inline static PointType MakePoint(PointType::ValueType p1,
                                  PointType::ValueType p2,
                                  PointType::ValueType p3)
{
  const PointType::ValueType a[] = {p1,p2,p3};
  PointType point(a);
  return point;
}

inline static VectorType MakeVector(VectorType::ValueType v1,
                             VectorType::ValueType v2,
                             VectorType::ValueType v3)
{
  const VectorType::ValueType a[] = {v1,v2,v3};
  VectorType vector(a);
  return vector;
}

inline static IndexType MakeIndex(IndexType::IndexValueType i1,
                           IndexType::IndexValueType i2,
                           IndexType::IndexValueType i3)
{
  IndexType idx = {{i1,i2,i3}};
  return idx;
}

inline static SizeType MakeSize(SizeType::SizeValueType s1,
                         SizeType::SizeValueType s2,
                         SizeType::SizeValueType s3)
{
  SizeType size = {{s1,s2,s3}};
  return size;
}

} // end namespace Dimension3

} // end namespace TypedefsAndConstructors
} // end namespace GTest
} // end namespace itk


#endif // itkGTestTypedefsAndConstructors_h
