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
 * \brief Contains commonly used type alias for working with 2-D Images,
 * and utility functions to construct commonly used objects.
 */
namespace Dimension2
{

constexpr unsigned int Dimension = 2;

using ImageBaseType = itk::ImageBase<Dimension>;


using SizeType = ImageBaseType::SizeType;
using IndexType = ImageBaseType::IndexType;
using PointType = ImageBaseType::PointType;
using DirectionType = ImageBaseType::DirectionType;
using VectorType = ImageBaseType::SpacingType;
using RegionType = ImageBaseType::RegionType;

} // end namespace Dimension2


/** \namespace itk::GTest::TypedefsAndConstructors::Dimension3
 * \brief Contains commonly used type alias for working with 3-D Images,
 * and utility functions to construct commonly used objects.
 */
namespace Dimension3
{

constexpr unsigned int Dimension = 3;

using ImageBaseType = itk::ImageBase<Dimension>;


using SizeType = ImageBaseType::SizeType;
using IndexType = ImageBaseType::IndexType;
using PointType = ImageBaseType::PointType;
using DirectionType = ImageBaseType::DirectionType;
using VectorType = ImageBaseType::SpacingType;
using RegionType = ImageBaseType::RegionType;

} // end namespace Dimension3

} // end namespace TypedefsAndConstructors
} // end namespace GTest
} // end namespace itk


#endif // itkGTestTypedefsAndConstructors_h
