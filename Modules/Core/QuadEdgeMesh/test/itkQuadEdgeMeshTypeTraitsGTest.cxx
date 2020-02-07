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
#include "itkQuadEdgeMeshPoint.h"

#include <gtest/gtest.h>
#include <type_traits>

// info: is_pod (c++20 deprecated) is equivalent to is_trivial && is_standard_layout

// QuadEdgeMeshPoint is neither trivial or has a standard_layout
TEST(QuadEdgeMeshTypeTraits, QuadEdgeMeshPointIsNotPOD)
{
  using T = itk::QuadEdgeMeshPoint<float, 3>;
  EXPECT_EQ(std::is_trivial<T>::value, false);
  EXPECT_EQ(std::is_standard_layout<T>::value, false);
}
