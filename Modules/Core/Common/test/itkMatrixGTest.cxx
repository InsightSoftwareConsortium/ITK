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

// First include the header file to be tested:
#include "itkMatrix.h"
#include <gtest/gtest.h>
#include <type_traits> // For is_trivially_copyable.


namespace
{
template <typename TMatrix>
void
Expect_Matrix_default_constructor_zero_initializes_all_elements()
{
  const TMatrix defaultConstructedMatrix;

  for (unsigned row{}; row < TMatrix::RowDimensions; ++row)
  {
    for (unsigned column{}; column < TMatrix::ColumnDimensions; ++column)
    {
      EXPECT_EQ(defaultConstructedMatrix(row, column), 0);
    }
  }
}

template <typename TMatrix>
void
Expect_GetIdentity_returns_identity_matrix()
{
  EXPECT_TRUE(TMatrix::GetIdentity().GetVnlMatrix().is_identity());
}

} // namespace


static_assert(std::is_trivially_copyable<itk::Matrix<float>>() && std::is_trivially_copyable<itk::Matrix<double>>() &&
                std::is_trivially_copyable<itk::Matrix<double, 2, 2>>(),
              "Matrix classes of built-in element types should be trivially copyable!");


TEST(Matrix, DefaultConstructorZeroInitializesAllElements)
{
  Expect_Matrix_default_constructor_zero_initializes_all_elements<itk::Matrix<float>>();
  Expect_Matrix_default_constructor_zero_initializes_all_elements<itk::Matrix<double>>();
  Expect_Matrix_default_constructor_zero_initializes_all_elements<itk::Matrix<double, 2, 2>>();
}


TEST(Matrix, GetIdentity)
{
  Expect_GetIdentity_returns_identity_matrix<itk::Matrix<float>>();
  Expect_GetIdentity_returns_identity_matrix<itk::Matrix<double>>();
  Expect_GetIdentity_returns_identity_matrix<itk::Matrix<double, 2, 2>>();
}
