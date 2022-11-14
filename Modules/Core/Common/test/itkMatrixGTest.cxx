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

// First include the header file to be tested:
#include "itkMatrix.h"
#include <gtest/gtest.h>
#include <numeric>     // For iota.
#include <type_traits> // For is_convertible and is_trivially_copyable.


namespace
{
template <typename TMatrix>
void
Expect_Matrix_default_constructor_zero_initializes_all_elements()
{
#ifndef __clang__
  // Clang versions before 3.9.0 reject the `const` here (erroneously).
  // Mac10.10-AppleClang-dbg-x86_64-static produced an error message on an
  // attempt to build ITK 5 from the master branch (2021-03-31), saying:
  // > error: default initialization of an object of const type
  // > 'const itk::Matrix' without a user-provided default constructor
  const
#endif
    TMatrix defaultConstructedMatrix;

  for (unsigned int row{}; row < TMatrix::RowDimensions; ++row)
  {
    for (unsigned int column{}; column < TMatrix::ColumnDimensions; ++column)
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


template <typename TMatrix>
constexpr bool
vnl_matrix_is_convertible_to_itk_Matrix()
{
  return std::is_convertible<vnl_matrix<typename TMatrix::ValueType>, TMatrix>();
}

template <typename TMatrix>
constexpr bool
vnl_matrix_fixed_is_convertible_to_itk_Matrix()
{
  return std::is_convertible<
    vnl_matrix_fixed<typename TMatrix::ValueType, TMatrix::RowDimensions, TMatrix::ColumnDimensions>,
    TMatrix>();
}


template <typename TMatrix>
void
Expect_Matrix_is_constructible_from_raw_array_of_arrays()
{
  using ValueType = typename TMatrix::ValueType;

  constexpr auto    numberOfRows = TMatrix::RowDimensions;
  constexpr auto    numberOfColumns = TMatrix::ColumnDimensions;
  constexpr auto    numberOfElements = numberOfRows * numberOfColumns;
  ValueType         rawArray[numberOfRows][numberOfColumns];
  ValueType * const beginOfRawArray = rawArray[0];

  // Just ensure that each element of the raw array has a different value.
  std::iota(beginOfRawArray, beginOfRawArray + numberOfElements, ValueType{ 1 });

  // Construct an itk::Matrix from a raw C-style array-of-arrays.
  const TMatrix matrix(rawArray);

  for (unsigned int row = 0; row < numberOfRows; ++row)
  {
    for (unsigned int column = 0; column < numberOfColumns; ++column)
    {
      EXPECT_EQ(matrix(row, column), rawArray[row][column]);
    }
  }
}
} // namespace


static_assert((!vnl_matrix_is_convertible_to_itk_Matrix<itk::Matrix<float>>()) &&
                (!vnl_matrix_is_convertible_to_itk_Matrix<itk::Matrix<double, 4, 5>>()),
              "itk::Matrix should prevent implicit conversion from vnl_matrix");

static_assert(vnl_matrix_fixed_is_convertible_to_itk_Matrix<itk::Matrix<float>>() &&
                vnl_matrix_fixed_is_convertible_to_itk_Matrix<itk::Matrix<double, 4, 5>>(),
              "itk::Matrix should allow implicit conversion from vnl_matrix_fixed");


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


TEST(Matrix, IsConstructibleFromRawArrayOfArrays)
{
  Expect_Matrix_is_constructible_from_raw_array_of_arrays<itk::Matrix<float>>();
  Expect_Matrix_is_constructible_from_raw_array_of_arrays<itk::Matrix<double, 2, 3>>();
}
