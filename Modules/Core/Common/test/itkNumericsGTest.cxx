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
#include "itkGTest.h"
#include <iostream>

#include "vnl/algo/vnl_svd.h"

namespace
{
template <typename T>
void
print_vnl_matrix(T & mat)
{
  std::cout << mat;
  for (unsigned int r = 0; r < mat.rows(); ++r)
  {
    for (unsigned int c = 0; c < mat.columns(); ++c)
    {
      std::cout << mat(r, c) << ' ';
    }
    std::cout << std::endl;
  }
}

template <typename V> // V is often double or float
vnl_matrix<V>
solve_with_warning(const vnl_matrix<V> & M, const vnl_matrix<V> & B)
{
  // Take svd of vnl_matrix<V> M, setting singular values
  // smaller than 1e-8 to 0, and hold the result.
  const vnl_svd<V> svd(M, 1e-8);
  // Check for rank-deficiency
  if (svd.singularities() > 1)
  {
    std::cout << "Warning: Singular matrix, condition = " << svd.well_condition() << std::endl;
  }
  return svd.solve(B);
}


} // namespace

TEST(Numerics, ConvertedLegacyTest)
{
  double                   data[] = { 1, 1, 1, 1, 2, 3, 1, 3, 6 };
  vnl_matrix<double>       M(data, 3, 3);
  const vnl_matrix<double> B(3, 1, 7.0); // column vector [7 7 7]^T

  // The original problem is non-singular; the solution is [7, 0, 0]^T.
  vnl_matrix<double> result = solve_with_warning(M, B);
  print_vnl_matrix(result);
  EXPECT_NEAR(result(0, 0), 7.0, 1e-6);
  EXPECT_NEAR(result(1, 0), 0.0, 1e-6);
  EXPECT_NEAR(result(2, 0), 0.0, 1e-6);

  // Making M singular forces the SVD to return the minimum-norm
  // solution [35/6, 7/3, -7/6]^T.
  M(2, 2) = 5;
  result = solve_with_warning(M, B);
  print_vnl_matrix(result);
  EXPECT_NEAR(result(0, 0), 35.0 / 6.0, 1e-6);
  EXPECT_NEAR(result(1, 0), 7.0 / 3.0, 1e-6);
  EXPECT_NEAR(result(2, 0), -7.0 / 6.0, 1e-6);
}
