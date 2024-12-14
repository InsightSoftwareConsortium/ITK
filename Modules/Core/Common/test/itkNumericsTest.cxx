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
#include <iostream>

#include "vnl/algo/vnl_svd.h"

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
    std::cout << '\n';
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
    std::cout << "Warning: Singular matrix, condition = " << svd.well_condition() << '\n';
  }
  return svd.solve(B);
}


int
test_svd()
{
  double                   data[] = { 1, 1, 1, 1, 2, 3, 1, 3, 6 };
  vnl_matrix<double>       M(data, 3, 3);
  const vnl_matrix<double> B(3, 1, 7.0); // column vector [7 7 7]^T
  vnl_matrix<double>       result = solve_with_warning(M, B);
  std::cout << "Original svd problem solution" << '\n';
  print_vnl_matrix(result);
  M(2, 2) = 5;
  result = solve_with_warning(M, B);
  std::cout << '\n' << "Modified svd problem solution" << '\n';
  print_vnl_matrix(result);
  return 0;
}

int
itkNumericsTest(int, char *[])
{
  test_svd();
  double             data[] = { 1, 1, 1, 1, 2, 3, 1, 3, 6 };
  vnl_matrix<double> mat(data, 3, 3);
  std::cout << '\n' << "A matrix" << '\n';
  for (unsigned int r = 0; r < mat.rows(); ++r)
  {
    for (unsigned int c = 0; c < mat.rows(); ++c)
    {
      std::cout << mat(r, c) << ' ';
    }
    std::cout << '\n';
  }

  return 0;
}
