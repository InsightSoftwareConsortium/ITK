/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkNumericsTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>

#include "vnl/vnl_matrix.h"
#include <vnl/algo/vnl_svd.h>



template <typename T>
void print_vnl_matrix(T& mat)
{
  std::cout << mat;
  for(int r = 0; r < mat.rows(); r++)
    {
    for(int c = 0; c < mat.rows(); c++)
      std::cout << mat(r, c) << " ";
    std::cout << std::endl;
    }
}


template <class D>  // D is often double or float
vnl_matrix<D> solve_with_warning(vnl_matrix<D>const& M,
				 vnl_matrix<D>const& B)
{
  // Take svd of vnl_matrix<D> M, setting singular values
  // smaller than 1e-8 to 0, and hold the result.
  vnl_svd<D> svd(M, 1e-8);
  // Check for rank-deficiency
  if (svd.singularities() > 1)
    std::cerr << "Warning: Singular matrix, condition = " << svd.well_condition() << std::endl;
  return svd.solve(B);
}


int test_svd() {
  double data[] = { 1, 1, 1,  1, 2, 3,  1, 3, 6};
  vnl_matrix<double> M (data, 3, 3);
  vnl_matrix<double> B (3, 1, 7.0); // column vector [7 7 7]^T
  vnl_matrix<double> result = solve_with_warning(M,B);
  std::cout << "Original svd problem solution" << std::endl;
  print_vnl_matrix(result);
  M(2,2)=5; result = solve_with_warning(M,B);
  std::cout << std::endl << "Modified svd problem solution" << std::endl;
  print_vnl_matrix(result);
  return 0;
}

int main()
{
  test_svd();
  double data[] = { 1, 1, 1,  1, 2, 3,  1, 3, 6};
  vnl_matrix<double> mat(data, 3, 3);
  std::cout << std::endl << "A matrix" << std::endl;
  for(int r = 0; r < mat.rows(); r++)
    {
    for(int c = 0; c < mat.rows(); c++)
      {
      std::cout << mat(r, c) << " ";
      }
    std::cout << std::endl;
    }

  return 1;
}



