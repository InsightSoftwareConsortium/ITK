/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkBasicArchitectureTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkImage.h"
#include "itkScalar.h"
#include "itkVector.h"
#include "itkRandomImageSource.h"
#include "itkShrinkImage.h"
#include "itkWriteVTKImage.h"
#include "itkReadVTKImage.h"
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
  
  // Test the creation of an image with native type
  itk::Image<float,2>::Pointer if2 = itk::Image<float,2>::New();

  std::cout << std::endl
            << "Image dimension is " << itk::Image<float,5>::ImageDimension
            << std::endl;
  std::cout << "Image dimension is " << itk::Image<short,1>::ImageDimension
            << std::endl;

  // Begin by creating a simple pipeline
  //
  // Create another source
  itk::ReadVTKImage< itk::Image<itk::Scalar<float>,2> >::Pointer reader;
  reader = itk::ReadVTKImage< itk::Image<itk::Scalar<float>,2> >::New();
  reader->SetFileName("junkInput.vtk");

  // Create a source
  itk::RandomImageSource< itk::Image<itk::Scalar<float>,2> >::Pointer random;
  random = itk::RandomImageSource< itk::Image<itk::Scalar<float>,2> >::New();

  // Create a filter
  itk::ShrinkImage< itk::Image<itk::Scalar<float>,2>, itk::Image<itk::Scalar<float>,2> >::Pointer shrink;
  shrink = itk::ShrinkImage< itk::Image<itk::Scalar<float>,2>, itk::Image<itk::Scalar<float>,2> >::New();
  shrink->SetInput(random->GetOutput());
  shrink->DebugOn();

  // Create a mapper
  itk::WriteVTKImage< itk::Image<itk::Scalar<float>,2> >::Pointer writer;
  writer = itk::WriteVTKImage< itk::Image<itk::Scalar<float>,2> >::New();
  writer->SetInput(shrink->GetOutput());
  writer->SetFileName("junkImage.vtk");
  writer->SetFileTypeToASCII();
  writer->DebugOn();
  writer->Write();

  return 1;
}



