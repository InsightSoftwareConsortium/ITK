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

template <class T, unsigned int TImageDimension>
int IterateOverImage( itk::ImageIterator<T, TImageDimension> it, unsigned int dim = 0)
{
  T value;
  int i, j;
  
  if (dim < TImageDimension - 1)
    {
    itk::Image<T, TImageDimension>::Index basisIndex;

    try
      {
      basisIndex = it.GetIndex().GetBasisIndex(dim);
      }
    catch (const int error)
      {
      if (error == itk::InvalidDimension)
        {
        itkGenericOutputMacro(<< "Exception: Invalid dimension");
        }
      }

    // set "it" to the beginning of the dim projection
    for (i=0; i < it.GetImageSize()[dim]; i++)
      {
      itkGenericOutputMacro(<< "Looping over " << dim);
      
      IterateOverImage(it, dim+1);
      // increment the iterator
      try
        {
        it += basisIndex;
        }
      catch (const int error)
        {
        if (error == itk::BoundsError)
          {
          itkGenericOutputMacro(<< "Exception: Exceeding image bounds.");
          }
        }
      }
    }
  else
    {
    itk::Image<T, TImageDimension>::Index basisIndex;

    try
      {
      basisIndex = it.GetIndex().GetBasisIndex(dim);
      }
    catch (const int error)
      {
      if (error == itk::InvalidDimension)
        {
        itkGenericOutputMacro(<< "Exception: Invalid dimension");
        }
      }

    // final dimension... do something
    for (j=0; j < it.GetImageSize()[dim]; j++)
      {
//     std::cout << "Looping over " << dim << ", "
//              << it.GetIndex() << std::endl;
  
      // set the pixel using iterator notation
      *it = value;

      // increment the iterator
      try
        {
        ++it; // fastest
        //it++; // fast
        //it += basisIndex; // slow
        //it.Increment( it.GetIndex().GetBasisIndex(dim) ); // slowest
        }
      catch (const int error)
        {
        if (error == itk::BoundsError)
          {
          itkGenericOutputMacro("Exception: Exceeding image bounds.");
          }
        }
      }
    }
  
  return 1;
}


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
  print_vnl_matrix(result);
  M(2,2)=5; result = solve_with_warning(M,B);
  print_vnl_matrix(result);
  return 0;
}

int main()
{
  test_svd();
  double data[] = { 1, 1, 1,  1, 2, 3,  1, 3, 6};
  vnl_matrix<double> mat(data, 3, 3);
  for(int r = 0; r < mat.rows(); r++)
    for(int c = 0; c < mat.rows(); c++)
      std::cout << mat(r, c) << " " << std::endl;
  
  // Test the creation of an image with native type
  itk::Image<float,2>::Pointer if2 = itk::Image<float,2>::New();
  
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

  // Next create some images and manipulate it.
  //
  // Create an image.
  itk::Image<itk::Scalar<float>, 2>::Pointer
    o2 = itk::Image<itk::Scalar<float>, 2>::New();

  unsigned long size[2];
  float origin[2], spacing[2];
  size[0] = 12;
  size[1] = 6;
  origin[0] = 5;
  origin[1] = 2.1;
  spacing[0] = 1.5;
  spacing[1] = 2.1;
  
  //  o2->SetDimension(2);
  o2->SetSize(size);
  o2->SetOrigin(origin);
  o2->SetSpacing(spacing);

  o2->Allocate();

  long index[2];
  index[0] = 5;
  index[1] = 4;
  itk::Image<itk::Scalar<float>, 2>::Index ind;
  ind.SetIndex( index );

  itk::Scalar<float> scalar;
  scalar.SetScalar(3.14159);

  o2->SetPixel(ind, scalar);

  scalar.SetScalar(1.25);

  scalar = o2->GetPixel(ind);

  itkGenericOutputMacro(<< "Scalar pixel value is: " << scalar.GetScalar());

  itk::Image<itk::Vector<unsigned short, 5>, 3>::Pointer
    o3 = itk::Image<itk::Vector<unsigned short, 5>, 3>::New();

  unsigned long size3D[3];
  float origin3D[3], spacing3D[3];
  size3D[0] = 2;
  size3D[1] = 3;
  size3D[2] = 4;
  origin3D[0] = 5;
  origin3D[1] = 2.1;
  origin3D[2] = 8.1;
  spacing3D[0] = 1.5;
  spacing3D[1] = 2.1;
  spacing3D[2] = 1;
  
  //  o3->SetDimension(3);
  o3->SetSize(size3D);
  o3->SetOrigin(origin3D);
  o3->SetSpacing(spacing3D);

  o3->Allocate();

  long index3D[3];
  index3D[0] = 1;
  index3D[1] = 2;
  index3D[2] = 3;
  itk::Image<itk::Vector<unsigned short, 5>, 3>::Index ind3D;
  ind3D.SetIndex( index3D );

  unsigned short vecValues[5] = { 3, 2, 1, 4, 5};
  itk::Vector<unsigned short, 5> vec;
  vec.SetVector(vecValues);

  // o3->SetPixel( itk::Index(1, 2, 3), vec);
  o3->SetPixel(ind3D, vec);

  vecValues[0] = vecValues[1] = vecValues[2] = vecValues[3] = vecValues[4] = 2;
  vec.SetVector(vecValues);

  vec = o3->GetPixel(ind3D);

  itkGenericOutputMacro(<< "Vector pixel value is: ["
                        << vec.GetVector()[0] << ", "
                        << vec.GetVector()[1] << ", "
                        << vec.GetVector()[2] << ", "
                        << vec.GetVector()[3] << ", "
                        << vec.GetVector()[4] << "]");
  
  IterateOverImage( o3->Begin() );

  // Pause for the user
  char keyStroke;
  std::cin >> keyStroke;
  return 1;
}



