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
#include "itkRandomImage.h"
#include "itkShrinkImage.h"
#include "itkVTKImageWriter.h"
#include "itkVTKImageReader.h"

template <class T, unsigned int TImageDimension>
int IterateOverImage( itkImageIterator<T, TImageDimension> it, unsigned int dim = 0)
{
  T value;
  int i, j;
  
  if (dim < TImageDimension - 1)
    {
    itkImage<T, TImageDimension>::Index basisIndex;

    try
      {
      basisIndex = it.GetIndex().GetBasisIndex(dim);
      }
    catch (const int error)
      {
      if (error == itkInvalidDimension)
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
        if (error == itkBoundsError)
          {
          itkGenericOutputMacro(<< "Exception: Exceeding image bounds.");
          }
        }
      }
    }
  else
    {
    itkImage<T, TImageDimension>::Index basisIndex;

    try
      {
      basisIndex = it.GetIndex().GetBasisIndex(dim);
      }
    catch (const int error)
      {
      if (error == itkInvalidDimension)
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
        if (error == itkBoundsError)
          {
          itkGenericOutputMacro("Exception: Exceeding image bounds.");
          }
        }
      }
    }
  
  return 1;
}

int main()
{
  // Begin by creating a simple pipeline
  //
  // Create another source
  itkVTKImageReader< itkImage<itkScalar<float>,2> >::Pointer reader;
  reader = itkVTKImageReader< itkImage<itkScalar<float>,2> >::New();
  reader->SetFileName("junkInput.vtk");

  // Create a source
  itkRandomImage< itkImage<itkScalar<float>,2> >::Pointer random;
  random = itkRandomImage< itkImage<itkScalar<float>,2> >::New();

  // Create a filter
  itkShrinkImage< itkImage<itkScalar<float>,2>, itkImage<itkScalar<float>,2> >::Pointer shrink;
  shrink = itkShrinkImage< itkImage<itkScalar<float>,2>, itkImage<itkScalar<float>,2> >::New();
  shrink->SetInput(random->GetOutput());
  shrink->DebugOn();

  // Create a mapper
  itkVTKImageWriter< itkImage<itkScalar<float>,2> >::Pointer writer;
  writer = itkVTKImageWriter< itkImage<itkScalar<float>,2> >::New();
  writer->SetInput(shrink->GetOutput());
  writer->SetFileName("junkImage.vtk");
  writer->SetFileTypeToASCII();
  writer->DebugOn();
  writer->Write();

  // Next create some images and manipulate it.
  //
  // Create an image.
  itkImage<itkScalar<float>, 2>::Pointer
    o2 = itkImage<itkScalar<float>, 2>::New();

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
  itkImage<itkScalar<float>, 2>::Index ind;
  ind.SetIndex( index );

  itkScalar<float> scalar;
  scalar.SetScalar(3.14159);

  o2->SetPixel(ind, scalar);

  scalar.SetScalar(1.25);

  scalar = o2->GetPixel(ind);

  itkGenericOutputMacro(<< "Scalar pixel value is: " << scalar.GetScalar());

  itkImage<itkVector<unsigned short, 5>, 3>::Pointer
    o3 = itkImage<itkVector<unsigned short, 5>, 3>::New();

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
  itkImage<itkVector<unsigned short, 5>, 3>::Index ind3D;
  ind3D.SetIndex( index3D );

  unsigned short vecValues[5] = { 3, 2, 1, 4, 5};
  itkVector<unsigned short, 5> vec;
  vec.SetVector(vecValues);

  // o3->SetPixel( itkIndex(1, 2, 3), vec);
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
  cin >> keyStroke;
  return 1;
}



