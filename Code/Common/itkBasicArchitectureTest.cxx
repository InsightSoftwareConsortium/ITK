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

int main()
{
  itkImageBase::Pointer o1 = itkImageBase::New();

  itkImage<itkScalar<float>, 2>::Pointer
    o2 = itkImage<itkScalar<float>, 2>::New();

  int size[2];
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

  unsigned long index[2];
  index[0] = 5;
  index[1] = 4;
  itkImage<itkScalar<float>, 2>::Index ind;
  ind.SetIndex( index );

  itkScalar<float> scalar;
  scalar.SetScalar(3.14159);

  o2->SetPixel(ind, scalar);

  scalar.SetScalar(1.25);

  scalar = o2->GetPixel(ind);

  std::cerr << "Scalar pixel value is: " << scalar.GetScalar() << std::endl;


  itkImage<itkVector<unsigned short, 5>, 3>::Pointer
    o3 = itkImage<itkVector<unsigned short, 5>, 3>::New();


  int size3D[3];
  float origin3D[3], spacing3D[3];
  size3D[0] = 12;
  size3D[1] = 6;
  size3D[2] = 8;
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

  unsigned long index3D[3];
  index3D[0] = 5;
  index3D[1] = 4;
  index3D[2] = 6;
  itkImage<itkVector<unsigned short, 5>, 3>::Index ind3D;
  ind3D.SetIndex( index3D );

  unsigned short vecValues[5] = { 3, 2, 1, 4, 5};
  itkVector<unsigned short, 5> vec;
  vec.SetVector(vecValues);

  o3->SetPixel(ind3D, vec);

  vecValues[0] = vecValues[1] = vecValues[2] = vecValues[3] = vecValues[4] = 2;
  vec.SetVector(vecValues);

  vec = o3->GetPixel(ind3D);

  std::cerr << "Vector pixel value is: ["
	    << vec.GetVector()[0] << ", "
	    << vec.GetVector()[1] << ", "
	    << vec.GetVector()[2] << ", "
	    << vec.GetVector()[3] << ", "
	    << vec.GetVector()[4] << "]"
	    << std::endl;
  
  
  return 0;
}

