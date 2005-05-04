/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricSecondRankTensorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkSymmetricSecondRankTensor.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"

int itkSymmetricSecondRankTensorTest(int, char* [] )
{
  int i, j;
  
  // Test it all
  
  float val[6] = {1.8, 0.2, 0.5, 3.4, 2.0, 1.2};
  
  typedef itk::SymmetricSecondRankTensor<float,3>         Float3DTensorType;
  typedef itk::SymmetricSecondRankTensor<unsigned char,3> Uchar3DTensorType;

  Float3DTensorType pixel(val);

  unsigned char pixelInit0[6] = {255, 255, 255,128,34,17};
  unsigned char pixelInit1[6] = {255, 255, 244,19,23,29};

  Uchar3DTensorType pixelArray[2];
  pixelArray[0] = pixelInit0;
  pixelArray[1] = pixelInit1;
  
  std::cout << "sizeof(pixel) = " << sizeof (pixel) << std::endl;
  if (sizeof(pixel) != 6 * sizeof(Float3DTensorType::ComponentType))
    {
    std::cerr << "ERROR: sizeof(pixel) == " << sizeof(pixel) << " but is should be " << 6 * sizeof(Float3DTensorType::ComponentType) << std::endl;
    return 1;
    }
  std::cout << "pixel.GetNumberOfComponents = " << pixel.GetNumberOfComponents() << std::endl;
  std::cout << "pixel.GetNthComponent()" << std::endl;
  for (i = 0; i < pixel.GetNumberOfComponents(); i++)
    {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << std::endl;
    }

  pixel(0,0) = 11.0; 
  pixel(0,1) = 21.0; 
  pixel(0,2) = 15.0; 
  pixel(1,0) = 11.0; 
  pixel(1,1) = 31.0; 
  pixel(1,2) = 10.0; 
  pixel(2,0) = 11.0; // these three last element will overwrite its symmetric counterparts
  pixel(2,1) = 41.0; 
  pixel(2,2) = 14.0; 

  std::cout << "testing the pixel(i,j) APID" << std::endl;
  for (i = 0; i < pixel.GetNumberOfComponents(); i++)
    {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << std::endl;
    }
  
  std::cout << "pixel[0] = 111; pixel[1] = 222; pixel[2] = 333;" << std::endl;
  std::cout << "pixel[3] = 444; pixel[4] = 555; pixel[5] = 666;" << std::endl;
  
  pixel[0] = 111; pixel[1] = 222; pixel[2] = 333;
  pixel[3] = 444; pixel[4] = 555; pixel[5] = 666;

  for (i = 0; i < pixel.GetNumberOfComponents(); i++)
    {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << std::endl;
    }
  
  std::cout << "std::cout << pixel << std::endl;" << std::endl;
  std::cout << "\t" << pixel << std::endl;
  
  for (j = 0; j < 2; j++)
    {
    std::cout << "pixelArray["<< j << "].GetNumberOfComponents = " << pixelArray[j].GetNumberOfComponents() << std::endl;
    std::cout << "pixelArray[" << j << "].GetNthComponent()" << std::endl;
    for (i = 0; i < pixelArray[j].GetNumberOfComponents(); i++)
      {
      std::cout << "\tpixelArray[" << j << "].GetNthComponent(" << i << ") = " << static_cast<int>(pixelArray[j].GetNthComponent(i)) << std::endl;
      }
    }

  std::cout << "Testing arithmetic methods" << std::endl;
  Float3DTensorType pa;
  Float3DTensorType pb;

  pa[0] = 1.25;
  pa[1] = 3.25;
  pa[2] = 5.25;
  pa[3] = 1.25;
  pa[4] = 3.25;
  pa[5] = 5.25;

  pb[0] = 1.55;
  pb[1] = 3.55;
  pb[2] = 5.55;
  pb[3] = 1.55;
  pb[4] = 3.55;
  pb[5] = 5.55;

  Float3DTensorType pc;
  
  pc = pa + pb;
  std::cout << "addition = " << pc << std::endl;

  pc = pa - pb;
  std::cout << "subtraction = " << pc << std::endl;

  pc += pb;
  std::cout << "in-place addition = " << pc << std::endl;

  pc -= pb;
  std::cout << "in-place subtraction = " << pc << std::endl;

  pc = pa * 3.2;
  std::cout << "product by scalar = " << pc << std::endl;




  /** Create an Image of tensors  */
  typedef Float3DTensorType   PixelType;
  typedef itk::Image< PixelType, 3 >           ImageType;
  
  ImageType::Pointer dti = ImageType::New();

  ImageType::SizeType  size;
  ImageType::IndexType start;
  ImageType::RegionType region;

  size[0] = 128;
  size[1] = 128;
  size[2] = 128;

  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  region.SetIndex( start );
  region.SetSize( size );

  dti->SetRegions( region );
  dti->Allocate();
  
  ImageType::SpacingType spacing;
  spacing[0] = 0.5;
  spacing[1] = 0.5;
  spacing[2] = 1.5;

  ImageType::PointType origin;
  origin[0] = 25.5;
  origin[1] = 25.5;
  origin[2] = 27.5;

  dti->SetOrigin( origin );
  dti->SetSpacing( spacing );

  PixelType tensor;

  tensor[0] = 1.2;
  tensor[1] = 2.2;
  tensor[2] = 3.2;
  tensor[3] = 4.2;
  tensor[4] = 5.2;
  tensor[5] = 6.2;

  dti->FillBuffer( tensor );
  
  typedef itk::ImageRegionIterator< ImageType > IteratorType;

  IteratorType it( dti, region );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Set( tensor );
    ++it;
    }

  // Test Eigen values computation
  {
    typedef itk::SymmetricSecondRankTensor<double,3>         Double3DTensorType;

    Double3DTensorType tensor;

    double v[3];
    v[0] = 19.0;
    v[1] = 23.0;
    v[2] = 29.0;

    tensor(0,0) = v[0]; 
    tensor(0,1) =  0.0; 
    tensor(0,2) =  0.0; 
    tensor(1,0) =  0.0; // overrides (0,1)
    tensor(1,1) = v[1]; 
    tensor(1,2) =  0.0; 
    tensor(2,0) =  0.0; // overrides (0,2)
    tensor(2,1) =  0.0; // overrides (1,2)
    tensor(2,2) = v[2]; 
   
    std::cout << "SymmetricTensor = " << std::endl;
    std::cout << tensor << std::endl;

    Double3DTensorType::EigenValuesArrayType     eigenValues;
    Double3DTensorType::EigenValuesArrayType     eigenValues2;
    Double3DTensorType::EigenVectorsMatrixType   eigenVectors;
    
    tensor.ComputeEigenAnalysis( eigenValues, eigenVectors );
    tensor.ComputeEigenValues( eigenValues2 );

    std::cout << "EigenValues = " << std::endl;
    std::cout << eigenValues << std::endl;

    std::cout << "EigenVectors = " << std::endl;
    std::cout << eigenVectors << std::endl;

    const double tolerance = 1e-4;

    {
      Double3DTensorType::EigenValuesArrayType     expectedValues;
      expectedValues[0] = v[0];
      expectedValues[1] = v[1];
      expectedValues[2] = v[2];

      for(unsigned int i=0; i<3; i++)
        {
        if( fabs( expectedValues[i] - eigenValues[i] ) > tolerance )
          {
          std::cerr << "EigenAnalysis computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues << std::endl;
          return EXIT_FAILURE;
          }
        }

      for(unsigned int j=0; j<3; j++)
        {
        if( fabs( expectedValues[j] - eigenValues2[j] ) > tolerance )
          {
          std::cerr << "EigenValues computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues2 << std::endl;
          return EXIT_FAILURE;
          }
        }

    }

    // Now let's do something more involved...
    tensor(0,0) =  7.0;
    tensor(0,1) =  0.0; 
    tensor(0,2) =  3.0;
    tensor(1,0) =  0.0; // overrides (0,1)
    tensor(1,1) =  0.0;
    tensor(1,2) =  0.0; 
    tensor(2,0) =  3.0; // overrides (0,2)
    tensor(2,1) =  0.0; // overrides (1,2)
    tensor(2,2) =  7.0;
 
    std::cout << "SymmetricTensor = " << std::endl;
    std::cout << tensor << std::endl;

    tensor.ComputeEigenAnalysis( eigenValues, eigenVectors );
    tensor.ComputeEigenValues( eigenValues2 );

    std::cout << "EigenValues = " << std::endl;
    std::cout << eigenValues << std::endl;

    std::cout << "EigenVectors = " << std::endl;
    std::cout << eigenVectors << std::endl;

    {
      Double3DTensorType::EigenValuesArrayType     expectedValues;
      expectedValues[0] =  0.0;
      expectedValues[1] =  4.0;
      expectedValues[2] = 10.0;

      for(unsigned int i=0; i<3; i++)
        {
        if( fabs( expectedValues[i] - eigenValues[i] ) > tolerance )
          {
          std::cerr << "EigenAnalysis computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues << std::endl;
          return EXIT_FAILURE;
          }
        }

      for(unsigned int j=0; j<3; j++)
        {
        if( fabs( expectedValues[j] - eigenValues2[j] ) > tolerance )
          {
          std::cerr << "EigenValues computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues2 << std::endl;
          return EXIT_FAILURE;
          }
        }


    }

    // Now let's do one where we know the rotation...
    tensor(0,0) =  9.0;
    tensor(0,1) =  0.0; 
    tensor(0,2) =  7.0;
    tensor(1,0) =  0.0; // overrides (0,1)
    tensor(1,1) =  0.0;
    tensor(1,2) =  0.0; 
    tensor(2,0) =  7.0; // overrides (0,2)
    tensor(2,1) =  0.0; // overrides (1,2)
    tensor(2,2) =  3.0;
 
    std::cout << "SymmetricTensor = " << std::endl;
    std::cout << tensor << std::endl;

    tensor.ComputeEigenAnalysis( eigenValues, eigenVectors );
    tensor.ComputeEigenValues( eigenValues2 );

    std::cout << "EigenValues = " << std::endl;
    std::cout << eigenValues << std::endl;

    std::cout << "EigenVectors = " << std::endl;
    std::cout << eigenVectors << std::endl;

    {
      Double3DTensorType::EigenValuesArrayType     expectedValues;
      expectedValues[0] = -1.61577;
      expectedValues[1] =  0.00000;
      expectedValues[2] = 13.61580;

      for(unsigned int i=0; i<3; i++)
        {
        if( fabs( expectedValues[i] - eigenValues[i] ) > tolerance )
          {
          std::cerr << "Eigenvalue computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues << std::endl;
          return EXIT_FAILURE;
          }
        }

      for(unsigned int j=0; j<3; j++)
        {
        if( fabs( expectedValues[j] - eigenValues2[j] ) > tolerance )
          {
          std::cerr << "EigenValues computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues2 << std::endl;
          return EXIT_FAILURE;
          }
        }

    }

  } // end of Test Eigen values computation


  // Test GetTrace() method
  {

    typedef itk::SymmetricSecondRankTensor<double,3>         Double3DTensorType;
    typedef Double3DTensorType::AccumulateValueType          AccumulateValueType;

    Double3DTensorType tensor;

    tensor(0,0) =  19.0;
    tensor(0,1) =   0.0; 
    tensor(0,2) =   0.0;
    tensor(1,0) =   0.0; // overrides (0,1)
    tensor(1,1) =  23.0;
    tensor(1,2) =   0.0; 
    tensor(2,0) =   7.0; // overrides (0,2)
    tensor(2,1) =   0.0; // overrides (1,2)
    tensor(2,2) =  29.0;

    AccumulateValueType expectedTrace = 
              itk::NumericTraits< AccumulateValueType >::Zero;

    expectedTrace += tensor(0,0);
    expectedTrace += tensor(1,1);
    expectedTrace += tensor(2,2);

    const double tolerance = 1e-4;

    AccumulateValueType computedTrace = tensor.GetTrace();
    if( fabs( computedTrace - expectedTrace ) > tolerance )
      {
      std::cerr << "Error computing the Trace" << std::endl;
      std::cerr << "Expected trace = " << expectedTrace << std::endl;
      std::cerr << "Computed trace = " << computedTrace << std::endl;
      return EXIT_FAILURE;
      }

  } // end of Test GetTrace() method

  return EXIT_SUCCESS;
}

