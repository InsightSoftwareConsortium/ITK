/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorMeanImageFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <stdio.h>

#include "itkVectorMeanImageFunction.h"
#include "itkImage.h"

int itkVectorMeanImageFunctionTest(int, char* [] )
{

  const unsigned int Dimension = 3;
  typedef unsigned char   PixelComponentType; 
  const unsigned int VectorDimension = 4;

  typedef itk::FixedArray< PixelComponentType, VectorDimension > PixelType;

  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::VectorMeanImageFunction< ImageType > FunctionType;

  // Create and allocate the image
  ImageType::Pointer      image = ImageType::New();
  ImageType::SizeType     size;
  ImageType::IndexType    start;
  ImageType::RegionType   region;
 
  size[0] = 20;
  size[1] = 20;
  size[2] = 20;

  start.Fill( 0 );
    
  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();

  ImageType::PixelType initialValue;

  initialValue[0] = 11;
  initialValue[1] = 13;
  initialValue[2] = 17;
  initialValue[3] = 19;

  image->FillBuffer( initialValue );

  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  function->SetNeighborhoodRadius( 5 );

  ImageType::IndexType    index;

  index[0] = 10;
  index[1] = 10;
  index[2] = 10;

  FunctionType::OutputType  mean;

  mean = function->EvaluateAtIndex( index );
  std::cout << "function->EvaluateAtIndex( index ): " << mean << std::endl;

  // Test Evaluate
  FunctionType::PointType point;
  point[0] = 25;
  point[1] = 25;
  point[2] = 25;
  FunctionType::OutputType mean2;
  mean2 = function->Evaluate(point);
  std::cout << "function->Evaluate(point): " << mean2 << std::endl;

  // Test EvaluateAtContinuousIndex
  FunctionType::ContinuousIndexType cindex;
  cindex[0] = 25;
  cindex[1] = 25;
  cindex[2] = 25;
  FunctionType::OutputType mean3;
  mean3 = function->EvaluateAtContinuousIndex(cindex);
  std::cout << "function->EvaluateAtContinuousIndex(cindex): " << mean3 << std::endl;

  // Test GetConstReferenceMacro
  const unsigned int & neighborhoodRadius = function->GetNeighborhoodRadius();
  std::cout << "function->GetNeighborhoodRadius(): " << neighborhoodRadius << std::endl;


  // since the input image is constant 
  // the should be equal to the initial value
  for( unsigned int ii=0; ii<VectorDimension; ii++ )
    {
    if( vnl_math_abs( initialValue[ii] - mean[ii] ) > 10e-7 )
      {
      std::cerr << "Error in mean computation" << std::endl;
      return EXIT_FAILURE;
      }
    }
  
  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}

