/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMahalanobisDistanceThresholdImageFunctionTest.cxx
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

#include "itkMahalanobisDistanceThresholdImageFunction.h"
#include "itkImage.h"
#include "itkRGBPixel.h"

int itkMahalanobisDistanceThresholdImageFunctionTest(int, char* [] )
{

  const unsigned int          Dimension = 3;
  typedef unsigned char       PixelComponentType; 
  typedef itk::RGBPixel<PixelComponentType>  PixelType; 

  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::MahalanobisDistanceThresholdImageFunction< ImageType > FunctionType;

  // Create and allocate the image
  ImageType::Pointer      image = ImageType::New();
  ImageType::SizeType     size;
  ImageType::IndexType    start;
  ImageType::RegionType   region;
 
  size[0] = 50;
  size[1] = 50;
  size[2] = 50;

  start.Fill( 0 );
    
  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();

  ImageType::PixelType initialValue;

  initialValue[0] = 11;
  initialValue[1] = 22;
  initialValue[2] = 33;

  image->FillBuffer( initialValue );

  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  const double threshold = 5.0;
  function->SetThreshold( threshold ); 


  vnl_matrix<double> Covariance( Dimension, Dimension );
  vnl_vector<double> Mean( Dimension );

  Mean[0] = 10.0;
  Mean[1] = 20.0;
  Mean[2] = 30.0;

  Covariance.fill( 0.0 );
  Covariance[0][0] = 100.0;
  Covariance[1][1] = 200.0;
  Covariance[2][2] = 300.0;

  function->SetCovariance( Covariance );
  function->SetMean( Mean );
  
  ImageType::IndexType    index;

  index[0] = 25;
  index[1] = 25;
  index[2] = 25;

  FunctionType::OutputType  belongs;

  belongs = function->EvaluateAtIndex( index );
  std::cout << "function->EvaluateAtIndex( index ): " << belongs << std::endl;

  // Test Evaluate
  FunctionType::PointType point;
  point[0] = 25;
  point[1] = 25;
  point[2] = 25;
  FunctionType::OutputType belongs2;
  belongs2 = function->Evaluate(point);
  std::cout << "function->Evaluate(point): " << belongs2 << std::endl;

  // Test EvaluateAtContinuousIndex
  FunctionType::ContinuousIndexType cindex;
  cindex[0] = 25;
  cindex[1] = 25;
  cindex[2] = 25;
  FunctionType::OutputType belongs3;
  belongs3 = function->EvaluateAtContinuousIndex(cindex);
  std::cout << "function->EvaluateAtContinuousIndex(cindex): " << belongs3 << std::endl;

  // Test GetConstReferenceMacro
  const double & getThreshold = function->GetThreshold();
  std::cout << "function->GetThreshold(): " << getThreshold << std::endl;
  if( fabs( threshold - getThreshold ) > 1e-9 )
    {
    std::cerr << "Error: Set/Get Threshold do not match" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}

