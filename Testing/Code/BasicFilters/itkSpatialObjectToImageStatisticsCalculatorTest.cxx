/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectToImageStatisticsCalculatorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkImage.h"
#include "itkSpatialObjectToImageStatisticsCalculator.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkEllipseSpatialObject.h"

int itkSpatialObjectToImageStatisticsCalculatorTest(int, char * [] )
{
  typedef unsigned char PixelType;
  typedef itk::Image<PixelType,2> ImageType;
  typedef itk::EllipseSpatialObject<2> EllipseType;
  typedef itk::FloodFilledSpatialFunctionConditionalIterator<ImageType,
                                                 EllipseType> IteratorType;
  
  // Image Definition
  ImageType::RegionType region;
  ImageType::SizeType size;
  size.Fill(50);

  // Circle definition
  EllipseType::Pointer ellipse = EllipseType::New();
  ellipse->SetRadius(10);

  EllipseType::VectorType offset;
  offset.Fill(25);
  ellipse->GetIndexToObjectTransform()->SetOffset(offset);
  ellipse->ComputeObjectToParentTransform();

  
  // Create a test image
  typedef itk::SpatialObjectToImageFilter<EllipseType,ImageType> ImageFilterType;
  ImageFilterType::Pointer filter = ImageFilterType::New();
  filter->SetInput(ellipse);
  filter->SetSize(size);
  filter->SetInsideValue(255);
  filter->Update();

  ImageType::Pointer image = filter->GetOutput();

  offset.Fill(25);
  ellipse->GetIndexToObjectTransform()->SetOffset(offset);
  ellipse->ComputeObjectToParentTransform();


  typedef itk::SpatialObjectToImageStatisticsCalculator<ImageType,EllipseType> CalculatorType;
  CalculatorType::Pointer calculator = CalculatorType::New();
  calculator->SetImage(image);
  calculator->SetSpatialObject(ellipse);
  calculator->Update();

  std::cout << " --- Ellipse and Image perfectly aligned --- " << std::endl;
  std::cout << "Sample mean = " << calculator->GetMean() << std::endl ;
  std::cout << "Sample covariance = " << calculator->GetCovarianceMatrix();

  if(calculator->GetMean() != 255 
    || calculator->GetCovarianceMatrix()[0][0] != 0
    )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  offset.Fill(20);
  ellipse->GetIndexToObjectTransform()->SetOffset(offset);
  ellipse->ComputeObjectToParentTransform();
  ellipse->Update();
  calculator->Update();

  std::cout << " --- Ellipse and Image mismatched left --- " << std::endl;
  std::cout << "Sample mean = " << calculator->GetMean() << std::endl ;
  std::cout << "Sample covariance = " << calculator->GetCovarianceMatrix();

  if( (fabs(calculator->GetMean()[0]-140.0)>1.0) 
    || (fabs(calculator->GetCovarianceMatrix()[0][0]-16141.0)>1.0)
    )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  offset.Fill(30);
  ellipse->GetIndexToObjectTransform()->SetOffset(offset);
  ellipse->ComputeObjectToParentTransform();
  ellipse->Update();
  calculator->Update();

  std::cout << " --- Ellipse and Image mismatched right --- " << std::endl;
  std::cout << "Sample mean = " << calculator->GetMean() << std::endl ;
  std::cout << "Sample covariance = " << calculator->GetCovarianceMatrix();

  if( (fabs(calculator->GetMean()[0]-140.0)>1.0) 
    || (fabs(calculator->GetCovarianceMatrix()[0][0]-16141.0)>1.0)
    )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  return EXIT_SUCCESS;
}
