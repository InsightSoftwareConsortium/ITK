/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialFunctionImageEvaluatorFilterTest.cxx
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

#include <stdio.h>

// Native ITK stuff
#include "itkSize.h"
#include "itkIndex.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkPoint.h"

// Spatial function stuff
#include "itkGaussianSpatialFunction.h"
#include "itkSpatialFunctionImageEvaluatorFilter.h"

int itkSpatialFunctionImageEvaluatorFilterTest(int, char* [] )
{
  const unsigned int dim = 3;

  //-----------------Create a new input image--------------------
  // Image size and spacing parameters
  unsigned long sourceImageSize[]  = { 20,20,20 };
  double sourceImageSpacing[] = { 1.0,1.0,1.0 };
  double sourceImageOrigin[] = { 0,0,0 };

  // Image typedef
  typedef itk::Image< unsigned char, dim > ImageType;

  // Create the sourceImage
  ImageType::Pointer sourceImage = ImageType::New();
  sourceImage->SetOrigin(sourceImageOrigin);
  sourceImage->SetSpacing(sourceImageSpacing);

  // Create a size object native to the sourceImage type
  ImageType::SizeType sourceImageSizeObject;
  // Set the size object to the array defined earlier
  sourceImageSizeObject.SetSize( sourceImageSize );
  // Create a region object native to the sourceImage type
  ImageType::RegionType largestPossibleRegion;
  // Resize the region
  largestPossibleRegion.SetSize( sourceImageSizeObject );
  // Set the largest legal region size (i.e. the size of the whole sourceImage) to what we just defined
  sourceImage->SetLargestPossibleRegion( largestPossibleRegion );
  // Set the buffered region
  sourceImage->SetBufferedRegion( largestPossibleRegion );
  // Set the requested region
  sourceImage->SetRequestedRegion( largestPossibleRegion );
  // Now allocate memory for the sourceImage
  sourceImage->Allocate();

  // Create and initialize a new Gaussian function
  typedef itk::GaussianSpatialFunction<char, dim> FunctionType;
  FunctionType::Pointer pFunc = FunctionType::New();

  // Run the image evaluator filter
  typedef itk::SpatialFunctionImageEvaluatorFilter<FunctionType, ImageType, ImageType> TFilter;
  TFilter::Pointer pFilter = TFilter::New();

  pFilter->SetInput(sourceImage);
  pFilter->SetFunction(pFunc);
  ImageType::Pointer outputImage = pFilter->GetOutput();

  pFilter->Update();

  return EXIT_SUCCESS;
}
