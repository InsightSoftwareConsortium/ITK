/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxCoreAtomTest.cxx
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

// Native ITK stuff
#include "itkSize.h"
#include "itkIndex.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkPoint.h"

// Blox stuff
#include "itkBloxBoundaryPointImage.h"
#include "itkBloxCoreAtomImage.h"
#include "itkGradientImageToBloxBoundaryPointImageFilter.h"
#include "itkBloxBoundaryPointToCoreAtomImageFilter.h"

// Spatial function stuff
#include "itkSphereSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

// DOG gradient related stuff
#include "itkBinomialBlurImageFilter.h"
#include "itkDifferenceOfGaussiansGradientImageFilter.h"

// Main for testing BloxImage/BloxPixel storage
int itkBloxCoreAtomTest(int, char* [] )
{
  const unsigned int dim = 3;

  //-----------------Create a new input image--------------------
  // Image size and spacing parameters
  unsigned long sourceImageSize[]  = { 20,20,20 };
  double sourceImageSpacing[] = { 1.0,1.0,1.0 };
  double sourceImageOrigin[] = { 0,0,0 };

  // Image typedef
  typedef itk::Image< unsigned char, dim > ImageType;

  // Creates the sourceImage (but doesn't set the size or allocate memory)
  ImageType::Pointer sourceImage = ImageType::New();
  sourceImage->SetOrigin(sourceImageOrigin);
  sourceImage->SetSpacing(sourceImageSpacing);

  printf("New sourceImage created\n");

  //-----The following block allocates the sourceImage-----

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

  printf("New sourceImage allocated\n");

  // Initialize the image to hold all 0's
  itk::ImageRegionIterator<ImageType> it = 
    itk::ImageRegionIterator<ImageType>(sourceImage, largestPossibleRegion);

  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    it.Set(0);
    }

  //---------Create and initialize a spatial function-----------

  typedef itk::SphereSpatialFunction<dim> FunctionType;
  typedef FunctionType::InputType FunctionPositionType;

  // Create and initialize a new sphere function

  FunctionType::Pointer spatialFunc = FunctionType::New();
  spatialFunc->SetRadius( 5 );

  FunctionPositionType center;
  center[0]=10;
  center[1]=10;
  center[2]=10;
  spatialFunc->SetCenter(center);

  printf("Sphere spatial function created\n");

  //---------Create and initialize a spatial function iterator-----------
  ImageType::IndexType seedPos;
  const ImageType::IndexValueType pos[] = {10,10,10};
  seedPos.SetIndex(pos);

  typedef itk::FloodFilledSpatialFunctionConditionalIterator
    <ImageType, FunctionType> ItType;
  ItType sfi = ItType(sourceImage, spatialFunc, seedPos);

  // Iterate through the entire image and set interior pixels to 255
  for( ; !( sfi.IsAtEnd() ); ++sfi)
    {
    sfi.Set(255);
    }

  printf("Spatial function iterator created, sphere drawn\n");

  //--------------------Do blurring and edge detection----------------
  typedef ImageType OutputType;
  
  // Create a binomial blur filter
  itk::BinomialBlurImageFilter<ImageType, OutputType>::Pointer binfilter;
  binfilter = itk::BinomialBlurImageFilter<ImageType, OutputType>::New();

  sourceImage->SetRequestedRegion(sourceImage->GetLargestPossibleRegion() );

  // Set filter parameters
  binfilter->SetInput(sourceImage);
  binfilter->SetRepetitions(4);

  // Set up the output of the filter
  ImageType::Pointer blurredImage = binfilter->GetOutput();

  // Create a differennce of gaussians gradient filter
  typedef itk::DifferenceOfGaussiansGradientImageFilter<OutputType,
    double> DOGFilterType;
  DOGFilterType::Pointer DOGFilter = DOGFilterType::New();

  // We're filtering the output of the binomial filter
  DOGFilter->SetInput(blurredImage);

  // Get the output of the gradient filter
  DOGFilterType::TOutputImage::Pointer gradientImage = DOGFilter->GetOutput();

  //------------------------Blox Boundary Point Analysis-------------------------

  typedef itk::GradientImageToBloxBoundaryPointImageFilter<DOGFilterType::TOutputImage> TBPFilter;
  typedef TBPFilter::TOutputImage BloxBPImageType;

  TBPFilter::Pointer bpFilter= TBPFilter::New();
  bpFilter->SetInput( DOGFilter->GetOutput() );

  BloxBPImageType::Pointer bloxBoundaryPointImage = bpFilter->GetOutput();

  bpFilter->Update();

  //----------------------Find core atoms-------------------------

  typedef itk::BloxCoreAtomImage<dim> CoreAtomType;
  CoreAtomType::Pointer coreAtomImage = CoreAtomType::New();

  typedef itk::BloxBoundaryPointToCoreAtomImageFilter<dim> TCAFilter;
  typedef TCAFilter::TOutputImage BloxCAImageType;

  TCAFilter::Pointer caFilter = TCAFilter::New();
  caFilter->SetInput(bloxBoundaryPointImage);
  caFilter->SetDistanceMin(8.0);
  caFilter->SetDistanceMax(12.0);
  caFilter->SetEpsilon(0.05);
  caFilter->SetPolarity(0);

  BloxCAImageType::Pointer bloxCoreAtomImage = caFilter->GetOutput();

  caFilter->Update();

  //--------------------Analyze core atom population---------------------

  std::cout << "Performing Eigenanalysis\n";
  
  bloxCoreAtomImage->DoEigenanalysis();

  return EXIT_SUCCESS;
}
