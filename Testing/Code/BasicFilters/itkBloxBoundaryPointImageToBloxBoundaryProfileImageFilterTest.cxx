/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest.cxx
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
#include "itkBloxBoundaryProfileImage.h"
#include "itkBloxBoundaryPointPixel.h"
#include "itkBloxBoundaryPointImage.h"
#include "itkGradientImageToBloxBoundaryPointImageFilter.h"
#include "itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter.h"

// Spatial function stuff
#include "itkSphereSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

// DOG gradient related stuff
#include "itkBinomialBlurImageFilter.h"
#include "itkDifferenceOfGaussiansGradientImageFilter.h"

#include "itkExceptionObject.h"


int itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest(int, char**)
{
  const unsigned int dim = 3;
  const unsigned int size = 20;

  //-----------------Create a new input image--------------------

  // Image size and spacing parameters
  unsigned long sourceImageSize[]  = { size,size,size };
  double sourceImageSpacing[] = { 1.0,1.0,1.0 };
  double sourceImageOrigin[] = { 0,0,0 };

  // Image typedef
  typedef itk::Image< unsigned char, dim > ImageType;

  // Creates the sourceImage (but doesn't set the size or allocate memory)
  ImageType::Pointer sourceImage = ImageType::New();
  sourceImage->SetOrigin(sourceImageOrigin);
  sourceImage->SetSpacing(sourceImageSpacing);

  printf("New sourceImage created\n");

  // The following block allocates the sourceImage

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

  // Initialize the image to voxel values of 32
  itk::ImageRegionIterator<ImageType> it = 
    itk::ImageRegionIterator<ImageType>(sourceImage, largestPossibleRegion);

  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    it.Set(32);
    }

  //---------Put a sphere in the input image-----------

  typedef itk::SphereSpatialFunction<dim> FunctionType;
  typedef FunctionType::InputType FunctionPositionType;

  // Create and initialize a new sphere function
  FunctionType::Pointer spatialFunc = FunctionType::New();
  spatialFunc->SetRadius( 7 );

  // Set center of spatial function to (10,10,10)
  FunctionPositionType center;
  center[0]=10;
  center[1]=10;
  center[2]=10;
  spatialFunc->SetCenter(center);

  printf("Sphere spatial function created\n");

  // Create and initialize a spatial function iterator
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
  binfilter->SetRepetitions(1);

  // Set up the output of the filter
  ImageType::Pointer blurredImage = binfilter->GetOutput();

  // Execute the filter
  binfilter->Update();
  std::cout << "Binomial blur filter updated\n";

  // Create a differennce of gaussians gradient filter
  typedef itk::DifferenceOfGaussiansGradientImageFilter<OutputType,
    double> DOGFilterType;
  DOGFilterType::Pointer DOGFilter = DOGFilterType::New();

  // We're filtering the output of the binomial filter
  DOGFilter->SetInput(blurredImage);

  // Get the output of the gradient filter
  DOGFilterType::TOutputImage::Pointer gradientImage = DOGFilter->GetOutput();

  // Go!
  DOGFilter->Update();

  //------------------------Blox Boundary Point Analysis-------------------------

  typedef itk::GradientImageToBloxBoundaryPointImageFilter<DOGFilterType::TOutputImage> TBPFilter;
  typedef TBPFilter::TOutputImage BloxBPImageType;

  TBPFilter::Pointer bpFilter= TBPFilter::New();
  bpFilter->SetInput( DOGFilter->GetOutput() );

  BloxBPImageType::Pointer bloxBoundaryPointImage = bpFilter->GetOutput();

  bpFilter->Update();

  //------------------------Blox Profile Analysis---------------------------------

  typedef itk::BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< ImageType > TProfileFilter;
  typedef itk::BloxBoundaryProfileImage< dim > BloxProfileImageType;

  TProfileFilter::Pointer profileFilter = TProfileFilter::New();
  std::cerr << "profile filter created" << std::endl;

  profileFilter->SetInput1( blurredImage );
  profileFilter->SetInput2( bloxBoundaryPointImage );
  std::cerr << "input images set" << std::endl;

  double setUniqueAxis = 5;
  double setSymmetricAxes = 2;
  unsigned int numberOfBins = 5;
  unsigned int splatMethod = 0;
  unsigned int spaceDimension = 4;

  profileFilter->Initialize(setUniqueAxis, setSymmetricAxes, numberOfBins, 
    splatMethod, spaceDimension);
  std::cerr << "profile filter initialized" << std::endl;

  BloxProfileImageType::Pointer bloxBoundaryProfileImage = profileFilter->GetOutput();

  try{profileFilter->Update();}
  catch( itk::ExceptionObject  & myException )
    {
    std::cerr << "Exception caught in Update() method" << std::endl;
    std::cerr << myException << std::endl;
    return EXIT_FAILURE;
    }

  //verification

  // double rad = spatialFunc->GetRadius();

  //-------------------Pull boundary profiles out of the image----------------------

  // Create an iterator that will walk the blox image
  typedef itk::ImageRegionIterator<BloxProfileImageType> BloxIterator;

  //profile iterator
  BloxIterator bloxIt = BloxIterator(bloxBoundaryProfileImage,
                                     bloxBoundaryProfileImage->GetRequestedRegion() );

  // Used for obtaining the index of a pixel
  BloxProfileImageType::IndexType bloxindex;

  // Used for obtaining position data from a BloxPoint
  itk::Point<double, 3> position;

  // Position are we at in the list
  int depth = 0;

  for ( bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    // The iterator for accessing linked list info from profile pixel
    itk::BloxBoundaryProfilePixel<3>::iterator bpiterator;

    // What position are we at in the list?
    depth = 0;

    // Get the index of the pixel
    bloxindex = bloxIt.GetIndex();

    // Walk through all of the elements at the pixel
    for (bpiterator = bloxIt.Value().begin(); bpiterator != bloxIt.Value().end(); ++bpiterator)
      {
      position = (*bpiterator)->GetOptimalBoundaryLocation();
      depth++;
      
      std::cout << "Boundary profile at ";
      std::cout << "Position=(" << position[0] << " " << position[1] << " " << position[2] << ") ";
      std::cout << "at index=(" << bloxindex.m_Index[0] << " " << bloxindex.m_Index[1] << " " << bloxindex.m_Index[2] << "),";
      std::cout << "depth=" << depth << "\n";
      } // end iterate
    }
  return EXIT_SUCCESS;
}
