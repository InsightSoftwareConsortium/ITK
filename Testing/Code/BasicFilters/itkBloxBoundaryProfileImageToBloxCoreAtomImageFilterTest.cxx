/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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
#include "itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter.h"

// Spatial function stuff
#include "itkSphereSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

// DOG gradient related stuff
#include "itkBinomialBlurImageFilter.h"
#include "itkDifferenceOfGaussiansGradientImageFilter.h"

#include "itkExceptionObject.h"
#include <time.h>

int itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest(int, char*[])
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

  std::cout << "New sourceImage created" << std::endl;

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

  std::cout << "New sourceImage allocated" << std::endl;

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
  unsigned int sphereRadius = 7;
  spatialFunc->SetRadius( sphereRadius );

  // Set center of spatial function to (10,10,10)
  FunctionPositionType center;
  center[0]=10;
  center[1]=10;
  center[2]=10;
  spatialFunc->SetCenter(center);

  std::cout << "Sphere spatial function created" << std::endl;

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

  std::cout << "Spatial function iterator created, sphere drawn" << std::endl;

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

  // Typedefs for finding boundary points with GradientImageToBloxBoundaryPointImageFilter
  typedef itk::GradientImageToBloxBoundaryPointImageFilter<DOGFilterType::TOutputImage> TBPFilter;
  typedef TBPFilter::TOutputImage BloxBPImageType;

  // Find boundary points using results of DOG blurring
  TBPFilter::Pointer bpFilter= TBPFilter::New();
  bpFilter->SetInput( DOGFilter->GetOutput() );

  // Get the output of the boundary point filter
  BloxBPImageType::Pointer bloxBoundaryPointImage = bpFilter->GetOutput();

  // Update
  bpFilter->Update();

  //------------------------Blox Profile Analysis---------------------------------

  // Typedefs for bloxboundaryprofile filter and image
  typedef itk::BloxBoundaryPointImageToBloxBoundaryProfileImageFilter< ImageType > TProfileFilter;
  typedef itk::BloxBoundaryProfileImage< dim > BloxProfileImageType;

  TProfileFilter::Pointer profileFilter = TProfileFilter::New();
  std::cout << "Profile filter created" << std::endl;

  // Set the inputs need to find profiles
  profileFilter->SetInput1( blurredImage );
  profileFilter->SetInput2( bloxBoundaryPointImage );
  std::cout << "Input images set" << std::endl;

  // Initialize and set required parameters
  double setUniqueAxis = 10; // major axis of sampling region (ellipsoid)
  double setSymmetricAxes = 5; // minor axes of sampling region (ellipsoid)

  unsigned int numberOfBins = static_cast<unsigned int>(setUniqueAxis); // lets make each bin 1 voxel wide

  unsigned int splatMethod = 0; // method to weight voxel intensities
  // 0 - Gaussian, 1 - Triangular
  unsigned int spaceDimension = 4; // number of cost function parameters

  profileFilter->Initialize(setUniqueAxis, setSymmetricAxes, numberOfBins, 
    splatMethod, spaceDimension);
  std::cout << "Profile filter initialized" << std::endl;

  // Get the output of the profile filter
  BloxProfileImageType::Pointer bloxBoundaryProfileImage = profileFilter->GetOutput();

  // Try and update profile filter if there are no exceptions
  try{profileFilter->Update();}
  catch( itk::ExceptionObject  & myException )
  {
    std::cerr << "Exception caught in Update() method" << std::endl;
    std::cerr << myException << std::endl;
    return EXIT_FAILURE;
  }

  //-------------Blox Brofile Core Atom Analysis-----------------------------
  
  typedef itk::BloxBoundaryProfileImageToBloxCoreAtomImageFilter<ImageType, 3> ProfileCAFilterType;
  typedef ProfileCAFilterType::Pointer ProfileCAFilterPointerType;
  typedef ProfileCAFilterType::TOutputImage ProfileCAImageType;
  typedef ProfileCAImageType::Pointer ProfileCAImagePointerType;

  /** Pointer to the Boundary Profile Core Atom Images. */
  ProfileCAImagePointerType profileCAImage;

  // Convert boundary profiles to core atoms
  ProfileCAFilterType::Pointer bloxProfileCoreAtomFilter = ProfileCAFilterType::New();
  bloxProfileCoreAtomFilter->SetInput1( blurredImage );
  bloxProfileCoreAtomFilter->SetInput2( bloxBoundaryProfileImage );

  bloxProfileCoreAtomFilter->SetDistanceMin( 12.0 );
  bloxProfileCoreAtomFilter->SetDistanceMax( 16.0 );
  bloxProfileCoreAtomFilter->SetEpsilon( 0.05 );
  bloxProfileCoreAtomFilter->SetPolarity( 0 );

  profileCAImage = bloxProfileCoreAtomFilter->GetOutput();
    
  // Try and update profile filter if there are no exceptions
  try{bloxProfileCoreAtomFilter->Update();}
  catch( itk::ExceptionObject  & myException )
  {
    std::cerr << "Exception caught in Update() method" << std::endl;
    std::cerr << myException << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
