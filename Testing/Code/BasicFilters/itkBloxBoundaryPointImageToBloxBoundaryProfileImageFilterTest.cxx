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
#include <time.h>

int itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest(int, char*[])
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

  // For calculating total execution time
  int startTime;
  int endTime;

  // Time the execution of profiles
  startTime = clock();

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

  profileFilter->SetUseOptimizerGradient(true);
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

  //-------------------Pull boundary profiles out of the image----------------------

  // The test for BloxBoundaryPointImageToBloxBoundaryProfileImageFilter 
  // requires that the mean of estimated boundary locations is within
  // 0.1 voxels of the sphere's boundary.

  // Create an iterator that will walk the blox image
  typedef itk::ImageRegionIterator<BloxProfileImageType> BloxIterator;

  //profile iterator
  BloxIterator bloxIt = BloxIterator(bloxBoundaryProfileImage,
      bloxBoundaryProfileImage->GetRequestedRegion() );

  // Used for obtaining the index of a pixel
  BloxProfileImageType::IndexType bloxindex;


  // Position are we at in the list
  double averageRadius = 0;
  unsigned int profileCount = 1;
  for (bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
  {
    // What position are we at in the list?

    // Get the index of the pixel
    bloxindex = bloxIt.GetIndex();
    std::cout << "bloxindex: " << bloxindex << std::endl;

    // The iterator for accessing linked list info from profile pixel
    // Walk through all of the elements at the pixel
    for(itk::BloxBoundaryProfilePixel<3>::const_iterator bpiterator = bloxIt.Value().begin(); bpiterator != bloxIt.Value().end(); ++bpiterator)
    {
      // Used for obtaining position data from a BloxPoint
      const itk::Point<double, 3> position = (*bpiterator)->GetOptimalBoundaryLocation();

      // Find location of boundary profile on sphere
      const double halfsize=static_cast<double>(size)/2.0;
      const double radius = sqrt( pow((position[0] - halfsize), 2.0) + pow((position[1] - halfsize), 2.0) + pow((position[2] - halfsize), 2.0) );

      // Keep running sum of estimated radius to compute average radius
      averageRadius += radius;
      profileCount++;
    } // end iterate
  }

  // Compute average radius estimated by boundary profiles
  averageRadius = averageRadius/profileCount;

  std::cout << "Sphere Radius = " << sphereRadius << "  Average Radius = " << averageRadius << std::endl;

  // Report time to execute itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter
  endTime = clock();
  std::cout << "Profile calculation time: " << (endTime - startTime)/CLOCKS_PER_SEC 
            << " seconds" << std::endl;

  // Test passes if estimated radius is within .1 voxel of sphere radius
  const double RadiusDifference = fabs(averageRadius - sphereRadius);
  const double tolerance = 1;
  if(RadiusDifference <= tolerance)
  {
    std::cout << "itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest Passed!!!" << std::endl;
    return EXIT_SUCCESS;
  }
  else
  {
    std::cerr << "itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest Failed! (TEST: (" 
      << RadiusDifference
      << " <= "
      << tolerance
      << ") Failed!"
      << std::endl;
    return EXIT_FAILURE;
  }
}
