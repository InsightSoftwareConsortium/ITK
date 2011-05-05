/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <stdio.h>

// Native ITK stuff
#include "itkIndex.h"
#include "itkImage.h"

// Blox stuff
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

  // Image typedef
  typedef itk::Image< unsigned char, dim > ImageType;

  //-----------------Create a new input image--------------------
  // Image size and spacing parameters
  ImageType::SizeValueType     sourceImageSize[]  = { 20,20,20 };
  ImageType::SpacingValueType  sourceImageSpacing[] = { 1.0,1.0,1.0 };
  ImageType::PointValueType    sourceImageOrigin[] = { 0,0,0 };

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
  typedef FunctionType::InputType         FunctionPositionType;

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

  //
  // get the seeds and display them.
  const ItType::SeedsContainerType &seeds(sfi.GetSeeds());
  std::cout << "Iterator seeds";
  for(ItType::SeedsContainerType::const_iterator s_it =
        seeds.begin(); s_it != seeds.end(); ++s_it)
    {
    std::cout << " " << (*s_it);
    }
  std::cout << std::endl;

  // Iterate through the entire image and set interior pixels to 255
  for(; !( sfi.IsAtEnd() ); ++sfi)
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
  typedef TCAFilter::TOutputImage                          BloxCAImageType;

  TCAFilter::Pointer caFilter = TCAFilter::New();
  caFilter->SetInput(bloxBoundaryPointImage);
  caFilter->SetDistanceMin(8.0);
  caFilter->SetDistanceMax(12.0);
  caFilter->SetEpsilon(0.05);
  caFilter->SetPolarity(0);

  BloxCAImageType::Pointer bloxCoreAtomImage = caFilter->GetOutput();

  caFilter->Update();

  // Test the macros in the image
  bloxCoreAtomImage->GetMedialNodeCount();
  bloxCoreAtomImage->GetNodePointerList();

  //--------------------Analyze core atom population---------------------

  std::cout << "Performing Eigenanalysis\n";

  bloxCoreAtomImage->DoEigenanalysis();

  //--------------------------Run voting test-----------------------------

  std::cout << "Doing core atom voting\n";

  bloxCoreAtomImage->DoCoreAtomVoting();

  return EXIT_SUCCESS;
}
