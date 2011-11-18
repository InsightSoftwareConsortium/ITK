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


// Native ITK stuff
#include "itkFilterWatcher.h"

// Spatial function stuff
#include "itkSphereSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

// DOG gradient related stuff
#include "itkBinomialBlurImageFilter.h"
#include "itkDifferenceOfGaussiansGradientImageFilter.h"
#include "itkVectorMagnitudeImageFilter.h"

/*
This file tests:
  itkDifferenceOfGaussiansGradientImageFilter
*/

int itkDifferenceOfGaussiansGradientTest(int, char* [] )
{
  const unsigned int dim = 3;

  // Image typedef
  typedef itk::Image< unsigned char, dim > TImageType;

  //-----------------Create a new input image--------------------
  // Image size and spacing parameters
  TImageType::SizeValueType     sourceImageSize[]  = { 20,20,20 };
  TImageType::SpacingValueType  sourceImageSpacing[] = { 1.0,1.0,1.0 };
  TImageType::PointValueType    sourceImageOrigin[] = { 0,0,0 };

  // Creates the sourceImage (but doesn't set the size or allocate memory)
  TImageType::Pointer sourceImage = TImageType::New();
  sourceImage->SetOrigin(sourceImageOrigin);
  sourceImage->SetSpacing(sourceImageSpacing);

  printf("New sourceImage created\n");

  //-----The following block allocates the sourceImage-----

  // Create a size object native to the sourceImage type
  TImageType::SizeType sourceImageSizeObject;
  // Set the size object to the array defined earlier
  sourceImageSizeObject.SetSize( sourceImageSize );
  // Create a region object native to the sourceImage type
  TImageType::RegionType largestPossibleRegion;
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
  itk::ImageRegionIterator<TImageType> it =
    itk::ImageRegionIterator<TImageType>(sourceImage, largestPossibleRegion);

  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    it.Set(0);
    }

  //---------Create and initialize a spatial function-----------

  typedef itk::SphereSpatialFunction<dim> TFunctionType;
  typedef TFunctionType::InputType        TFunctionPositionType;

  // Create and initialize a new sphere function

  TFunctionType::Pointer spatialFunc = TFunctionType::New();
  spatialFunc->SetRadius( 5 );

  TFunctionPositionType center;
  center[0]=10;
  center[1]=10;
  center[2]=10;
  spatialFunc->SetCenter(center);

  printf("Sphere spatial function created\n");

  //---------Create and initialize a spatial function iterator-----------
  TImageType::IndexType seedPos;
  const TImageType::IndexValueType pos[] = {10,10,10};
  seedPos.SetIndex(pos);

  typedef itk::FloodFilledSpatialFunctionConditionalIterator
    <TImageType, TFunctionType> TItType;
  TItType sfi = TItType(sourceImage, spatialFunc, seedPos);

  // for coverage, recover the seeds
  const TItType::SeedsContainerType &seeds(sfi.GetSeeds());
  //
  // show seed indices
  std::cout << "Seeds for FloodFilledSpatialFunctionConditionalIterator"
            << std::endl;
  for(TItType::SeedsContainerType::const_iterator s_it
        = seeds.begin();
      s_it != seeds.end(); ++s_it)
    {
    std::cout << (*s_it) << " ";
    }
  std::cout << std::endl;

  // Iterate through the entire image and set interior pixels to 255
  for(; !( sfi.IsAtEnd() ); ++sfi)
    {
    sfi.Set(255);
    }

  std::cout << "Spatial function iterator created, sphere drawn"
            << std::endl;

  //--------------------Do blurring----------------
  typedef TImageType TOutputType;

  // Create a binomial blur filter
  itk::BinomialBlurImageFilter<TImageType, TOutputType>::Pointer binfilter;
  binfilter = itk::BinomialBlurImageFilter<TImageType, TOutputType>::New();

  sourceImage->SetRequestedRegion(sourceImage->GetLargestPossibleRegion() );

  // Set filter parameters
  binfilter->SetInput(sourceImage);
  binfilter->SetRepetitions(4);

  // Set up the output of the filter
  TImageType::Pointer blurredImage = binfilter->GetOutput();

  // Execute the filter
  binfilter->Update();
  std::cout << "Binomial blur filter updated"
            << std::endl;

  //------------Finally we can test the DOG filter------------

  // Create a differennce of gaussians gradient filter
  typedef itk::DifferenceOfGaussiansGradientImageFilter<TOutputType,
    double> TDOGFilterType;
  TDOGFilterType::Pointer DOGFilter = TDOGFilterType::New();
  FilterWatcher watcher(DOGFilter);

  // We're filtering the output of the binomial filter
  DOGFilter->SetInput(blurredImage);

  // Test the get/set macro for width
  DOGFilter->SetWidth(4);
  unsigned int theWidth = DOGFilter->GetWidth();
  std::cout << "DOGFilter->GetWidth(): " << theWidth << std::endl;

  // Get the output of the gradient filter
  TDOGFilterType::TOutputImage::Pointer gradientImage = DOGFilter->GetOutput();

  // Go!
  DOGFilter->Update();

  //-------------Test vector magnitude-------------
  typedef itk::VectorMagnitudeImageFilter<TDOGFilterType::TOutputImage,
    itk::Image<unsigned char, dim> > VectorMagType;

  VectorMagType::Pointer vectorMagFilter = VectorMagType::New();

  vectorMagFilter->SetInput(gradientImage);
  vectorMagFilter->Update();

  return EXIT_SUCCESS;
}
