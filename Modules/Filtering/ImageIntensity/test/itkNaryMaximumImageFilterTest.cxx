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

#include "itkNaryMaximumImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

// Create a namespace in order to avoid conflicts with other tests.

namespace NaryMaximumImageFilterTest
{

// Define the dimension of the images
const unsigned int Dimension = 3;

// Declare the pixel types of the images
typedef float                PixelType;

// Declare the types of the images
typedef itk::Image< PixelType, Dimension >  InputImageType;
typedef itk::Image< PixelType, Dimension >  OutputImageType;

// Declare the type of the index to access images
typedef itk::Index< Dimension >         IndexType;

// Declare the type of the size
typedef itk::Size< Dimension >          SizeType;

// Declare the type of the Region
typedef itk::ImageRegion< Dimension >   RegionType;

// Declare the type of the Iterators
typedef itk::ImageRegionIteratorWithIndex< InputImageType >  InImageIteratorType;
typedef itk::ImageRegionIteratorWithIndex< OutputImageType > OutImageIteratorType;

// Declare the type for the itk::NaryMaximumImageFilter filter
typedef itk::NaryMaximumImageFilter<
                              InputImageType,
                              OutputImageType > FilterType;

// Function for image initialization
void InitializeImage( InputImageType * image, double value )
{
  InputImageType::Pointer inputImage( image );

  // Define their size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start.Fill(0);

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  InImageIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( value );
    ++it;
    }
}

// Function for image printing
void PrintImage( InputImageType * image, const char *)
{
  // Create an iterator for going through the image
  InImageIteratorType it( image, image->GetRequestedRegion() );

  it.GoToBegin();

  // Print the content of the image
  while( !it.IsAtEnd() )
  {
    std::cout << it.Get() << std::endl;
    ++it;
  }
}

} // end namespace NaryMaximumImageFilterTest


int itkNaryMaximumImageFilterTest( int, char* [] )
{

  // It is safe to open the namespace here because
  // the symbols will not be exposed outside this function
  using namespace NaryMaximumImageFilterTest;

  // Create two images
  InputImageType::Pointer inputImageA = InputImageType::New();
  InputImageType::Pointer inputImageB = InputImageType::New();

  static ITK_CONSTEXPR_VAR int minValue = 12;
  static ITK_CONSTEXPR_VAR int maxValue = 13;
  InitializeImage( inputImageA, minValue );
  InitializeImage( inputImageB, maxValue );

  PrintImage( inputImageA, "Input image A" );
  PrintImage( inputImageB, "Input image B" );

  // Create the  itk::NaryMaximumImageFilter filter
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, NaryMaximumImageFilter,
    NaryFunctorImageFilter );

  // Set the input images
  filter->SetInput( 0, inputImageA );
  filter->SetInput( 1, inputImageB );

  filter->SetFunctor( filter->GetFunctor() );

  // Execute the filter
  filter->Update();

  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  PrintImage( outputImage, "Resulting image 1" );

  OutImageIteratorType it( outputImage, outputImage->GetRequestedRegion() );
  it.GoToBegin();
  while( !it.IsAtEnd() )
  {
    if( itk::Math::NotExactlyEquals( it.Get(), maxValue ) )
      {
      std::cerr << "Test Failed!" << std::endl;
      return EXIT_FAILURE;
      }
    ++it;
  }

  // Now try it the other way
  InitializeImage( inputImageA, minValue );
  InitializeImage( inputImageB, maxValue );

  filter->SetInput( 1, inputImageA );
  filter->SetInput( 0, inputImageB );

  filter->InPlaceOff(); // let's make sure this works too, while we're at it...

  // Execute the filter
  filter->Update();

  PrintImage( outputImage, "Resulting image 2" );

  OutImageIteratorType it2( outputImage, outputImage->GetRequestedRegion() );
  it2.GoToBegin();
  while( !it2.IsAtEnd() )
  {
    if( itk::Math::NotExactlyEquals( it2.Get(), maxValue ) )
    {
      std::cerr << "Test Failed!" << std::endl;
      return EXIT_FAILURE;
    }
    ++it2;
  }

  std::cerr << "Test Passed!" << std::endl;

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
