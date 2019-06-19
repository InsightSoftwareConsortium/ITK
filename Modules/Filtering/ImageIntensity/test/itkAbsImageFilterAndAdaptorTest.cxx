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

#include "itkAbsImageFilter.h"
#include "itkAbsImageAdaptor.h"
#include "itkImageAdaptor.h"
#include "itkMath.h"
#include "itkSubtractImageFilter.h"
#include "itkUnaryGeneratorImageFilter.h"
#include "itkTestingMacros.h"

int itkAbsImageFilterAndAdaptorTest(int, char* [] )
{
  int testStatus = EXIT_SUCCESS;

  // Define the dimension of the images
  constexpr unsigned int ImageDimension = 3;

  // Declare the types of the images
  using InputImageType = itk::Image<float, ImageDimension>;
  using OutputImageType = itk::Image<float, ImageDimension>;

  // Declare Iterator types apropriated for each image
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;
  using OutputIteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;

  // Declare the type of the index to access images
  using IndexType = itk::Index<ImageDimension>;

  // Declare the type of the size
  using SizeType = itk::Size<ImageDimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<ImageDimension>;

  // Create two images
  InputImageType::Pointer inputImage = InputImageType::New();

  // Define their size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();
  // Create one iterator for the Input Image (this is a light object)
  InputIteratorType it( inputImage, inputImage->GetBufferedRegion() );

  // Initialize the content of Image A
  const double pi    = std::atan( 1.0 ) * 4.0;
  const double value = pi / 6.0;
  std::cout << "Content of the Input " << std::endl;
  it.GoToBegin();
  while( !it.IsAtEnd() )
  {
    it.Set( value );
    std::cout << it.Get() << std::endl;
    ++it;
  }

  // Declare the type for the Abs filter
  using FilterType = itk::AbsImageFilter< InputImageType,
                               OutputImageType >;

  // Create an Abs Filter
  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS( filter, AbsImageFilter, UnaryGeneratorImageFilter );

  // Connect the input images
  filter->SetInput( inputImage );

  // Get the Smart Pointer to the Filter Output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Execute the filter
  filter->Update();

  // Create an iterator for going through the image output
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());

  //  Check the content of the result image
  std::cout << "Verification of the output " << std::endl;
  const OutputImageType::PixelType epsilon = 1e-6;
  ot.GoToBegin();
  it.GoToBegin();
  while( !ot.IsAtEnd() )
    {
    std::cout.precision( static_cast< int >( itk::Math::abs( std::log10( epsilon ) ) ) );
    std::cout << ot.Get() << " = ";
    std::cout << itk::Math::abs( it.Get() ) << std::endl;
    const InputImageType::PixelType  input  = it.Get();
    const OutputImageType::PixelType output = ot.Get();
    const OutputImageType::PixelType absolute = itk::Math::abs(input);
    if( !itk::Math::FloatAlmostEqual( absolute, output, 10, epsilon ) )
      {
      std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( epsilon ) ) ) );
      std::cerr << "Error in itkAbsImageFilterTest " << std::endl;
      std::cerr << " abs(" << input << ") = " << absolute << std::endl;
      std::cerr << " differs from " << output;
      std::cerr << " by more than " << epsilon << std::endl;
      testStatus = EXIT_FAILURE;
      }
    ++ot;
    ++it;
    }

  //
  // Test AbsImageAdaptor
  //

  using AdaptorType = itk::AbsImageAdaptor< InputImageType,
                          OutputImageType::PixelType >;

  AdaptorType::Pointer absAdaptor = AdaptorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS( absAdaptor, AbsImageAdaptor, ImageAdaptor );

  absAdaptor->SetImage( inputImage );

  using DiffFilterType = itk::SubtractImageFilter<
                        OutputImageType,
                        AdaptorType,
                        OutputImageType >;

  DiffFilterType::Pointer diffFilter = DiffFilterType::New();

  diffFilter->SetInput1( outputImage );
  diffFilter->SetInput2( absAdaptor  );

  diffFilter->Update();

  // Get the Smart Pointer to the Diff filter Output
  OutputImageType::Pointer diffImage = diffFilter->GetOutput();

  //  Check the content of the diff image
  std::cout << "Comparing the results with those of an Adaptor" << std::endl;
  std::cout << "Verification of the output " << std::endl;

  // Create an iterator for going through the image output
  OutputIteratorType dt(diffImage, diffImage->GetRequestedRegion());

  dt.GoToBegin();
  while( !dt.IsAtEnd() )
    {
    std::cout.precision( static_cast< int >( itk::Math::abs( std::log10( epsilon ) ) ) );
    std::cout << dt.Get() << std::endl;
    const OutputImageType::PixelType diff = dt.Get();
    if( !itk::Math::FloatAlmostEqual( diff, ( OutputImageType::PixelType )0, 10, epsilon ) )
      {
      std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( epsilon ) ) ) );
      std::cerr << "Error in itkAbsImageFilterTest " << std::endl;
      std::cerr << "Comparing results with Adaptors" << std::endl;
      std::cerr << " difference = " << diff << std::endl;
      std::cerr << " differs from 0 ";
      std::cerr << " by more than " << epsilon << std::endl;
      testStatus = EXIT_FAILURE;
      }
    ++dt;
    }

  return testStatus;
}
