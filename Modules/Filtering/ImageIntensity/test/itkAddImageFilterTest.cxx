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

#include "itkAddImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkTestingMacros.h"
#include <complex>


int itkAddImageFilterTest( int, char* [] )
{

  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Declare the pixel types of the images
  typedef float                               PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, Dimension >  InputImageType1;
  typedef itk::Image< PixelType, Dimension >  InputImageType2;
  typedef itk::Image< PixelType, Dimension >  OutputImageType;

  // Declare appropriate Iterator types for each image
  typedef itk::ImageRegionIteratorWithIndex< OutputImageType > OutputImageIteratorType;

  // Declare the type of the index to access images
  typedef itk::Index< Dimension >         IndexType;

  // Declare the type of the size
  typedef itk::Size< Dimension >          SizeType;

  // Declare the type of the region
  typedef itk::ImageRegion< Dimension >   RegionType;

  // Create two images
  InputImageType1::Pointer inputImageA = InputImageType1::New();
  InputImageType2::Pointer inputImageB = InputImageType2::New();

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
  inputImageA->SetLargestPossibleRegion( region );
  inputImageA->SetBufferedRegion( region );
  inputImageA->SetRequestedRegion( region );
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion( region );
  inputImageB->SetBufferedRegion( region );
  inputImageB->SetRequestedRegion( region );
  inputImageB->Allocate();

  // Initialize the content of Image A
  const InputImageType1::PixelType valueA = 2.0;
  inputImageA->FillBuffer( valueA );

  // Initialize the content of Image B
  const InputImageType2::PixelType valueB = 3.0;
  inputImageB->FillBuffer( valueB );


  // Declare the type for the itk::AddImageFilter
  typedef itk::AddImageFilter<
                               InputImageType1,
                               InputImageType2,
                               OutputImageType > FilterType;


  // Create the filter
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, AddImageFilter,
    BinaryFunctorImageFilter );

  // Connect the input images
  filter->SetInput1( inputImageA );
  filter->SetInput2( inputImageB );

  filter->SetFunctor( filter->GetFunctor() );

  // Execute the filter
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();


  // Create an iterator for going through the image output
  OutputImageIteratorType oIt( outputImage, outputImage->GetBufferedRegion() );

  // Check the content of the result image
  //
  const OutputImageType::PixelType expectedValue =
    static_cast< OutputImageType::PixelType >( valueA + valueB );
  while( !oIt.IsAtEnd() )
    {
    if( !itk::Math::ExactlyEquals( oIt.Get(), expectedValue ) )
      {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in pixel value at index [" << oIt.GetIndex() << "]" << std::endl;
      std::cerr << "Expected: " << expectedValue
        << ", but got: " << oIt.Get() << std::endl;
      return EXIT_FAILURE;
      }
    ++oIt;
    }


  // Complementary tests
  // Instantiate the filter with other pixel types
  //
  {
  typedef double                    PixelType2;
  typedef itk::Image< PixelType2 >  ImageType2;

  typedef itk::AddImageFilter< ImageType2, ImageType2, ImageType2 > FilterType2;
  FilterType2::Pointer filter2 = FilterType2::New();

  TEST_EXPECT_TRUE( !filter2.IsNull() );
  }

  {
  typedef float                       PixelType3;
  typedef std::complex< PixelType3 >  ComplexPixelType;

  typedef itk::Image< PixelType3 >        ImageType3;
  typedef itk::Image< ComplexPixelType >  ComplexImageType;

  typedef itk::AddImageFilter< ImageType3,
                              ComplexImageType,
                              ComplexImageType > FilterType3;

  FilterType3::Pointer filter3 = FilterType3::New();

  TEST_EXPECT_TRUE( !filter3.IsNull() );
  }


  // All objects should be automatically destroyed at this point
  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
