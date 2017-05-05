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

#include "itkVectorImage.h"
#include "itkDivideImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"


int itkDivideImageFilterTest2( int, char* [] )
{

  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Declare the pixel types of the images
  typedef float ElementPixelType;

  // Declare the types of the images
  typedef itk::VectorImage< ElementPixelType, Dimension >  InputImageType1;
  typedef itk::Image< ElementPixelType, Dimension >        InputImageType2;
  typedef itk::VectorImage< ElementPixelType, Dimension >  OutputImageType;

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
  inputImageA->SetNumberOfComponentsPerPixel( 4 );
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion( region );
  inputImageB->SetBufferedRegion( region );
  inputImageB->SetRequestedRegion( region );
  inputImageB->Allocate();

  // Initialize the content of Image A
  InputImageType1::PixelType valueA( inputImageA->GetNumberOfComponentsPerPixel() );
  InputImageType1::PixelType::ValueType elementValueA = 2.0;
  valueA.Fill( elementValueA );
  inputImageA->FillBuffer( valueA );

  // Initialize the content of Image B
  const InputImageType2::PixelType valueB = 3.0;
  inputImageB->FillBuffer( valueB );


  // Declare the type for the itk::DivideImageFilter
  typedef itk::DivideImageFilter<
                                InputImageType1,
                                InputImageType2,
                                OutputImageType > FilterType;

  // Create the filter
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, DivideImageFilter,
    BinaryFunctorImageFilter );

  // Set the input images
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
  const OutputImageType::PixelType::ValueType expectedValue =
    static_cast< OutputImageType::PixelType::ValueType >( elementValueA / valueB );
  const OutputImageType::PixelType::ValueType epsilon = 1e-6;
  while( !oIt.IsAtEnd() )
    {
    for( unsigned int i = 0; i < oIt.GetImageDimension(); ++i )
      {
      if( !itk::Math::FloatAlmostEqual( oIt.Get()[i], expectedValue, 2, epsilon ) )
        {
        std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( epsilon ) ) ) );
        std::cerr << "Test failed!" << std::endl;
        std::cerr << "Error in pixel value at index [" << oIt.GetIndex()
          << ":" << i << "]" << std::endl;
        std::cerr << "Expected value " << expectedValue << std::endl;
        std::cerr << " differs from " << oIt.Get()[i];
        std::cerr << " by more than " << epsilon << std::endl;
        return EXIT_FAILURE;
        }
      }
    ++oIt;
    }


  // Check for exception if constant is 0
  filter->SetInput2( 0.0 );
  TRY_EXPECT_EXCEPTION( filter->Update() );


  // All objects should be automatically destroyed at this point
  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;}
