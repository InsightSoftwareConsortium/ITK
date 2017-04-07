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

#include "itkNaryAddImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

#include <iostream>


// Function for image initialization
template< typename ImageType >
void InitializeImage( ImageType * image, const typename ImageType::PixelType & value )
{
  typename ImageType::Pointer inputImage( image );

  // Define their size, and start index
  typename ImageType::SizeType size;
  size.Fill( 2 );

  typename ImageType::IndexType start;
  start.Fill( 0 );

  typename ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  inputImage->FillBuffer( value );
}


int itkNaryAddImageFilterTest( int, char* [] )
{
  bool testStatus = true;

  // Define the dimension of the images
  const unsigned int Dimension3D = 3;

  // Declare the pixel types of the images
  typedef float PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, Dimension3D > InputImageType;
  typedef itk::Image< PixelType, Dimension3D > OutputImageType;

  // Create some images
  InputImageType::Pointer inputImageA = InputImageType::New();
  InputImageType::Pointer inputImageB = InputImageType::New();
  InputImageType::Pointer inputImageC = InputImageType::New();


  const InputImageType::PixelType valueA = 12;
  InitializeImage< InputImageType >( inputImageA, valueA );
  const InputImageType::PixelType valueB = 17;
  InitializeImage< InputImageType >( inputImageB, valueB );
  const InputImageType::PixelType valueC = -4;
  InitializeImage< InputImageType >( inputImageC, valueC );


  // Declare the type for the itk::NaryAddImageFilter
  typedef itk::NaryAddImageFilter<
                              InputImageType,
                              OutputImageType > FilterType;

  // Create the filter
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, NaryAddImageFilter,
    NaryFunctorImageFilter );

  // Set the input images
  filter->SetInput( 0, inputImageA );
  filter->SetInput( 1, inputImageB );
  filter->SetInput( 2, inputImageC );

  filter->SetFunctor( filter->GetFunctor() );

  // Execute the filter
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Test the validity of the output
  typedef itk::ImageRegionConstIterator< InputImageType >  InputImageIteratorType;
  typedef itk::ImageRegionConstIterator< OutputImageType > OutputImageIteratorType;

  InputImageIteratorType iterA( inputImageA, inputImageA->GetRequestedRegion() );
  InputImageIteratorType iterB( inputImageB, inputImageA->GetRequestedRegion() );
  InputImageIteratorType iterC( inputImageC, inputImageA->GetRequestedRegion() );
  OutputImageIteratorType oIt( outputImage, inputImageA->GetRequestedRegion() );

  const OutputImageType::PixelType epsilon = 1e-9;
  unsigned int failures = 0;
  while( !oIt.IsAtEnd() )
    {
    OutputImageType::PixelType expectedValue =
      static_cast< OutputImageType::PixelType >( iterA.Get() + iterB.Get() + iterC.Get() );
    if( !itk::Math::FloatAlmostEqual( oIt.Get(), expectedValue, 10, epsilon ) )
      {
      ++failures;
      }
    ++iterA;
    ++iterB;
    ++iterC;
    ++oIt;
    }

  if( failures > 0 )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Got " << failures << " different pixels." << std::endl;
    testStatus = false;
    }


  // Execute the filter in place
  filter->InPlaceOn();

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Test the validity of the output
  OutputImageIteratorType oIt2( outputImage, inputImageA->GetRequestedRegion() );
  failures = 0;
  while( !oIt2.IsAtEnd() )
    {
    // Here we cannot test using the input iterators anymore since
    // inputImageA should have been overwritten
    OutputImageType::PixelType expectedValue =
      static_cast< OutputImageType::PixelType >( valueA + valueB + valueC );
    if( !itk::Math::FloatAlmostEqual( oIt2.Get(), expectedValue, 10, epsilon ) )
      {
      ++failures;
      }
    ++oIt2;
    }

  if( failures > 0 )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Got " << failures << " different pixels." << std::endl;
    testStatus = false;
    }


  // Testing with vector Images
  //

  // Define the dimension of the images
  const unsigned int Dimension2D = 2;

  // Declare the pixel types of the images
  typedef int ElementPixelType;

  typedef itk::Vector< ElementPixelType, Dimension2D >  VectorPixelType;
  typedef itk::Image< VectorPixelType, Dimension2D >    VectorImageType;

  VectorImageType::Pointer vectorImageA = VectorImageType::New();
  VectorImageType::Pointer vectorImageB = VectorImageType::New();
  VectorImageType::Pointer vectorImageC = VectorImageType::New();

  VectorPixelType vectorImageValueA, vectorImageValueB, vectorImageValueC;

  const VectorImageType::PixelType::ValueType vectorValueA = 12;
  vectorImageValueA.Fill( vectorValueA );
  vectorImageValueA[0] = 5;

  const VectorImageType::PixelType::ValueType  vectorValueB = 17;
  vectorImageValueB.Fill( vectorValueB );
  vectorImageValueB[0] = 9;

  const VectorImageType::PixelType::ValueType vectorValueC = -4;
  vectorImageValueC.Fill( vectorValueC );
  vectorImageValueC[0] = -80;

  InitializeImage< VectorImageType >( vectorImageA, vectorImageValueA );
  InitializeImage< VectorImageType >( vectorImageB, vectorImageValueB );
  InitializeImage< VectorImageType >( vectorImageC, vectorImageValueC );


  // Create an ADD Filter
  typedef itk::NaryAddImageFilter<
                              VectorImageType,
                              VectorImageType > VectorAdderType;

  VectorAdderType::Pointer vectorFilter = VectorAdderType::New();

  EXERCISE_BASIC_OBJECT_METHODS( vectorFilter, NaryAddImageFilter,
    NaryFunctorImageFilter );


  // Set the input images
  vectorFilter->SetInput( 0, vectorImageA );
  vectorFilter->SetInput( 1, vectorImageB );
  vectorFilter->SetInput( 2, vectorImageC );

  // Get the filter output
  VectorImageType::Pointer vectorOutputImage = vectorFilter->GetOutput();


  // Execute the filter
  TRY_EXPECT_NO_EXCEPTION( vectorFilter->Update() );


  // Test the validity of the output
  typedef itk::ImageRegionConstIterator< VectorImageType > VectorIteratorType;

  VectorIteratorType vIterA( vectorImageA, vectorImageA->GetRequestedRegion() );
  VectorIteratorType vIterB( vectorImageB, vectorImageA->GetRequestedRegion() );
  VectorIteratorType vIterC( vectorImageC, vectorImageA->GetRequestedRegion() );
  VectorIteratorType vOutIter( vectorOutputImage, vectorImageA->GetRequestedRegion() );

  failures = 0;
  while( !vOutIter.IsAtEnd() )
    {
    VectorImageType::PixelType expectedValue =
      static_cast< VectorImageType::PixelType >( vIterA.Get() + vIterB.Get() + vIterC.Get() );
    for( unsigned int i = 0; i < vOutIter.GetImageIteratorDimension(); ++i )
      {
        if( !itk::Math::ExactlyEquals( vOutIter.Get()[i], expectedValue[i] ) )
        {
        ++failures;
        }
      }
    ++vIterA;
    ++vIterB;
    ++vIterC;
    ++vOutIter;
    }

  if( failures > 0 )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Got " << failures << " different pixels." << std::endl;
    testStatus = false;
    }

  if( !testStatus )
    {
    return EXIT_FAILURE;
    }

  // All objects should be automatically destroyed at this point
  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
