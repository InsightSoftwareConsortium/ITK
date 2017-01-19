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

#include "itkTernaryMagnitudeSquaredImageFilter.h"
#include "itkTestingMacros.h"


int itkTernaryMagnitudeSquaredImageFilterTest( int, char* [] )
{

  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Declare the pixel types of the images
  typedef float                PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, Dimension> InputImageType1;
  typedef itk::Image< PixelType, Dimension> InputImageType2;
  typedef itk::Image< PixelType, Dimension> InputImageType3;
  typedef itk::Image< PixelType, Dimension> OutputImageType;

  // Declare the type of the index to access images
  typedef itk::Index< Dimension >       IndexType;

  // Declare the type of the size
  typedef itk::Size< Dimension >        SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion< Dimension > RegionType;

  // Create the input images
  InputImageType1::Pointer inputImageA = InputImageType1::New();
  InputImageType2::Pointer inputImageB = InputImageType2::New();
  InputImageType3::Pointer inputImageC = InputImageType3::New();

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

  // Initialize Image C
  inputImageC->SetLargestPossibleRegion( region );
  inputImageC->SetBufferedRegion( region );
  inputImageC->SetRequestedRegion( region );
  inputImageC->Allocate();

  // Declare appropriate Iterator types for each image
  typedef itk::ImageRegionIteratorWithIndex< InputImageType1 >
    InputImage1IteratorType;
  typedef itk::ImageRegionIteratorWithIndex< InputImageType2 >
    InputImage2IteratorType;
  typedef itk::ImageRegionIteratorWithIndex< InputImageType3 >
    InputImage3IteratorType;
  typedef itk::ImageRegionIteratorWithIndex< OutputImageType >
    OutputImageIteratorType;

  // Create one iterator for Image A (this is a light object)
  InputImage1IteratorType it1( inputImageA, inputImageA->GetBufferedRegion() );

  // Initialize the content of Image A
  const InputImageType1::PixelType valueA = 2.0;
  while( !it1.IsAtEnd() )
  {
    it1.Set( valueA );
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  InputImage2IteratorType it2( inputImageB, inputImageB->GetBufferedRegion() );

  // Initialize the content of Image B
  const InputImageType2::PixelType valueB = 3.0;
  while( !it2.IsAtEnd() )
  {
    it2.Set( valueB );
    ++it2;
  }

  // Create one iterator for Image C (this is a light object)
  InputImage3IteratorType it3( inputImageC, inputImageC->GetBufferedRegion() );

  // Initialize the content of Image C
  const InputImageType3::PixelType valueC = 4.0;
  while( !it3.IsAtEnd() )
  {
    it3.Set( valueC );
    ++it3;
  }


  // Declare the type for the TernaryMagnitudeSquaredImageFilter
  typedef itk::TernaryMagnitudeSquaredImageFilter<
                                InputImageType1,
                                InputImageType2,
                                InputImageType3,
                                OutputImageType > FilterType;


  // Create the filter
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, TernaryMagnitudeSquaredImageFilter,
    TernaryFunctorImageFilter );

  // Set the input images
  filter->SetInput1( inputImageA );
  filter->SetInput2( inputImageB );
  filter->SetInput3( inputImageC );

  filter->SetFunctor( filter->GetFunctor() );

  // Execute the filter
  filter->Update();

  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the image output
  OutputImageIteratorType oIt( outputImage, outputImage->GetBufferedRegion() );

  // Check the content of the result image
  const float epsilon = 1e-6;
  oIt.GoToBegin();
  it1.GoToBegin();
  it2.GoToBegin();
  it3.GoToBegin();
  while( !oIt.IsAtEnd() )
    {
    PixelType outputValue = static_cast< OutputImageType::PixelType >(
      it1.Get() * it1.Get() + it2.Get() * it2.Get() + it3.Get() * it3.Get() );
    if( !itk::Math::FloatAlmostEqual( oIt.Get(), outputValue, 10, epsilon ) )
      {
      std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( epsilon ) ) ) );
      std::cerr << "Error " << std::endl;
      std::cerr << "Value should be  " << outputValue << std::endl;
      std::cerr << "but is           " << oIt.Get()  << std::endl;
      return EXIT_FAILURE;
      }
    ++oIt;
    ++it1;
    ++it2;
    ++it3;
    }

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
