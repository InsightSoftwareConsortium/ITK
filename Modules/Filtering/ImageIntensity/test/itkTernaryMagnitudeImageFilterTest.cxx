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

#include "itkTernaryMagnitudeImageFilter.h"
#include "itkTestingMacros.h"

int itkTernaryMagnitudeImageFilterTest( int, char* [] )
{
  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Define the pixel types
  typedef float PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, Dimension > InputImage1Type;
  typedef itk::Image< PixelType, Dimension > InputImage2Type;
  typedef itk::Image< PixelType, Dimension > InputImage3Type;
  typedef itk::Image< PixelType, Dimension > OutputImageType;

  // Declare the type of the index to access images
  typedef itk::Index< Dimension >         IndexType;

  // Declare the type of the size
  typedef itk::Size< Dimension >          SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion< Dimension >   RegionType;

  // Create the input images
  InputImage1Type::Pointer inputImageA = InputImage1Type::New();
  InputImage2Type::Pointer inputImageB = InputImage2Type::New();
  InputImage3Type::Pointer inputImageC = InputImage3Type::New();

  // Define their size and start index
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

  // Declare Iterator types for each image
  typedef itk::ImageRegionIteratorWithIndex< InputImage1Type > InputImage1IteratorType;
  typedef itk::ImageRegionIteratorWithIndex< InputImage2Type > InputImage2IteratorType;
  typedef itk::ImageRegionIteratorWithIndex< InputImage3Type > InputImage3IteratorType;
  typedef itk::ImageRegionIteratorWithIndex< OutputImageType > OutputImageIteratorType;

  // Create one iterator for Image A (this is a light object)
  InputImage1IteratorType it1( inputImageA, inputImageA->GetBufferedRegion() );

  // Initialize the content of Image A
  std::cout << "First operand " << std::endl;
  InputImage1Type::PixelType valueA = 2.0;
  while( !it1.IsAtEnd() )
  {
    it1.Set( valueA );
    std::cout
      << itk::NumericTraits< InputImage1Type::PixelType >::PrintType( it1.Get() )
      << std::endl;
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  InputImage2IteratorType it2( inputImageB, inputImageB->GetBufferedRegion() );

  // Initialize the content of Image B
  std::cout << "Second operand " << std::endl;
  InputImage2Type::PixelType valueB = 3.0;
  while( !it2.IsAtEnd() )
  {
    it2.Set( valueB );
    std::cout
      << itk::NumericTraits< InputImage2Type::PixelType >::PrintType( it2.Get() )
      << std::endl;
    ++it2;
  }

  // Create one iterator for Image C (this is a light object)
  InputImage3IteratorType it3( inputImageC, inputImageC->GetBufferedRegion() );

  // Initialize the content of Image C
  std::cout << "Third operand " << std::endl;
  InputImage3Type::PixelType valueC = 4.0;
  while( !it3.IsAtEnd() )
  {
    it3.Set( valueC );
    std::cout
      << itk::NumericTraits< InputImage3Type::PixelType >::PrintType( it3.Get() )
      << std::endl;
    ++it3;
  }

  typedef itk::TernaryMagnitudeImageFilter<
    InputImage1Type,
    InputImage2Type,
    InputImage3Type,
    OutputImageType > TernaryMagnitudeImageFilterType;

  // Create the filter
  TernaryMagnitudeImageFilterType::Pointer filter =
    TernaryMagnitudeImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, TernaryMagnitudeImageFilter,
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
  OutputImageIteratorType it4( outputImage, outputImage->GetBufferedRegion() );

  // Print the content of the result image
  std::cout << "Result " << std::endl;
  while( !it4.IsAtEnd() )
  {
    std::cout
      << itk::NumericTraits< OutputImageType::PixelType >::PrintType( it4.Get() )
      << std::endl;
    ++it4;
  }

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
