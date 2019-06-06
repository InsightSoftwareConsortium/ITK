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

#include "itkConstrainedValueAdditionImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"


int itkConstrainedValueAdditionImageFilterTest( int argc, char* argv[] )
{
  if ( argc < 2 )
    {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv)
      << "outputImage " << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  constexpr unsigned int Dimension = 3;

  // Define the pixel types
  using InputPixelType = float;
  using OutputPixelType = unsigned short;

  // Declare the types of the images
  using InputImageType1 = itk::Image< InputPixelType, Dimension >;
  using InputImageType2 = itk::Image< InputPixelType, Dimension >;
  using OutputImageType = itk::Image< OutputPixelType, Dimension >;

  // Declare the type of the index to access images
  using IndexType = itk::Index< Dimension >;

  // Declare the type of the size
  using SizeType = itk::Size< Dimension >;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion< Dimension >;

  // Create the input images
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

  // Declare Iterator types apropriated for each image
  using InputIteratorType1 = itk::ImageRegionIteratorWithIndex< InputImageType1 >;
  using InputIteratorType2 = itk::ImageRegionIteratorWithIndex< InputImageType2 >;

  // Create one iterator for Image A (this is a light object)
  InputIteratorType1 it1( inputImageA, inputImageA->GetBufferedRegion() );

  // Initialize the content of Image A
  float valueA = 125; // when added to B will saturate a char in some of the pixels.
  while( !it1.IsAtEnd() )
    {
    it1.Set( valueA );
    ++it1;
    valueA += 1.0;
    }

  // Create one iterator for Image B (this is a light object)
  InputIteratorType2 it2( inputImageB, inputImageB->GetBufferedRegion() );

  // Initialize the content of Image B
  float valueB = 125; // when added to A will saturate a char in some of the pixels.
  while( !it2.IsAtEnd() )
    {
    it2.Set( valueB );
    ++it2;
    valueB += 1.0;
    }

  // Declare the type for the ADD filter
  using ConstrainedValueAdditionImageFilterType = itk::ConstrainedValueAdditionImageFilter<
    InputImageType1,
    InputImageType2,
    OutputImageType >;

  // Create the filter
  ConstrainedValueAdditionImageFilterType::Pointer filter =
    ConstrainedValueAdditionImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS( filter, ConstrainedValueAdditionImageFilter,
    BinaryGeneratorImageFilter );

  // Set the input images
  filter->SetInput1( inputImageA );
  filter->SetInput2( inputImageB );

  // Execute the filter
  filter->Update();

  // Get the filter Output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Write the result image
  using WriterType = itk::ImageFileWriter< OutputImageType >;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[1] );

  writer->SetInput( outputImage );

  ITK_TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
