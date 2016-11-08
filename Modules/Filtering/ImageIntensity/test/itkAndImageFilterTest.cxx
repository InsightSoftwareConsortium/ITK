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

#include "itkAndImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"

int itkAndImageFilterTest( int argc, char* argv[] )
{
  if ( argc < 2 )
    {
    std::cout << "Usage: " << argv[0]
      << "outputImage " << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  const unsigned int Dimension = 3;

  // Declare the pixel type
  typedef unsigned short                    PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, Dimension> InputImage1Type;
  typedef itk::Image< PixelType, Dimension> InputImage2Type;
  typedef itk::Image< PixelType, Dimension> OutputImageType;

  // Declare the type of the index to access images
  typedef itk::Index< Dimension >         IndexType;

  // Declare the type of the size
  typedef itk::Size< Dimension >          SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion< Dimension >   RegionType;

  // Create the two input images
  InputImage1Type::Pointer inputImageA = InputImage1Type::New();
  InputImage2Type::Pointer inputImageB = InputImage2Type::New();

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

  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex< InputImage1Type >
    InputImage1IteratorType;
  typedef itk::ImageRegionIteratorWithIndex< InputImage2Type >
    InputImage2IteratorType;

  // Create one iterator for Image A (this is a light object)
  InputImage1IteratorType it1( inputImageA, inputImageA->GetBufferedRegion() );
  it1.GoToBegin();

  // Initialize the content of Image A
  InputImage1Type::PixelType valueA = 2;
  while( !it1.IsAtEnd() )
  {
    it1.Set( valueA );
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  InputImage2IteratorType it2( inputImageB, inputImageB->GetBufferedRegion() );
  it2.GoToBegin();

  // Initialize the content of Image B
  InputImage2Type::PixelType valueB = 3;
  while( !it2.IsAtEnd() )
  {
    it2.Set( valueB );
    ++it2;
  }

  typedef itk::AndImageFilter<
    InputImage1Type,
    InputImage2Type,
    OutputImageType > AndImageFilterType;

  // Create the filter
  AndImageFilterType::Pointer filter = AndImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, AndImageFilter, BinaryFunctorImageFilter );

  // Set the input images
  filter->SetInput1( inputImageA );
  filter->SetInput2( inputImageB );

  filter->SetFunctor( filter->GetFunctor() );

  // Execute the filter
  filter->Update();

  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Write the result image
  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[1] );

  writer->SetInput( outputImage );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
