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

#include <fstream>
#include "itkMatrixIndexSelectionImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int itkMatrixIndexSelectionImageFilterTest( int argc, char* argv[] )
{
  if( argc < 1 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  const unsigned int Dimension = 2;

  // Declare the pixel types of the images
  typedef itk::Matrix< unsigned short, Dimension, Dimension > PixelType;
  typedef unsigned char                                       OutputPixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, Dimension >       InputImageType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  // Create a matrix image
  InputImageType::Pointer image = InputImageType::New();
  InputImageType::RegionType region;

  InputImageType::SizeType size;
  size.Fill( 100 );

  InputImageType::IndexType index;
  index.Fill( 0 );

  region.SetSize( size );
  region.SetIndex( index );
  image->SetRegions( region );
  image->Allocate();

  size = region.GetSize();
  index = region.GetIndex();
  unsigned int width = size[0];
  unsigned int height = size[1];

  size[0] = width;
  size[1] = height / 2;

  // Populate upper half of image
  index[0] = 0;
  index[1] = 0;
  region.SetSize( size );
  region.SetIndex( index );
  {
  PixelType pixel;
  pixel[0][0] = 128;
  pixel[0][1] = 192;
  pixel[1][0] =   0;
  pixel[1][1] =  64;

  itk::ImageRegionIterator< InputImageType > it( image, region );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( pixel );
    ++it;
    }
  }

  // Populate lower half of image
  index[0] = 0;
  index[1] = height / 2;
  region.SetSize( size );
  region.SetIndex( index );
  {
  PixelType pixel;
  pixel[0][0] =  64;
  pixel[0][1] =  16;
  pixel[1][0] = 255;
  pixel[1][1] = 192;

  itk::ImageRegionIterator< InputImageType > it( image, region );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( pixel );
    ++it;
    }
  }

  typedef itk::MatrixIndexSelectionImageFilter< InputImageType, OutputImageType >
    SelectionFilterType;

  SelectionFilterType::Pointer filter = SelectionFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, MatrixIndexSelectionImageFilter,
    UnaryFunctorImageFilter );

  filter->SetInput( image );

  unsigned int indexA = 0;
  unsigned int indexB = 1;
  filter->SetIndices( indexA, indexB );

  unsigned int testIndexA;
  unsigned int testIndexB;
  filter->GetIndices( testIndexA, testIndexB );

  if( indexA != testIndexA || indexB != testIndexB )
    {
    std::cerr << "Error " << std::endl;
    std::cerr << " Expected indices: ("
      << indexA << ", "<< indexB << ")" << std::endl;
    std::cerr << " differ from ";
    std::cerr << " obtained indices: ("
      << testIndexA << ", "<< testIndexB << ")" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetFunctor( filter->GetFunctor() );

  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[1] );
  writer->SetInput( filter->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
