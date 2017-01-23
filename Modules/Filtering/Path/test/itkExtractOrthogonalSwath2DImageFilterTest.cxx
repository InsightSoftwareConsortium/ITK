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

#include "itkPolyLineParametricPath.h"
#include "itkChainCodePath.h"
#include "itkFourierSeriesPath.h"
#include "itkPathToChainCodePathFilter.h"
#include "itkChainCodeToFourierSeriesPathFilter.h"
#include "itkExtractOrthogonalSwath2DImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int itkExtractOrthogonalSwath2DImageFilterTest( int argc, char* argv[] )
{
  if( argc != 2)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " outputImage " << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;
  typedef unsigned char                             PixelType;

  typedef itk::Image< PixelType, Dimension >        ImageType;
  typedef itk::PolyLineParametricPath< Dimension >  PolyLineParametricPathType;
  typedef itk::ChainCodePath< Dimension >           ChainCodePathType;
  typedef itk::FourierSeriesPath< Dimension >       FourierSeriesPathType;

  typedef PolyLineParametricPathType::VertexType    VertexType;
  typedef ImageType::IndexType                      IndexType;

  typedef itk::PathToChainCodePathFilter< PolyLineParametricPathType, ChainCodePathType >
    PathToChainCodePathFilterType;
  typedef itk::ChainCodeToFourierSeriesPathFilter< ChainCodePathType, FourierSeriesPathType >
    ChainCodeToFourierSeriesPathFilterType;
  typedef itk::ExtractOrthogonalSwath2DImageFilter< ImageType >
    ExtractOrthogonalSwath2DImageFilterType;


  // Set up the image
  std::cout << "Making a 64x64 white square centered in a 128x128 black image" << std::endl;
  ImageType::Pointer inputImage = ImageType::New();
  IndexType start;
  start[0] = 0;
  start[1] = 0;
  ImageType::SizeType size;
  size[0] = 128;
  size[1] = 128;
  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( start );
  inputImage->SetRegions( region );
  double spacing[ImageType::ImageDimension];
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  inputImage->SetSpacing( spacing );

  inputImage->Allocate();

  typedef itk::ImageRegionIterator< ImageType > ImageRegionIteratorType;
  ImageRegionIteratorType it( inputImage, inputImage->GetRequestedRegion() );
  it.GoToBegin();
  IndexType pixelIndex;
  while( !it.IsAtEnd() )
    {
    pixelIndex = it.GetIndex();
    if( pixelIndex[0] >= int(size[0]/4) && pixelIndex[0] < int(size[0]*3/4) &&
        pixelIndex[1] >= int(size[1]/4) && pixelIndex[1] < int(size[1]*3/4) )
      {
      it.Set( 255 );
      }
    else
      {
      it.Set( 0 );
      }
    ++it;
    }

  // Set up the path
  std::cout << "Making a square Path with v0 at (24,24) -> (24,104) -> (104,104) -> (104,24)" << std::endl;
  PolyLineParametricPathType::Pointer inputPath = PolyLineParametricPathType::New();
  VertexType v;
  v.Fill( 24 );
  inputPath->AddVertex( v );
  v[0] = 24;
  v[1] = 104;
  inputPath->AddVertex( v );
  v.Fill( 104 );
  inputPath->AddVertex( v );
  v[0] = 104;
  v[1] = 24;
  inputPath->AddVertex( v );
  v.Fill( 24 );
  inputPath->AddVertex( v );

  // Set up the first filter
  PathToChainCodePathFilterType::Pointer pathToChainCodePathFilter =
    PathToChainCodePathFilterType::New();
  pathToChainCodePathFilter->SetInput( inputPath );
  ChainCodePathType::Pointer chainPath = pathToChainCodePathFilter->GetOutput();

  // Set up the second filter
  ChainCodeToFourierSeriesPathFilterType::Pointer chainCodeToFourierSeriesPathFilte =
    ChainCodeToFourierSeriesPathFilterType::New();
  chainCodeToFourierSeriesPathFilte->SetInput( pathToChainCodePathFilter->GetOutput() );
  chainCodeToFourierSeriesPathFilte->SetNumberOfHarmonics( 7 ); // make a nice, round, path for the swath
  FourierSeriesPathType::Pointer outputPath = chainCodeToFourierSeriesPathFilte->GetOutput();

  // Set up the third filter; THIS IS THE MAIN FILTER TO BE TESTED
  ExtractOrthogonalSwath2DImageFilterType::Pointer extractOrthogonalSwath2DImageFilter =
    ExtractOrthogonalSwath2DImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( extractOrthogonalSwath2DImageFilter,
    ExtractOrthogonalSwath2DImageFilter, ImageAndPathToImageFilter );

  extractOrthogonalSwath2DImageFilter->SetImageInput( inputImage );
  extractOrthogonalSwath2DImageFilter->SetPathInput( chainCodeToFourierSeriesPathFilte->GetOutput() );
  // Set the desired size of the filter's output
  size[0] = 512;
  size[1] = 21*2 + 1;
  extractOrthogonalSwath2DImageFilter->SetSize( size );

  // Set up the output
  ImageType::Pointer outputImage = extractOrthogonalSwath2DImageFilter->GetOutput();

  // Test spacing
  double pathImageSpacing[ImageType::ImageDimension];

  for( unsigned int i = 0; i < Dimension; ++i )
    {
    pathImageSpacing[i] = 1.0;
    }
  extractOrthogonalSwath2DImageFilter->SetSpacing( pathImageSpacing );
  const double* spacing_result = extractOrthogonalSwath2DImageFilter->GetSpacing();

  for( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
    {
    if( spacing_result[i] != 1.0 )
      {
      std::cout << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Test origin
  double pathImageOrigin[ImageType::ImageDimension];

  for( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
    {
    pathImageOrigin[i] = 0.0;
    }
  extractOrthogonalSwath2DImageFilter->SetOrigin( pathImageOrigin );
  const double* origin_result = extractOrthogonalSwath2DImageFilter->GetOrigin();

  for( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
    {
    if( origin_result[i] != 0.0 )
      {
      std::cout << "Test failed" << std::endl;
      return EXIT_FAILURE;
      }
    }


  // Update the pipeline
  TRY_EXPECT_NO_EXCEPTION( outputImage->Update() );

  // Test pipeline execution
  //

  // Test only pixels definitely inside or outside the original white square
  ImageType::IndexType index;

  for( unsigned int col = 0; col < size[0]; ++col )
    {
    index[0] = col;
    index[1] = 1;
    if( outputImage->GetPixel( index ) != 255 )
      {
      std::cout << "index " << index << " = " << int( outputImage->GetPixel( index ) ) << ": [FAILURE]" << std::endl;
      return EXIT_FAILURE;
      }

    index[0] = col;
    index[1] = size[1] - 2;
    if( outputImage->GetPixel( index ) != 0 )
      {
      std::cout << "index "<< index << " = " << int( outputImage->GetPixel( index ) ) << ": [FAILURE]" << std::endl;
      return EXIT_FAILURE;
      }
    }

  itk::ImageFileWriter< ImageType >::Pointer writer =
    itk::ImageFileWriter< ImageType >::New();
  writer->SetInput( extractOrthogonalSwath2DImageFilter->GetOutput() );
  writer->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( writer->Write() );

  std::cout << "Test finished" << std::endl;

  return EXIT_SUCCESS;
}
