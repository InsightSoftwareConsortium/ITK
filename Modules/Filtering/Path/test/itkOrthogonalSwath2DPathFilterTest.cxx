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

#include "itkRescaleIntensityImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkExtractOrthogonalSwath2DImageFilter.h"
#include "itkDerivativeImageFilter.h"

#include "itkOrthogonalSwath2DPathFilter.h"

#include "itkPathToImageFilter.h"
#include "itkTestingMacros.h"


int itkOrthogonalSwath2DPathFilterTest( int, char*[] )
{
  const unsigned int Dimension = 2;
  typedef unsigned char UCharPixelType;
  typedef double        DoublePixelType;

  typedef itk::Image< UCharPixelType, Dimension >     UCharImageType;
  typedef itk::Image< DoublePixelType, Dimension >    DoubleImageType;
  typedef itk::PolyLineParametricPath< Dimension >    PolyLineParametricPathType;
  typedef itk::ChainCodePath<2>                       ChainCodePathType;
  typedef itk::FourierSeriesPath<2>                   FourierSeriesPathType;

  typedef PolyLineParametricPathType::VertexType      VertexType;

  typedef UCharImageType::IndexType                   IndexType;

  typedef itk::OrthogonallyCorrected2DParametricPath  OutputPathType;

  // Image pre-processing filter typedefs
  typedef itk::RescaleIntensityImageFilter< UCharImageType, DoubleImageType >
    CastFilterType;
  typedef itk::DiscreteGaussianImageFilter< DoubleImageType, DoubleImageType >
    SmoothFilterType;
  typedef itk::ExtractOrthogonalSwath2DImageFilter< DoubleImageType >
    SwathFilterType;
  typedef itk::DerivativeImageFilter< DoubleImageType, DoubleImageType >
    MeritFilterType;

  // Path pre-processing filter typedefs
  typedef itk::PathToChainCodePathFilter< PolyLineParametricPathType, ChainCodePathType >
    PathToChainCodePathFilterType;
  typedef itk::ChainCodeToFourierSeriesPathFilter< ChainCodePathType, FourierSeriesPathType>
    ChainCodeToFourierSeriesPathFilterType;
  typedef itk::OrthogonalSwath2DPathFilter< FourierSeriesPathType, DoubleImageType >
    OrthogonalSwath2DPathFilterType;

  // Filter typedefs for saving results
  typedef itk::PathToImageFilter< FourierSeriesPathType, UCharImageType >
    FourierSeriesPathToImageFilterType;
  typedef itk::PathToImageFilter< OutputPathType, UCharImageType >
    OrthogonallyCorrected2DParametricPathToImageFilterType;
  typedef itk::RescaleIntensityImageFilter< DoubleImageType, UCharImageType >
    RescaleIntensityImageFilterType;

  // Set up the path
  std::cout << "Making a square Path with v0 at (24,24) -> (24,104) -> (104,104) -> (104,24)" << std::endl;
  PolyLineParametricPathType::Pointer inputPath = PolyLineParametricPathType::New();

  VertexType v;
  v.Fill( 24 );
  inputPath->AddVertex( v );
  v[0] = 24;
  v[1] = 104;
  inputPath->AddVertex( v );
  v.Fill(104);
  inputPath->AddVertex( v );
  v[0] = 104;
  v[1] = 24;
  inputPath->AddVertex( v );
  v.Fill( 24 );
  inputPath->AddVertex( v );

  // Set up the first path filter
  PathToChainCodePathFilterType::Pointer pathToChainCodePathFilter =
    PathToChainCodePathFilterType::New();
  pathToChainCodePathFilter->SetInput( inputPath );

  // Set up the second path filter
  ChainCodeToFourierSeriesPathFilterType::Pointer chainCodeToFourierSeriesPathFilter =
    ChainCodeToFourierSeriesPathFilterType::New();
  chainCodeToFourierSeriesPathFilter->SetInput( pathToChainCodePathFilter->GetOutput() );
  chainCodeToFourierSeriesPathFilter->SetNumberOfHarmonics( 7 ); // make a nice, round, path for the swath

  // Set up the image
  std::cout << "Making a 64x64 black square centered in a 128x128 white image" << std::endl;
  UCharImageType::Pointer inputImage = UCharImageType::New();
  IndexType start;
  start[0] = 0;
  start[1] = 0;
  UCharImageType::SizeType size;
  size[0] = 128;
  size[1] = 128;
  UCharImageType::RegionType region;
  region.SetSize (size );
  region.SetIndex( start );
  inputImage->SetRegions( region );
  double spacing[UCharImageType::ImageDimension];
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  inputImage->SetSpacing( spacing );

  inputImage->Allocate();

  typedef itk::ImageRegionIterator< UCharImageType > ImageRegionIteratorType;
  ImageRegionIteratorType it( inputImage, inputImage->GetRequestedRegion() );
  it.GoToBegin();
  IndexType pixelIndex;
  while( !it.IsAtEnd() )
    {
    pixelIndex = it.GetIndex();
    if( pixelIndex[0] >= int(size[0]/4) && pixelIndex[0] < int(size[0]*3/4) &&
        pixelIndex[1] >= int(size[1]/4) && pixelIndex[1] < int(size[1]*3/4) )
      {
      it.Set( 0 );
      }
    else
      {
      it.Set( 255 );
      }
    ++it;
    }

  // Cast the input image into a double image
  CastFilterType::Pointer castFilter = CastFilterType::New();
  castFilter->SetInput( inputImage );
  castFilter->SetOutputMinimum( 0 );
  castFilter->SetOutputMaximum( 1.0 );

  // Smooth the (double pixel type) input image
  SmoothFilterType::Pointer smoothFilter = SmoothFilterType::New();
  smoothFilter->SetInput( castFilter->GetOutput() );
  double gaussianVariance = 1.0;
  // We want a fast 3x3 kernel. A Gausian operator will not truncate its kernel
  // width to any less than a 5x5 kernel (kernel width of 3 for 1 center pixel +
  // 2 edge pixels). However, a Gausian operator always uses at least a 3x3
  // kernel, and so setting the maximum error to 1.0 (no limit) will make it
  // stop growing the kernel at the desired 3x3 size.
  double maxError = 0.9;
  smoothFilter->SetUseImageSpacingOff();
  smoothFilter->SetVariance( gaussianVariance );
  smoothFilter->SetMaximumError( maxError );

  // Extract the swath image
  SwathFilterType::Pointer swathFilter = SwathFilterType::New();
  swathFilter->SetImageInput( smoothFilter->GetOutput() );
  swathFilter->SetPathInput( chainCodeToFourierSeriesPathFilter->GetOutput() );
  size[0] = 512;
  size[1] = 16*2 + 1; // the top 1 and bottom 1 rows are dropped when smoothing
  swathFilter->SetSize( size );

  // Find the vertical gradient of the swath image
  MeritFilterType::Pointer meritFilter = MeritFilterType::New();
  meritFilter->SetInput( swathFilter->GetOutput() );
  meritFilter->SetOrder( 1 ); // first partial derivative
  meritFilter->SetDirection( 1 ); // d/dy

  // Set up the test OrthogonalSwath2DPathFilter
  OrthogonalSwath2DPathFilterType::Pointer orthogonalSwath2DPathFilter =
    OrthogonalSwath2DPathFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( orthogonalSwath2DPathFilter,
    OrthogonalSwath2DPathFilter, PathAndImageToPathFilter );

  orthogonalSwath2DPathFilter->SetPathInput(
    chainCodeToFourierSeriesPathFilter->GetOutput() );
  orthogonalSwath2DPathFilter->SetImageInput( meritFilter->GetOutput() );
  OutputPathType::Pointer outPath = orthogonalSwath2DPathFilter->GetOutput();

  // Set up the input & output path images
  FourierSeriesPathToImageFilterType::Pointer fourierSeriesPathToImageFilter =
    FourierSeriesPathToImageFilterType::New();
  OrthogonallyCorrected2DParametricPathToImageFilterType::Pointer
    orthogonallyCorrected2DParametricPathToImageFilter =
    OrthogonallyCorrected2DParametricPathToImageFilterType::New();
  size[0] = 128;
  size[1] = 128;
  fourierSeriesPathToImageFilter->SetSize( size );  // same size as the input image
  fourierSeriesPathToImageFilter->SetPathValue( 255 );
  fourierSeriesPathToImageFilter->SetInput( orthogonalSwath2DPathFilter->GetPathInput() );

  orthogonallyCorrected2DParametricPathToImageFilter->SetSize( size );  // same size as the input image
  orthogonallyCorrected2DParametricPathToImageFilter->SetPathValue( 255 );
  orthogonallyCorrected2DParametricPathToImageFilter->SetInput(
    orthogonalSwath2DPathFilter->GetOutput() );

  UCharImageType::Pointer inputPathImage =
    fourierSeriesPathToImageFilter->GetOutput();
  UCharImageType::Pointer outputImage =
    orthogonallyCorrected2DParametricPathToImageFilter->GetOutput();

  // Setup the swath merit output image
  RescaleIntensityImageFilterType::Pointer rescaleIntensityImageFilter =
    RescaleIntensityImageFilterType::New();
  rescaleIntensityImageFilter->SetInput( meritFilter->GetOutput() );
  rescaleIntensityImageFilter->SetOutputMinimum( 0 );
  rescaleIntensityImageFilter->SetOutputMaximum( 255 );
  UCharImageType::Pointer swathMeritImage =
    rescaleIntensityImageFilter->GetOutput();


  // Update the pipeline

  TRY_EXPECT_NO_EXCEPTION( outputImage->Update() );

/*
  // Save the output images
  itk::ImageFileWriter<ImageType>::Pointer writer
    = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput( output1Filter->GetOutput() );
  writer->SetFileName( "OrthSwathFilterPathIn.png" );
  writer->Write();
  writer->SetInput( output2Filter->GetOutput() );
  writer->SetFileName( "OrthSwathFilterPathOut.png" );
  writer->Write();
  writer->SetInput( output3Filter->GetOutput() );
  writer->SetFileName( "OrthSwathFilterSwathMerit.png" );
  writer->Write();
*/

  std::cerr << "Test finished" << std::endl;

  return EXIT_SUCCESS;
}
