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


int itkOrthogonalSwath2DPathFilterTest(int, char*[])
{
  typedef itk::Image<unsigned char, 2>                ImageType;
  typedef itk::Image<double, 2>                       FloatImageType;
  typedef itk::PolyLineParametricPath<2>              InPathType;
  typedef itk::ChainCodePath<2>                       ChainPathType;
  typedef itk::FourierSeriesPath<2>                   FSPathType;

  typedef InPathType::VertexType                      VertexType;

  typedef ImageType::IndexType                        IndexType;

  typedef itk::OrthogonallyCorrected2DParametricPath  OutputPathType;

  // pre-process the image
  typedef itk::RescaleIntensityImageFilter<ImageType,FloatImageType>      CastFilterType;
  typedef itk::DiscreteGaussianImageFilter<FloatImageType,FloatImageType> SmoothFilterType;
  typedef itk::ExtractOrthogonalSwath2DImageFilter<FloatImageType>        SwathFilterType;
  typedef itk::DerivativeImageFilter<FloatImageType,FloatImageType>       MeritFilterType;

  // pre-process the path
  typedef itk::PathToChainCodePathFilter<InPathType,ChainPathType>          PathFilter1Type;
  typedef itk::ChainCodeToFourierSeriesPathFilter<ChainPathType,FSPathType> PathFilter2Type;

  // test the filter
  typedef itk::OrthogonalSwath2DPathFilter<FSPathType,FloatImageType>       TestFilterType;

  // save the results
  typedef itk::PathToImageFilter< FSPathType,     ImageType >         Output1FilterType;
  typedef itk::PathToImageFilter< OutputPathType, ImageType >         Output2FilterType;
  typedef itk::RescaleIntensityImageFilter<FloatImageType,ImageType>  Output3FilterType;

  // Setup the path
  std::cout << "Making a square Path with v0 at (24,24) -> (24,104) -> (104,104) -> (104,24)" << std::endl;
  InPathType::Pointer inPath;
  VertexType        v;
  inPath = InPathType::New();
  v.Fill(24);
  inPath->AddVertex(v);
  v[0]=24;
  v[1]=104;
  inPath->AddVertex(v);
  v.Fill(104);
  inPath->AddVertex(v);
  v[0]=104;
  v[1]=24;
  inPath->AddVertex(v);
  v.Fill(24);
  inPath->AddVertex(v);

  // Setup the first path filter
  PathFilter1Type::Pointer pathFilter1 = PathFilter1Type::New();
  pathFilter1->SetInput(inPath);

  // Setup the second path filter
  PathFilter2Type::Pointer pathFilter2 = PathFilter2Type::New();
  pathFilter2->SetInput(pathFilter1->GetOutput());
  pathFilter2->SetNumberOfHarmonics(7); // make a nice, round, path for the swath

  // Setup the image
  std::cout << "Making a 64x64 black square centered in a 128x128 white image"<<std::endl;
  ImageType::Pointer  inImage = ImageType::New();
  IndexType start;
  start[0]=0;
  start[1]=0;
  ImageType::SizeType size;
  size[0]=128;
  size[1]=128;
  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(start);
  inImage->SetRegions(region);
  double spacing[ ImageType::ImageDimension ];
  spacing[0]=1.0;
  spacing[1]=1.0;
  inImage->SetSpacing(spacing);
  inImage->Allocate();
  typedef itk::ImageRegionIterator<ImageType> ImageItType;
  ImageItType it( inImage, inImage->GetRequestedRegion() );
  it.GoToBegin();
  IndexType pixelIndex;
  while( !it.IsAtEnd() )
    {
    pixelIndex = it.GetIndex();
    if( pixelIndex[0] >= int(size[0]/4) && pixelIndex[0] < int(size[0]*3/4) &&
        pixelIndex[1] >= int(size[1]/4) && pixelIndex[1] < int(size[1]*3/4) )
      it.Set(0);
    else
      it.Set(255);
    ++it;
    }

  // Cast the input image into a double image
  CastFilterType::Pointer castFilter = CastFilterType::New();
  castFilter->SetInput( inImage );
  castFilter->SetOutputMinimum(0);
  castFilter->SetOutputMaximum(1.0);

  // Smooth the (double pixel type) input image
  SmoothFilterType::Pointer smoothFilter = SmoothFilterType::New();
  smoothFilter->SetInput( castFilter->GetOutput() );
  double gaussianVariance = 1.0;
  // We want a fast 3x3 kernel.  Gausian Operator will not truncate its kernel
  // width to any less than a 5x5 kernel (kernel width of 3 for 1 center pixel +
  // 2 edge pixels).  However, GausianOperator always uses at least a 3x3
  // kernel, and so setting the maximum error to 1.0 (no limit) will make it
  // stop growing the kernel at the desired 3x3 size.
  double maxError = 0.9;
  smoothFilter->SetUseImageSpacingOff();
  smoothFilter->SetVariance( gaussianVariance );
  smoothFilter->SetMaximumError( maxError );

  // Extract the swath image
  SwathFilterType::Pointer swathFilter = SwathFilterType::New();
  swathFilter->SetImageInput( smoothFilter->GetOutput() );
//  swathFilter->SetImageInput( castFilter->GetOutput() );
  swathFilter->SetPathInput( pathFilter2->GetOutput() );
  size[0]=512;
  size[1]=16*2+1; // the top 1 and bottom 1 rows are dropped when smoothing
  swathFilter->SetSize(size);

  // Find the vertical gradient of the swath image
  MeritFilterType::Pointer meritFilter = MeritFilterType::New();
  meritFilter->SetInput( swathFilter->GetOutput() );
  meritFilter->SetOrder( 1 ); // first partial derivative
  meritFilter->SetDirection( 1 ); // d/dy

  // Setup the test filter
  std::cerr << "Creating the test filter" << std::endl;
  TestFilterType::Pointer testFilter = TestFilterType::New();
  std::cerr << "Setting up the test filter" << std::endl;
  testFilter->SetPathInput(pathFilter2->GetOutput());
  testFilter->SetImageInput(meritFilter->GetOutput());
  OutputPathType::Pointer outPath = testFilter->GetOutput();

  // Setup the input & output path images
  std::cerr << "Setting up the output path image" << std::endl;
  Output1FilterType::Pointer output1Filter = Output1FilterType::New();
  Output2FilterType::Pointer output2Filter = Output2FilterType::New();
  size[0]=128;
  size[1]=128;
  output1Filter->SetSize(size);  // same size as the input image
  output2Filter->SetSize(size);  // same size as the input image
  output1Filter->SetPathValue(255);
  output2Filter->SetPathValue(255);
  output1Filter->SetInput( testFilter->GetPathInput() );
  output2Filter->SetInput( testFilter->GetOutput() );
  ImageType::Pointer inPathImage = output1Filter->GetOutput();
  ImageType::Pointer outImage =    output2Filter->GetOutput();

  // Setup the swath merit output image
  Output3FilterType::Pointer output3Filter = Output3FilterType::New();
  output3Filter->SetInput( meritFilter->GetOutput() );
  output3Filter->SetOutputMinimum(0);
  output3Filter->SetOutputMaximum(255);
  ImageType::Pointer swathMeritImage = output3Filter->GetOutput();


  // Testing PrintSelf
  std::cerr << testFilter << std::endl;

  // Update the pipeline
  std::cerr << "Running the Pipeline: ";
  outImage->Update();
  std::cerr << "[DONE]" << std::endl;
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
  return EXIT_SUCCESS;
}
