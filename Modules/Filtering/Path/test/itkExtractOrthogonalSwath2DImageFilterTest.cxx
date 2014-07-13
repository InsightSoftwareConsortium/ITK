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

int itkExtractOrthogonalSwath2DImageFilterTest(int argc, char* argv[])
{
  if( argc != 2)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " outputImage " << std::endl;
    return -1;
    }

  typedef itk::Image<unsigned char, 2>         ImageType;
  typedef itk::PolyLineParametricPath<2>       InPathType;
  typedef itk::ChainCodePath<2>                ChainPathType;
  typedef itk::FourierSeriesPath<2>            FSPathType;

  typedef InPathType::VertexType               VertexType;
  typedef ImageType::IndexType                 IndexType;

  typedef itk::PathToChainCodePathFilter<InPathType,ChainPathType>
    Filter1Type;
  typedef itk::ChainCodeToFourierSeriesPathFilter<ChainPathType,FSPathType>
    Filter2Type;
  typedef itk::ExtractOrthogonalSwath2DImageFilter<ImageType>
    Filter3Type;

  InPathType::Pointer             inPath;
  ChainPathType::Pointer          chainPath;
  FSPathType::Pointer             outPath;

  // Setup the image
  std::cout << "Making a 64x64 white square centered in a 128x128 black image"<<std::endl;
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
      it.Set(255);
    else
      it.Set(0);
    ++it;
    }

  // Setup the path
  std::cout << "Making a square Path with v0 at (24,24) -> (24,104) -> (104,104) -> (104,24)" << std::endl;
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

  // Setup the first filter
  Filter1Type::Pointer filter1 = Filter1Type::New();
  filter1->SetInput(inPath);
  chainPath=filter1->GetOutput();

  // Setup the second filter
  Filter2Type::Pointer filter2 = Filter2Type::New();
  filter2->SetInput(filter1->GetOutput());
  filter2->SetNumberOfHarmonics(7); // make a nice, round, path for the swath
  outPath=filter2->GetOutput();

  // Setup the third filter; THIS IS THE MAIN FILTER TO BE TESTED
  Filter3Type::Pointer filter3 = Filter3Type::New();
  filter3->SetImageInput(inImage);
  filter3->SetPathInput(filter2->GetOutput());
  // Set the desired size of the filter's output
  size[0]=512;
  size[1]=21*2+1;
  filter3->SetSize(size);

  // Setup the output
  ImageType::Pointer outImage;
  outImage=filter3->GetOutput();

  // Testing spacing
  std::cout << "Testing Spacing: ";

  float spacing_float[2];
  double spacing_double[2];

  for(unsigned int i=0;i<2;i++)
  {
    spacing_float[i]=1.0;
    spacing_double[i]=1.0;
  }
  filter3->SetSpacing(spacing_float);
  filter3->SetSpacing(spacing_double);
  const double* spacing_result = filter3->GetSpacing();

  for(unsigned int i=0;i<2;i++)
  {
    if(spacing_result[i]!=1.0)
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Testing Origin
  std::cout << "Testing Origin: ";

  float origin_float[2];
  double origin_double[2];

  for(unsigned int i=0;i<2;i++)
  {
    origin_float[i]=0.0;
    origin_double[i]=0.0;
  }
  filter3->SetOrigin(origin_float);
  filter3->SetOrigin(origin_double);
  const double* origin_result = filter3->GetOrigin();

  for(unsigned int i=0;i<2;i++)
  {
    if(origin_result[i]!=0.0)
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

  // Testing PrintSelf
  std::cerr << filter3 << std::endl;

  // Update the pipeline
  std::cerr << "Running the Pipeline: ";
  outImage->Update();
  std::cerr << "[DONE]" << std::endl;

  // Testing pipeline execution
  std::cerr << "Testing Output Image: ";

  // Test only pixels definitely inside or outside the original white square
  ImageType::IndexType index;

  for(unsigned int col=0;col<size[0];col++)
  {
    index[0] = col;
    index[1] = 1;
    if(outImage->GetPixel(index) != 255)
    {
      std::cout << "index "<<index<<" = "<<int(outImage->GetPixel(index))<<": [FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }

    index[0] = col;
    index[1] = size[1]-2;
    if(outImage->GetPixel(index) != 0)
    {
      std::cout << "index "<<index<<" = "<<int(outImage->GetPixel(index))<<": [FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }
  std::cout << "[PASSED]" << std::endl;

  itk::ImageFileWriter<ImageType>::Pointer writer
    = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput( filter3->GetOutput() );
  writer->SetFileName( argv[1] );
  writer->Write();

  return EXIT_SUCCESS;
}
