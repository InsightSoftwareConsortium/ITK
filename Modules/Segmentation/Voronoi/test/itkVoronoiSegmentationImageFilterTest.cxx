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

#include "itkVoronoiSegmentationImageFilter.h"
#include "itkVoronoiSegmentationImageFilterBase.h"
#include "itkTestingMacros.h"

int itkVoronoiSegmentationImageFilterTest( int, char* [] )
{
  const int width = 256;
  const int height = 256;

  typedef itk::Image<unsigned short, 2> UShortImage;
  typedef itk::Image<unsigned char, 2>  PriorImage;
  typedef itk::VoronoiSegmentationImageFilter<UShortImage, UShortImage, PriorImage>
    VoronoiSegmentationImageFilterType;

  VoronoiSegmentationImageFilterType::Pointer voronoiSegmenter =
    VoronoiSegmentationImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( voronoiSegmenter, VoronoiSegmentationImageFilter,
    VoronoiSegmentationImageFilterBase );

  UShortImage::Pointer inputImage = UShortImage::New();
  UShortImage::SizeType size = {{width, height}};
  UShortImage::IndexType index;
  index.Fill(0);

  UShortImage::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);

  std::cout << "Allocating image" << std::endl;
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();


  itk::ImageRegionIteratorWithIndex <UShortImage> it(inputImage, region);

  // Background: random field with mean: 500, std: 50
  std::cout << "Setting background random pattern image" << std::endl;
  while( !it.IsAtEnd()) {
    it.Set((unsigned short)(vnl_sample_uniform(450, 550)) );
    ++it;
  }

  // Object (2): random field with mean: 520, std: 20
  std::cout << "Defining object #2" << std::endl;
  unsigned int i;
  unsigned int j;
  for(i = 30; i < 94; i++){
    index[0] = i;
    for (j = 30; j< 94; j++){
      index[1] = j;
      inputImage->SetPixel(index, (unsigned short)(vnl_sample_uniform(500, 540)) );
    }
  }

  for(i = 150; i < 214; i++){
    index[0] = i;
    for (j = 150; j< 214; j++){
      index[1] = j;
      inputImage->SetPixel(index, (unsigned short)(vnl_sample_uniform(500, 540)) );
    }
  }

  int k;
  unsigned short TestImg[65536];

  voronoiSegmenter->SetInput(inputImage);

  double mean = 520;
  double std = 20;
  double meanTolerance = 10;
  double stdTolerance = 20;
  int numberOfSeeds = 400;
  int steps = 5;

  voronoiSegmenter->SetMean( mean );
  voronoiSegmenter->SetSTD( std );
  voronoiSegmenter->SetMeanTolerance( meanTolerance );
  voronoiSegmenter->SetSTDTolerance( stdTolerance );
  voronoiSegmenter->SetNumberOfSeeds( numberOfSeeds );
  voronoiSegmenter->SetSteps( steps );

  TEST_SET_GET_VALUE( mean, voronoiSegmenter->GetMean() );
  TEST_SET_GET_VALUE( std, voronoiSegmenter->GetSTD() );
  TEST_SET_GET_VALUE( meanTolerance, voronoiSegmenter->GetMeanTolerance() );
  TEST_SET_GET_VALUE( stdTolerance, voronoiSegmenter->GetSTDTolerance() );
  TEST_SET_GET_VALUE( numberOfSeeds, voronoiSegmenter->GetNumberOfSeeds() );
  TEST_SET_GET_VALUE( steps, voronoiSegmenter->GetSteps() );

  std::cout << "Running algorithm" << std::endl;
  voronoiSegmenter->Update();

  std::cout << "Walking output" << std::endl;
  itk::ImageRegionIteratorWithIndex <UShortImage> ot(voronoiSegmenter->GetOutput(), region);

  k = 0;
  while( !ot.IsAtEnd() )
    {
    TestImg[k] = ot.Get();
    (void)(TestImg[k]); // prevents "set but not used" warning
    k++;
    ++ot;
    }

  /* Test OK on local machine.
  FILE *imgfile = fopen("output.raw","wb");
  fwrite(TestImg,2,65536,imgfile);
  fclose(imgfile);

  imgfile = fopen("input.raw","wb");
  it.GoToBegin();
  k=0;
  while( !it.IsAtEnd()){
    TestImg[k]=it.Get();
    k++;
    ++it;
  }
  fwrite(TestImg,2,65536,imgfile);
  fclose(imgfile);*/

  std::cout << "Test Succeeded!" << std::endl;
  return EXIT_SUCCESS;
}
