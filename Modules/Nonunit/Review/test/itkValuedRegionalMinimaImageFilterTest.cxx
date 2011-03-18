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
// a test routine for regional extrema using flooding
#include "itkValuedRegionalMinimaImageFilter.h"
#include "itkMaximumImageFilter.h"
#include "itkHConcaveImageFilter.h"
#include "itkInvertIntensityImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkAndImageFilter.h"
#include "itkSimpleFilterWatcher.h"

int itkValuedRegionalMinimaImageFilterTest(int argc, char * argv[])
{
  const int dim = 2;

  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage  OutputImageFile1 OutputImageFile2  "
              << "OutputImageFile3" << std::endl;
    return EXIT_FAILURE;
    }

  typedef unsigned char                 PixelType;
  typedef itk::Image< PixelType, dim >  ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );

  typedef itk::ValuedRegionalMinimaImageFilter< ImageType, ImageType >
                                                              FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetFullyConnected( atoi(argv[1]) );
  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->Update();

  // produce the same output with other filters
  typedef itk::HConcaveImageFilter< ImageType, ImageType > ConcaveType;
  ConcaveType::Pointer concave = ConcaveType::New();
  concave->SetInput( reader->GetOutput() );
  concave->SetFullyConnected( atoi(argv[1]) );
  concave->SetHeight( 1 );

  // concave gives minima with value=1 and others with value=0
  // rescale the image so we have minima=255 other=0
  typedef itk::RescaleIntensityImageFilter< ImageType, ImageType > RescaleType;
  RescaleType::Pointer rescale = RescaleType::New();
  rescale->SetInput( concave->GetOutput() );
  rescale->SetOutputMaximum( 255 );
  rescale->SetOutputMinimum( 0 );

  // in the input image, select the values of the pixel at the minima
  typedef itk::AndImageFilter< ImageType, ImageType, ImageType > AndType;
  AndType::Pointer a = AndType::New();
  a->SetInput(0, rescale->GetOutput() );
  a->SetInput(1, reader->GetOutput() );

  // all pixel which are not minima must have value=255.
  // get the non minima pixel by inverting the rescaled image
  // we will have minima value=0 and non minima value=255
  typedef itk::InvertIntensityImageFilter< ImageType, ImageType > InvertType;
  InvertType::Pointer invert = InvertType::New();
  invert->SetInput( rescale->GetOutput() );

  // get the highest value from "a" and from invert. The minima have
  // value>=0 in "a" image and the non minima have a value=0. In invert,
  // the non minima have a value=255 and the minima a value=0
  typedef itk::MaximumImageFilter< ImageType, ImageType, ImageType > MaxType;
  MaxType::Pointer max = MaxType::New();
  max->SetInput(0, invert->GetOutput() );
  max->SetInput(1, a->GetOutput() );

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( max->GetOutput() );
  writer2->SetFileName( argv[4] );
  writer2->Update();

  return EXIT_SUCCESS;
}
