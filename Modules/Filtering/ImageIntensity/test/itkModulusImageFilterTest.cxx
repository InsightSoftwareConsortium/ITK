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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkModulusImageFilter.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int itkModulusImageFilterTest(int argc, char * argv[])
{
  if( argc < 3 )
  {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int Dimension = 2;

  typedef unsigned char                       PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  // get the distance map inside the spots
  // spot are already black so there is no need to invert the image
  typedef itk::DanielssonDistanceMapImageFilter< ImageType, ImageType > DistanceFilter;
  DistanceFilter::Pointer distance = DistanceFilter::New();
  distance->SetInput( reader->GetOutput() );

  typedef itk::ModulusImageFilter< ImageType, ImageType > FilterType;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, ModulusImageFilter,
    BinaryFunctorImageFilter );

  filter->SetInput( distance->GetOutput() );

  FilterType::InputPixelType dividend = 8;
  filter->SetDividend( dividend );
  TEST_SET_GET_VALUE( dividend, filter->GetDividend() )

  filter->InPlaceOn();
  filter->SetFunctor( filter->GetFunctor() );

  itk::SimpleFilterWatcher watcher(filter);

  typedef itk::RescaleIntensityImageFilter< ImageType, ImageType > ThresholdType;
  ThresholdType::Pointer rescale = ThresholdType::New();
  rescale->SetInput( filter->GetOutput() );
  rescale->SetOutputMaximum( itk::NumericTraits< PixelType >::max() );
  rescale->SetOutputMinimum( itk::NumericTraits< PixelType >::NonpositiveMin() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( rescale->GetOutput() );
  writer->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
