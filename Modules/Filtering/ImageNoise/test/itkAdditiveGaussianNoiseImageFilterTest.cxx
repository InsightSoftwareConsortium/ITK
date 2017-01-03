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
#include "itkSimpleFilterWatcher.h"

#include "itkAdditiveGaussianNoiseImageFilter.h"
#include "itkTestingMacros.h"

int itkAdditiveGaussianNoiseImageFilterTest(int argc, char * argv[])
{

  if( argc < 3 )
    {
    std::cerr << "usage: " << argv[0] << " input output [std_dev [mean]]" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char                       PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::AdditiveGaussianNoiseImageFilter< ImageType, ImageType >
    AdditiveGaussianNoiseFilterType;

  AdditiveGaussianNoiseFilterType::Pointer additiveGaussianNoiseFilter =
    AdditiveGaussianNoiseFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( additiveGaussianNoiseFilter, AdditiveGaussianNoiseImageFilter,
    NoiseBaseImageFilter );

  double stdDev = 1.0;
  if( argc >= 4 )
    {
    stdDev = atof( argv[3] );
    }
  additiveGaussianNoiseFilter->SetStandardDeviation( stdDev );
  TEST_SET_GET_VALUE( stdDev, additiveGaussianNoiseFilter->GetStandardDeviation() );

  double mean = 0.0;
  if( argc >= 5 )
    {
    mean = atof( argv[4] );
    }
  additiveGaussianNoiseFilter->SetMean( mean );
  TEST_SET_GET_VALUE( mean, additiveGaussianNoiseFilter->GetMean() );

  additiveGaussianNoiseFilter->SetInput( reader->GetOutput() );

  itk::SimpleFilterWatcher watcher( additiveGaussianNoiseFilter, "AdditiveGaussianNoiseImageFilter" );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( additiveGaussianNoiseFilter->GetOutput() );
  writer->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
