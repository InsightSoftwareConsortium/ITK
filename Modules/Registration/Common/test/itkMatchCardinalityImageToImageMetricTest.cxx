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

#include "itkMatchCardinalityImageToImageMetric.h"
#include "itkTranslationTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkImageFileReader.h"

#include <iostream>

int itkMatchCardinalityImageToImageMetricTest(int argc, char* argv[] )
{

  if (argc < 2)
    {
    std::cout << "Usage: " << argv[0] << " InputFile" << std::endl;
    exit (1);
    }

  typedef itk::Image<unsigned char,2>          ImageType;
  typedef itk::TranslationTransform<double, 2> TransformType;
  typedef itk::MatchCardinalityImageToImageMetric<ImageType,ImageType>
                                               MetricType;
  typedef itk::ImageFileReader<ImageType>      ReaderType;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,double>
                                               InterpolatorType;

  ReaderType::Pointer reader = ReaderType::New();
  MetricType::Pointer metric = MetricType::New();
  TransformType::Pointer transform = TransformType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  MetricType::ParametersType offset(2);

  reader->SetFileName (argv[1]);
  reader->Update();

  metric->SetMovingImage (reader->GetOutput());
  metric->SetFixedImage (reader->GetOutput());
  metric->SetInterpolator (interpolator);
  metric->SetTransform (transform);
  metric->SetFixedImageRegion (reader->GetOutput()->GetLargestPossibleRegion());
  metric->Initialize();

  std::cout << "First measure matches..." << std::endl;
  for (float x = -200.0; x <= 200.0; x+= 50.0)
    {
    offset[0] = x;
    for (float y = 0.0; y <= 0.0; y+= 10.0)
      {
      offset[1] = y;
      try
        {
        std::cout << "Offset: " << offset << " = " << metric->GetValue(offset) << std::endl;
        }
      catch( itk::ExceptionObject & excp )
        {
        std::cerr << "Exception thrown while computing metric " << std::endl;
        std::cerr << excp << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  std::cout << "Now measure mismatches..." << std::endl;
  metric->MeasureMatchesOff();

  for (float x = -200.0; x <= 200.0; x+= 50.0)
    {
    offset[0] = x;
    for (float y = 0.0; y <= 0.0; y+= 10.0)
      {
      offset[1] = y;
      try
        {
        std::cout << "Offset: " << offset << " = " << metric->GetValue(offset) << std::endl;
        }
      catch( itk::ExceptionObject & excp )
        {
        std::cerr << "Exception thrown while computing metric " << std::endl;
        std::cerr << excp << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  return EXIT_SUCCESS;

}
