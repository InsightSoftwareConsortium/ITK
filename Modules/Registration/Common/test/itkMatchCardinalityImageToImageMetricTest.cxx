/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkTestingMacros.h"

int
itkMatchCardinalityImageToImageMetricTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cout << " InputFile" << std::endl;
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<unsigned char, 2>;
  using TransformType = itk::TranslationTransform<double, 2>;
  using MetricType = itk::MatchCardinalityImageToImageMetric<ImageType, ImageType>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using InterpolatorType = itk::NearestNeighborInterpolateImageFunction<ImageType, double>;

  auto reader = ReaderType::New();
  auto metric = MetricType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(metric, MatchCardinalityImageToImageMetric, ImageToImageMetric);


  auto transform = TransformType::New();
  auto interpolator = InterpolatorType::New();

  MetricType::ParametersType offset(2);

  reader->SetFileName(argv[1]);
  reader->Update();

  metric->SetMovingImage(reader->GetOutput());
  metric->SetFixedImage(reader->GetOutput());
  metric->SetInterpolator(interpolator);
  metric->SetTransform(transform);
  metric->SetFixedImageRegion(reader->GetOutput()->GetLargestPossibleRegion());
  metric->Initialize();

  std::cout << "First measure matches..." << std::endl;
  for (float x = -200.0; x <= 200.0; x += 50.0)
  {
    offset[0] = x;
    for (float y = 0.0; y <= 0.0; y += 10.0)
    {
      offset[1] = y;
      try
      {
        std::cout << "Offset: " << offset << " = " << metric->GetValue(offset) << std::endl;
      }
      catch (const itk::ExceptionObject & excp)
      {
        std::cerr << "Exception thrown while computing metric " << std::endl;
        std::cerr << excp << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  std::cout << "Now measure mismatches..." << std::endl;
  bool measureMatches = false;
  ITK_TEST_SET_GET_BOOLEAN(metric, MeasureMatches, measureMatches);

  for (float x = -200.0; x <= 200.0; x += 50.0)
  {
    offset[0] = x;
    for (float y = 0.0; y <= 0.0; y += 10.0)
    {
      offset[1] = y;
      try
      {
        std::cout << "Offset: " << offset << " = " << metric->GetValue(offset) << std::endl;
      }
      catch (const itk::ExceptionObject & excp)
      {
        std::cerr << "Exception thrown while computing metric " << std::endl;
        std::cerr << excp << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  return EXIT_SUCCESS;
}
