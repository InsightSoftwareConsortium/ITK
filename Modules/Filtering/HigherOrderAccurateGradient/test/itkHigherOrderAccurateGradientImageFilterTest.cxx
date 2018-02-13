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
#include "itkGradientImageFilter.h"
#include "itkVectorMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkHigherOrderAccurateGradientImageFilter.h"

int
itkHigherOrderAccurateGradientImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // First order accurate.
  using FirstFilterType = itk::GradientImageFilter<ImageType, float, float>;
  FirstFilterType::Pointer firstFilter = FirstFilterType::New();
  firstFilter->SetInput(reader->GetOutput());

  using FilterType = itk::HigherOrderAccurateGradientImageFilter<ImageType, float, float>;
  using GradientImageType = FilterType::OutputImageType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());

  std::string outputPrefix = argv[2];

  using GradientMagnitudeFilterType = itk::VectorMagnitudeImageFilter<GradientImageType, ImageType>;
  GradientMagnitudeFilterType::Pointer gradientMagnitude = GradientMagnitudeFilterType::New();

  using GradientMagnitudeWriterType = itk::ImageFileWriter<ImageType>;
  GradientMagnitudeWriterType::Pointer gradientMagnitudeWriter = GradientMagnitudeWriterType::New();
  gradientMagnitudeWriter->SetInput(gradientMagnitude->GetOutput());

  std::ostringstream ostrm;
  try
  {
    gradientMagnitude->SetInput(firstFilter->GetOutput());
    ostrm.str("");
    ostrm << outputPrefix + "_GradientImageFilter_Magnitude.mha";
    gradientMagnitudeWriter->SetFileName(ostrm.str());
    gradientMagnitudeWriter->Update();
    gradientMagnitude->SetInput(filter->GetOutput());
    for (unsigned int accuracy = 1; accuracy < 6; ++accuracy)
    {
      filter->SetOrderOfAccuracy(accuracy);
      ostrm.str("");
      ostrm << outputPrefix << "_Accuracy" << accuracy << "_Magnitude.mha";
      gradientMagnitudeWriter->SetFileName(ostrm.str());
      gradientMagnitudeWriter->Update();
    }
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
