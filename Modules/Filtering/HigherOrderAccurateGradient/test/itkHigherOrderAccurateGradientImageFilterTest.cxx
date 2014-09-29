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

  const unsigned int                       Dimension = 2;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;

  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer                     reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // First order accurate.
  typedef itk::GradientImageFilter<ImageType, float, float> FirstFilterType;
  FirstFilterType::Pointer                                  firstFilter = FirstFilterType::New();
  firstFilter->SetInput(reader->GetOutput());

  typedef itk::HigherOrderAccurateGradientImageFilter<ImageType, float, float> FilterType;
  typedef FilterType::OutputImageType                                          GradientImageType;
  FilterType::Pointer                                                          filter = FilterType::New();
  filter->SetInput(reader->GetOutput());

  std::string outputPrefix = argv[2];

  typedef itk::VectorMagnitudeImageFilter<GradientImageType, ImageType> GradientMagnitudeFilterType;
  GradientMagnitudeFilterType::Pointer gradientMagnitude = GradientMagnitudeFilterType::New();

  typedef itk::ImageFileWriter<ImageType> GradientMagnitudeWriterType;
  GradientMagnitudeWriterType::Pointer    gradientMagnitudeWriter = GradientMagnitudeWriterType::New();
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
