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
#include "itkVectorMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkBSplineApproximationGradientImageFilter.h"
#include "itkImageToImageOfVectorsFilter.h"

int
itkBSplineApproximationGradientImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage outputImage ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int Dimension = 2;
  using PixelType = float;
  using InputImageType = itk::Image<PixelType, Dimension>;
  using OutputImageType = InputImageType;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using ImageToVectorImageFilterType = itk::ImageToImageOfVectorsFilter<InputImageType, 1>;
  ImageToVectorImageFilterType::Pointer imageToVI = ImageToVectorImageFilterType::New();
  imageToVI->SetInput(0, reader->GetOutput());
  using VectorImageType = ImageToVectorImageFilterType::OutputImageType;
  try
  {
    reader->UpdateOutputInformation();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  using GradientImageFilterType = itk::BSplineApproximationGradientImageFilter<VectorImageType, float>;
  GradientImageFilterType::Pointer gradientImageFilter = GradientImageFilterType::New();
  gradientImageFilter->SetInput(imageToVI->GetOutput());
  gradientImageFilter->SetNumberOfLevels(1);
  gradientImageFilter->SetControlPointSpacingRatio(2.0);

  using GradientMagnitudeFilterType =
    itk::VectorMagnitudeImageFilter<GradientImageFilterType::OutputImageType, OutputImageType>;
  GradientMagnitudeFilterType::Pointer gradientMagnitude = GradientMagnitudeFilterType::New();
  gradientMagnitude->SetInput(gradientImageFilter->GetOutput());

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(gradientMagnitude->GetOutput());
  writer->SetFileName(std::string(argv[2]) + "GradientMagnitudeOutput.mha");

  using FullWriterType = itk::ImageFileWriter<GradientImageFilterType::OutputImageType>;
  FullWriterType::Pointer fullWriter = FullWriterType::New();
  fullWriter->SetInput(gradientImageFilter->GetOutput());
  fullWriter->SetFileName(std::string(argv[2]) + "GradientOutput.mha");

  try
  {
    writer->Update();
    fullWriter->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
