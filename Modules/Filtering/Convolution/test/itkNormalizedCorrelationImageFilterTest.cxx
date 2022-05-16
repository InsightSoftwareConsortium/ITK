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

#include <fstream>
#include "itkNormalizedCorrelationImageFilter.h"
#include "itkAnnulusOperator.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkResampleImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkNormalizedCorrelationImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage MaskImage OutputImage\n";
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned char;
  using CorrelationPixelType = float;

  using InputImageType = itk::Image<PixelType, Dimension>;
  using CorrelationImageType = itk::Image<CorrelationPixelType, Dimension>;

  itk::ImageFileReader<InputImageType>::Pointer input = itk::ImageFileReader<InputImageType>::New();
  input->SetFileName(argv[1]);
  input->Update();

  // define an operator
  using AnnulusType = itk::AnnulusOperator<CorrelationPixelType, Dimension>;
  AnnulusType annulus;
  annulus.SetInnerRadius(5);
  annulus.SetThickness(2);
  annulus.NormalizeOn();
  annulus.BrightCenterOn();
  annulus.CreateOperator();

  // create a mask
  itk::ImageFileReader<InputImageType>::Pointer mask = itk::ImageFileReader<InputImageType>::New();
  mask->SetFileName(argv[2]);

  // resample the mask to be the size of the input
  using InterpolatorType = itk::NearestNeighborInterpolateImageFunction<InputImageType>;
  auto interpolator = InterpolatorType::New();
  if (interpolator.IsNull())
  {
    return EXIT_FAILURE;
  }

  using ResampleType = itk::ResampleImageFilter<InputImageType, InputImageType>;
  auto resample = ResampleType::New();
  resample->SetInput(mask->GetOutput());
  resample->SetOutputParametersFromImage(input->GetOutput());


  // Create a filter
  using FilterType = itk::NormalizedCorrelationImageFilter<InputImageType, InputImageType, CorrelationImageType>;

  auto                     filter = FilterType::New();
  itk::SimpleFilterWatcher watcher(filter, "Normalized correlation");

  filter->SetInput(input->GetOutput());
  filter->SetTemplate(annulus);
  filter->SetMaskImage(resample->GetOutput());


  using ThresholdType = itk::BinaryThresholdImageFilter<CorrelationImageType, InputImageType>;
  auto threshold = ThresholdType::New();
  threshold->SetInput(filter->GetOutput());
  threshold->SetLowerThreshold(0.1);
  threshold->SetUpperThreshold(1.0);
  threshold->SetInsideValue(255);
  threshold->SetOutsideValue(0);

  // Generate test image
  itk::ImageFileWriter<InputImageType>::Pointer writer;
  writer = itk::ImageFileWriter<InputImageType>::New();
  writer->SetInput(threshold->GetOutput());
  writer->SetFileName(argv[3]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
