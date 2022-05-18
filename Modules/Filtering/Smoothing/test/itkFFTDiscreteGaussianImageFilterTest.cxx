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
#include <iostream>

#include "itkConstantBoundaryCondition.h"
#include "itkFFTDiscreteGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

template <typename ImageType>
int
itkFFTDiscreteGaussianImageFilterTestProcedure(int argc, char ** argv)
{
  float        sigma = (argc > 4) ? std::stof(argv[4]) : 0.0;
  float        kernelError = (argc > 5) ? std::stof(argv[5]) : 0.01;
  unsigned int kernelWidth = (argc > 6) ? std::stoi(argv[6]) : 32;
  unsigned int filterDimensionality = (argc > 7) ? std::stoi(argv[7]) : ImageType::ImageDimension;
  unsigned int kernelSource = (argc > 8) ? std::stoi(argv[8]) : 0;

  typename ImageType::Pointer inputImage = itk::ReadImage<ImageType>(argv[2]);

  using FilterType = itk::FFTDiscreteGaussianImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, FFTDiscreteGaussianImageFilter, DiscreteGaussianImageFilter);

  // Test setting inputs

  filter->SetInput(inputImage);

  filter->SetSigma(sigma);
  for (auto & param : filter->GetSigmaArray())
  {
    ITK_TEST_EXPECT_EQUAL(sigma, param);
  }
  for (auto & param : filter->GetVariance())
  {
    double tolerance = 1e-6;
    ITK_TEST_EXPECT_TRUE(std::fabs((sigma * sigma) - param) < tolerance);
  }

  filter->SetMaximumError(kernelError);
  for (size_t dim = 0; dim < ImageType::ImageDimension; ++dim)
  {
    ITK_TEST_SET_GET_VALUE(kernelError, filter->GetMaximumError()[dim]);
  }

  filter->SetMaximumKernelWidth(kernelWidth);
  ITK_TEST_SET_GET_VALUE(kernelWidth, filter->GetMaximumKernelWidth());

  filter->SetFilterDimensionality(filterDimensionality);
  ITK_TEST_SET_GET_VALUE(filterDimensionality, filter->GetFilterDimensionality());

  itk::ZeroFluxNeumannBoundaryCondition<ImageType> zfnBoundaryCondition;
  filter->SetRealBoundaryCondition(&zfnBoundaryCondition);
  ITK_TEST_SET_GET_VALUE(&zfnBoundaryCondition, filter->GetRealBoundaryCondition());

  itk::FFTDiscreteGaussianImageFilterEnums::KernelSource source;
  if (kernelSource == 0)
  {
    source = itk::FFTDiscreteGaussianImageFilterEnums::KernelSource::OPERATORS;
  }
  else
  {
    source = itk::FFTDiscreteGaussianImageFilterEnums::KernelSource::IMAGE_SOURCE;
  }
  filter->SetKernelSource(source);
  ITK_TEST_SET_GET_VALUE(filter->GetKernelSource(), source);

  // Test that attempting to set unused interface parameters throws warnings
  // Ignore values from "Get" methods as these are unused

  itk::ConstantBoundaryCondition<ImageType> constantBoundaryCondition;
  filter->SetInputBoundaryCondition(&constantBoundaryCondition);

  // Run convolution

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  itk::WriteImage(filter->GetOutput(), argv[3], true);

  // Test streaming enumeration for FFTDiscreteGaussianImageFilterEnums::KernelSource elements
  for (const auto val : { itk::FFTDiscreteGaussianImageFilterEnums::KernelSource::OPERATORS,
                          itk::FFTDiscreteGaussianImageFilterEnums::KernelSource::IMAGE_SOURCE })
  {
    std::cout << "STREAMED ENUM VALUE FFTDiscreteGaussianImageFilterEnums::KernelSource: " << val << std::endl;
  }

  return EXIT_SUCCESS;
}

int
itkFFTDiscreteGaussianImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr
      << itkNameOfTestExecutableMacro(argv)
      << " imageDimension inputFilename outputFilename sigma kernelError kernelWidth filterDimensionality kernelSource"
      << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int ImageDimension = static_cast<unsigned int>(std::stoi(argv[1]));

  if (ImageDimension == 2)
  {
    itkFFTDiscreteGaussianImageFilterTestProcedure<itk::Image<float, 2>>(argc, &argv[0]);
  }
  else if (ImageDimension == 3)
  {
    itkFFTDiscreteGaussianImageFilterTestProcedure<itk::Image<float, 3>>(argc, &argv[0]);
  }
  else
  {
    std::cout << "Did not recognize image dimension argument!" << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
