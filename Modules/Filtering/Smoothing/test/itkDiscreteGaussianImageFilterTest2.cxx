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

#include "itkDiscreteGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

/** Execute the filter on user-provided input */

template <typename TIMAGE>
int
itkDiscreteGaussianImageFilterTestA(const char *       inputFilename,
                                    const char *       outputFilename,
                                    const float        sigma,
                                    const float        kernelError,
                                    const unsigned int kernelWidth,
                                    const unsigned int filterDimensionality)
{
  using ImageType = TIMAGE;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(inputFilename);

  using FilterType = itk::DiscreteGaussianImageFilter<ImageType, ImageType>;

  auto filter = FilterType::New();
  filter->SetInput(reader->GetOutput());
  filter->SetSigma(sigma);
  filter->SetMaximumError(kernelError);
  filter->SetMaximumKernelWidth(kernelWidth);
  filter->SetFilterDimensionality(filterDimensionality);
  filter->Update();

  using WriterType = itk::ImageFileWriter<ImageType>;

  auto writer = WriterType::New();
  writer->SetFileName(outputFilename);
  writer->SetInput(filter->GetOutput());
  writer->UseCompressionOn();
  writer->Update();

  return EXIT_SUCCESS;
}

int
itkDiscreteGaussianImageFilterTest2(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " imageDimension vectorDimension inputFilename outputFilename"
              << " [sigma] [kernelError] [kernelWidth] [filterDimensionality]" << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int img_dim = std::stoi(argv[1]);
  if (img_dim < 2 || img_dim > 3)
  {
    std::cerr << "This test only supports 2D or 3D images for demo! exiting ..." << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int vec_dim = std::stoi(argv[2]);
  if (vec_dim != 1 && vec_dim != 3)
  {
    std::cerr << "This test only supports 3-channel image or 1-channel image for demo! Exiting ... " << std::endl;
    return EXIT_FAILURE;
  }

  float        sigma = (argc > 5) ? std::stof(argv[5]) : 0.0;
  float        kernelError = (argc > 6) ? std::stof(argv[6]) : 0.01;
  unsigned int kernelWidth = (argc > 7) ? static_cast<unsigned int>(std::stoi(argv[7])) : 32;
  unsigned int filterDimensionality = (argc > 8) ? static_cast<unsigned int>(std::stoi(argv[8])) : img_dim;

  using ScalarPixelType = float;
  using VectorPixelType = itk::Vector<ScalarPixelType, 3>;

  if (img_dim == 2 && vec_dim == 1)
  {
    itkDiscreteGaussianImageFilterTestA<itk::Image<ScalarPixelType, 2>>(
      argv[3], argv[4], sigma, kernelError, kernelWidth, filterDimensionality);
  }
  else if (img_dim == 2 && vec_dim == 3)
  {
    itkDiscreteGaussianImageFilterTestA<itk::Image<VectorPixelType, 2>>(
      argv[3], argv[4], sigma, kernelError, kernelWidth, filterDimensionality);
  }
  else if (img_dim == 3 && vec_dim == 1)
  {
    itkDiscreteGaussianImageFilterTestA<itk::Image<ScalarPixelType, 3>>(
      argv[3], argv[4], sigma, kernelError, kernelWidth, filterDimensionality);
  }
  else if (img_dim == 3 && vec_dim == 1)
  {
    itkDiscreteGaussianImageFilterTestA<itk::Image<VectorPixelType, 3>>(
      argv[3], argv[4], sigma, kernelError, kernelWidth, filterDimensionality);
  }

  return EXIT_SUCCESS;
}
