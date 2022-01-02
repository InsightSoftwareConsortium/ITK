/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include <iostream>

#include "itkDiscreteGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

template <typename TIMAGE>
int
itkDiscreteGaussianImageFilterTestA(const char * inputFilename, float sigma, const char * outputFilename)
{
  using ImageType = TIMAGE;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(inputFilename);

  using FilterType = itk::DiscreteGaussianImageFilter<ImageType, ImageType>;

  auto filter = FilterType::New();
  filter->SetInput(reader->GetOutput());
  filter->SetVariance(sigma);
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
  if (argc != 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv)
              << " imageDimension vectorDimension inputFilename sigma outputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int img_dim = std::stoi(argv[1]);
  unsigned int vec_dim = std::stoi(argv[2]);

  if (img_dim != 2)
  {
    std::cerr << "This test only supports 2D image for demo! exiting ..." << std::endl;
    return EXIT_FAILURE;
  }

  if (vec_dim != 1 && vec_dim != 3)
  {
    std::cerr << "This test only supports 3-channel image or 1-channel image for demo! Exiting ... " << std::endl;
    return EXIT_FAILURE;
  }

  using ScalarPixelType = float;
  using ScalarImageType = itk::Image<ScalarPixelType, 2>;
  using VectorPixelType = itk::Vector<ScalarPixelType, 3>;
  using VectorImageType = itk::Image<VectorPixelType, 2>;

  switch (vec_dim)
  {
    case 1:
      itkDiscreteGaussianImageFilterTestA<ScalarImageType>(argv[3], std::stod(argv[4]), argv[5]);
      break;
    case 3:
      itkDiscreteGaussianImageFilterTestA<VectorImageType>(argv[3], std::stod(argv[4]), argv[5]);
      break;
  }

  return EXIT_SUCCESS;
}
