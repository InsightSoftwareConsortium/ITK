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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkScalarImageKmeansImageFilter.h"
#include "itkRelabelComponentImageFilter.h"
#include "itkTestingMacros.h"

int
itkScalarImageKmeansImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputScalarImage"
              << " outputLabeledImage"
              << " useNonContiguousLabels"
              << " numberOfClasses"
              << " mean1 mean2... meanN" << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = unsigned char;
  constexpr unsigned int Dimension = 2;

  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  using KMeansFilterType = itk::ScalarImageKmeansImageFilter<ImageType>;

  auto kmeansFilter = KMeansFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(kmeansFilter, ScalarImageKmeansImageFilter, ImageToImageFilter);

  kmeansFilter->SetInput(reader->GetOutput());

  auto useNonContiguousLabels = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(kmeansFilter, UseNonContiguousLabels, useNonContiguousLabels);

  typename KMeansFilterType::ImageRegionType            region;
  typename KMeansFilterType::ImageRegionType::IndexType index = { { 50, 50 } };
  typename KMeansFilterType::ImageRegionType::SizeType  size = { { 80, 100 } };
  region.SetIndex(index);
  region.SetSize(size);
  kmeansFilter->SetImageRegion(region);
  ITK_TEST_SET_GET_VALUE(region, kmeansFilter->GetImageRegion());

  const unsigned int numberOfInitialClasses = std::stoi(argv[4]);

  constexpr unsigned int numberOfArgumentsBeforeMeans = 5;
  if (static_cast<unsigned int>(argc) < numberOfInitialClasses + numberOfArgumentsBeforeMeans)
  {
    std::cerr << "Error: " << std::endl;
    std::cerr << numberOfInitialClasses << " classes has been specified ";
    std::cerr << "but no enough means have been provided in the command ";
    std::cerr << "line arguments " << std::endl;
    return EXIT_FAILURE;
  }

  // Before we add any mean check that an expection is thrown
  ITK_TRY_EXPECT_EXCEPTION(kmeansFilter->Update());


  for (unsigned int k = 0; k < numberOfInitialClasses; ++k)
  {
    kmeansFilter->AddClassWithInitialMean(std::stod(argv[k + numberOfArgumentsBeforeMeans]));
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(kmeansFilter->Update());


  KMeansFilterType::ParametersType estimatedMeans = kmeansFilter->GetFinalMeans();

  const unsigned int numberOfClasses = estimatedMeans.Size();

  for (unsigned int i = 0; i < numberOfClasses; ++i)
  {
    std::cout << "cluster[" << i << "] ";
    std::cout << "    estimated mean : " << estimatedMeans[i] << std::endl;
  }

  using OutputImageType = KMeansFilterType::OutputImageType;

  using RelabelFilterType = itk::RelabelComponentImageFilter<OutputImageType, OutputImageType>;


  auto relabeler = RelabelFilterType::New();

  relabeler->SetInput(kmeansFilter->GetOutput());

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();
  writer->SetInput(relabeler->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  using SizesType = RelabelFilterType::ObjectSizeInPixelsContainerType;

  const SizesType & sizes = relabeler->GetSizeOfObjectsInPixels();

  auto sizeItr = sizes.begin();
  auto sizeEnd = sizes.end();

  std::cout << "Number of pixels per class " << std::endl;
  unsigned int kclass = 0;
  while (sizeItr != sizeEnd)
  {
    std::cout << "Class " << kclass << " = " << *sizeItr << std::endl;
    ++kclass;
    ++sizeItr;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
