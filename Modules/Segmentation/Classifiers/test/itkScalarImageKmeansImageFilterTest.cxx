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
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputScalarImage outputLabeledImage numberOfClasses mean1 mean2... meanN " << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = unsigned char;
  constexpr unsigned int Dimension = 2;

  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(argv[1]);

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Problem encoutered while reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }


  using KMeansFilterType = itk::ScalarImageKmeansImageFilter<ImageType>;

  KMeansFilterType::Pointer kmeansFilter = KMeansFilterType::New();

  kmeansFilter->SetInput(reader->GetOutput());

  kmeansFilter->SetUseNonContiguousLabels(argv[3]);

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

  // before we add any mean check that an expection is thrown
  ITK_TRY_EXPECT_EXCEPTION(kmeansFilter->Update());


  for (unsigned k = 0; k < numberOfInitialClasses; k++)
  {
    kmeansFilter->AddClassWithInitialMean(std::stod(argv[k + numberOfArgumentsBeforeMeans]));
  }


  try
  {
    kmeansFilter->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Problem encoutered while classifying the image " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  KMeansFilterType::ParametersType estimatedMeans = kmeansFilter->GetFinalMeans();

  const unsigned int numberOfClasses = estimatedMeans.Size();

  for (unsigned int i = 0; i < numberOfClasses; ++i)
  {
    std::cout << "cluster[" << i << "] ";
    std::cout << "    estimated mean : " << estimatedMeans[i] << std::endl;
  }

  using OutputImageType = KMeansFilterType::OutputImageType;

  using RelabelFilterType = itk::RelabelComponentImageFilter<OutputImageType, OutputImageType>;


  RelabelFilterType::Pointer relabeler = RelabelFilterType::New();

  relabeler->SetInput(kmeansFilter->GetOutput());

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(relabeler->GetOutput());
  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Problem encoutered while writing image file : " << argv[2] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }


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

  return EXIT_SUCCESS;
}
