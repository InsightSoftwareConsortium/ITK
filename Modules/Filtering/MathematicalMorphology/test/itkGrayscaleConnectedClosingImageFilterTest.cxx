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

//

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkGrayscaleConnectedClosingImageFilter.h"
#include "itkTestingMacros.h"


int
itkGrayscaleConnectedClosingImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << "  inputImageFile  ";
    std::cerr << " outputImageFile seedX seedY " << std::endl;
    return EXIT_FAILURE;
  }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned char;
  using OutputPixelType = unsigned char;
  using WritePixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using WriteImageType = itk::Image<WritePixelType, Dimension>;


  // readers/writers
  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<WriteImageType>;

  // define the connected closing filter
  using ConnectedClosingFilterType = itk::GrayscaleConnectedClosingImageFilter<InputImageType, OutputImageType>;


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  // Create the filter
  ConnectedClosingFilterType::Pointer connectedClosing = ConnectedClosingFilterType::New();
  itk::SimpleFilterWatcher            watcher(connectedClosing, "connectedClosing");

  // Setup the input and output files
  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Setup the connectedopening method
  connectedClosing->SetInput(reader->GetOutput());

  InputImageType::IndexType seed;
  seed[0] = std::stoi(argv[3]);
  seed[1] = std::stoi(argv[4]);
  connectedClosing->SetSeed(seed);

  // Run the filter
  writer->SetInput(connectedClosing->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}
