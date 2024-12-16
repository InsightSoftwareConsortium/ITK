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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNrrdImageIO.h"
#include "itkTestingMacros.h"

// Specific ImageIO test

int
itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " Input Output\n";
    return EXIT_FAILURE;
  }

  using InPixelType = itk::DiffusionTensor3D<double>;
  using InImage = itk::Image<InPixelType, 3>;

  using ReaderType = itk::ImageFileReader<InImage>;

  auto reader = ReaderType::New();

  reader->SetImageIO(itk::NrrdImageIO::New());

  reader->SetFileName(argv[1]);

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
  }

  const InImage::Pointer image = reader->GetOutput();
  image->Print(std::cout);

  // Generate test image
  const itk::ImageFileWriter<InImage>::Pointer writer = itk::ImageFileWriter<InImage>::New();
  writer->SetImageIO(itk::NrrdImageIO::New());
  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);
  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
