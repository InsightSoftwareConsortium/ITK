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

#include "itkImage.h"
#include <iostream>

#include "itkMINCImageIO.h"
#include "itkMINCImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include <vector>
#include <numeric>

int
itkMINCImageIOTest2(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputfile outputfile " << std::endl;
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<unsigned short, 3>;

  itk::MINCImageIO::Pointer mincIO1 = itk::MINCImageIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(mincIO1, MINCImageIO, ImageIOBase);


  unsigned int               supportedDimCount = 4; // includes the degenerate 0-dimensional case
  std::vector<unsigned long> supportedDims(supportedDimCount);
  std::iota(std::begin(supportedDims), std::end(supportedDims), 0);
  for (auto const & value : supportedDims)
  {
    ITK_TEST_EXPECT_TRUE(mincIO1->SupportsDimension(value));
  }

  ITK_TEST_EXPECT_TRUE(!mincIO1->SupportsDimension(supportedDims.back() + 1));

  itk::MINCImageIO::Pointer mincIO2 = itk::MINCImageIO::New();

  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  reader->SetImageIO(mincIO1);
  writer->SetImageIO(mincIO2);

  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  writer->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  ImageType::ConstPointer image = reader->GetOutput();

  image->Print(std::cout);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
