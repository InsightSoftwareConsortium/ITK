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
#include "itkRandomImageSource.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkRawImageIO.h"
#include "itkTestingMacros.h"


// Specific ImageIO test

int
itkRawImageIOTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " Output1 Output2" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using ImageType = itk::Image<unsigned short, 2>;
  using PixelType = ImageType::PixelType;
  using ImageIteratorType = itk::ImageRegionConstIterator<ImageType>;

  // Create a source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  ImageType::SizeValueType size[Dimension];

  size[0] = 128;
  size[1] = 64;

  itk::RandomImageSource<ImageType>::Pointer random;
  random = itk::RandomImageSource<ImageType>::New();
  random->SetMin(0);
  random->SetMax(24680);
  random->SetSize(size);

  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  //
  itk::RawImageIO<unsigned short, Dimension>::Pointer io;
  io = itk::RawImageIO<unsigned short, Dimension>::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(io, RawImageIO, ImageIOBase);

  //  io->SetFileTypeToASCII();

  io->SetFileDimensionality(Dimension);
  ITK_TEST_SET_GET_VALUE(Dimension, io->GetFileDimensionality());

  ITK_TEST_EXPECT_TRUE(io->SupportsDimension(Dimension));

  unsigned long dim = 3;
  ITK_TEST_EXPECT_TRUE(!io->SupportsDimension(dim));

  // Binary files have no image information to read
  io->WriteImageInformation();

  // Write out the image
  std::string filename = "";
  ITK_TEST_EXPECT_TRUE(!io->CanWriteFile(filename.c_str()));

  ITK_TRY_EXPECT_EXCEPTION(io->GetHeaderSize());

  filename = argv[1];
  ITK_TEST_EXPECT_TRUE(io->CanWriteFile(filename.c_str()));

  itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput(random->GetOutput());
  writer->SetFileName(filename);
  writer->SetImageIO(io);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Create a source object (in this case a reader)
  itk::ImageFileReader<ImageType>::Pointer reader;
  reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(io);
  reader->SetFileName(filename);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  io->CanReadFile(filename.c_str());

  // Not used; empty method body; called for coverage purposes
  io->ReadHeader();

  // Compare pixel by pixel in memory


  ImageIteratorType it(reader->GetOutput(), reader->GetOutput()->GetBufferedRegion());

  ImageIteratorType ot(random->GetOutput(), random->GetOutput()->GetBufferedRegion());

  it.GoToBegin();
  ot.GoToBegin();
  while (!it.IsAtEnd())
  {
    const PixelType iv = it.Get();
    const PixelType ov = ot.Get();
    if (iv != ov)
    {
      std::cerr << "Error in read/write of pixel " << it.GetIndex() << std::endl;
      std::cerr << "Read value  is : " << iv << std::endl;
      std::cerr << "it should be   : " << ov << std::endl;
      std::cerr << "Test FAILED ! " << std::endl;
      return EXIT_FAILURE;
    }
    ++it;
    ++ot;
  }

  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);
  writer->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
