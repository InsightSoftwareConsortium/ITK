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
#include "itkImageSeriesWriter.h"
#include "itkNumericSeriesFileNames.h"
#include "itkJPEG2000ImageIOFactory.h"
#include "itkTestingMacros.h"

#include <fstream>

int
itkJPEG2000ImageIOTest05(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input outputdir extension" << std::endl;
    return EXIT_FAILURE;
  }


  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, 3>;
  using OutputImageType = itk::Image<PixelType, 2>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  // reader->SetUseStreaming( true );

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  reader->GetOutput()->Print(std::cout);

  //  Register the factory
  itk::JPEG2000ImageIOFactory::RegisterOneFactory();

  itk::NumericSeriesFileNames::Pointer fit = itk::NumericSeriesFileNames::New();

  using WriterType = itk::ImageSeriesWriter<ImageType, OutputImageType>;

  auto writer = WriterType::New();


  char format[4096];
  snprintf(format, sizeof(format), "%s/series.%%d.%s", argv[2], argv[3]);

  std::cout << "Format = " << format << std::endl;

  ImageType::RegionType region = reader->GetOutput()->GetBufferedRegion();
  ImageType::SizeType   size = region.GetSize();

  fit->SetStartIndex(0);
  fit->SetEndIndex(size[2] - 1); // The number of slices to write
  fit->SetIncrementIndex(1);
  fit->SetSeriesFormat(format);

  writer->SetInput(reader->GetOutput());
  writer->SetFileNames(fit->GetFileNames());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
