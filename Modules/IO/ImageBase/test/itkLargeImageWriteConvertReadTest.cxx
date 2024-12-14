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

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkTimeProbesCollectorBase.h"
#include "itkTestingMacros.h"

int
itkLargeImageWriteConvertReadTest(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputFileName numberOfPixelsInOneDimension"
              << '\n';
    return EXIT_FAILURE;
  }
  using OutputPixelType = unsigned char;
  using OutputImageType = itk::Image<OutputPixelType, 2>;
  using InputImageType = itk::Image<unsigned short, 2>;

  using WriterType = itk::ImageFileWriter<OutputImageType>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  itk::TimeProbesCollectorBase chronometer;

  { // begin write block
    auto image = OutputImageType::New();

    const size_t numberOfPixelsInOneDimension = atol(argv[2]);

    auto                             size = itk::MakeFilled<OutputImageType::SizeType>(numberOfPixelsInOneDimension);
    const OutputImageType::IndexType index{};

    const OutputImageType::RegionType region{ index, size };
    image->SetRegions(region);

    chronometer.Start("Allocate");
    image->Allocate();
    chronometer.Stop("Allocate");

    std::cout << "Initializing pixel values " << '\n';
    using IteratorType = itk::ImageRegionIterator<OutputImageType>;

    IteratorType itr(image, region);
    itr.GoToBegin();

    OutputPixelType pixelValue{};

    chronometer.Start("Initializing");
    while (!itr.IsAtEnd())
    {
      itr.Set(pixelValue);
      ++pixelValue;
      ++itr;
    }
    chronometer.Stop("Initializing");

    std::cout << "Trying to write the image to disk" << '\n';
    try
    {
      auto writer = WriterType::New();
      writer->SetInput(image);
      writer->SetFileName(argv[1]);
      chronometer.Start("Write");
      writer->Update();
      chronometer.Stop("Write");
    }
    catch (const itk::ExceptionObject & ex)
    {
      std::cout << ex << '\n';
      return EXIT_FAILURE;
    }

  } // end writing block so data is freed

  std::cout << "Trying to read the image back from disk" << '\n';
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  try
  {
    chronometer.Start("Read");
    reader->Update();
    chronometer.Stop("Read");
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << ex << '\n';
    return EXIT_FAILURE;
  }

  const InputImageType::ConstPointer readImage = reader->GetOutput();
  chronometer.Report(std::cout);

  std::cout << '\n';
  std::cout << "Test PASSED !" << '\n';

  return EXIT_SUCCESS;
}
