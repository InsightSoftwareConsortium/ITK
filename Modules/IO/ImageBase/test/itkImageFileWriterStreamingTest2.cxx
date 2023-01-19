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
#include "itkPipelineMonitorImageFilter.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkTestingMacros.h"

using PixelType = unsigned char;
using ImageType = itk::Image<PixelType, 3>;

using ReaderType = itk::ImageFileReader<ImageType>;
using WriterType = itk::ImageFileWriter<ImageType>;


bool
SameImage(std::string output, std::string baseline)
{

  PixelType    intensityTolerance = 0;
  unsigned int radiusTolerance = 0;
  unsigned int numberOfPixelTolerance = 0;

  auto testReader = ReaderType::New();
  auto baselineReader = ReaderType::New();
  testReader->SetFileName(output);
  baselineReader->SetFileName(baseline);

  using DiffType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  auto diff = DiffType::New();
  diff->SetValidInput(baselineReader->GetOutput());
  diff->SetTestInput(testReader->GetOutput());
  diff->SetDifferenceThreshold(intensityTolerance);
  diff->SetToleranceRadius(radiusTolerance);
  diff->UpdateLargestPossibleRegion();

  unsigned long status = diff->GetNumberOfPixelsWithDifferences();

  if (status > numberOfPixelTolerance)
  {
    return false;
  }
  return true;
}

// This test is designed to improve coverage and test boundary cases
int
itkImageFileWriterStreamingTest2(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output " << std::endl;
    return EXIT_FAILURE;
  }

  // We remove the output file
  itksys::SystemTools::RemoveFile(argv[2]);

  //


  unsigned int numberOfDataPieces = 4;

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->SetUseStreaming(true);

  using MonitorFilter = itk::PipelineMonitorImageFilter<ImageType>;
  auto monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());

  auto writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(monitor->GetOutput());
  writer->SetNumberOfStreamDivisions(numberOfDataPieces);

  if (std::string(argv[2]) != writer->GetFileName())
  {
    return EXIT_FAILURE;
  }

  if (numberOfDataPieces != writer->GetNumberOfStreamDivisions())
  {
    return EXIT_FAILURE;
  }

  // Write the whole image
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  if (!monitor->VerifyAllInputCanStream(4))
  {
    std::cout << monitor;
    return EXIT_FAILURE;
  }

  if (!SameImage(argv[1], argv[2]))
  {
    return EXIT_FAILURE;
  }

  reader->Modified();
  // get the size of the image
  reader->UpdateOutputInformation();
  ImageType::RegionType largestRegion;
  largestRegion = reader->GetOutput()->GetLargestPossibleRegion().GetSize();
  itk::ImageIORegion ioregion(3);


  ////////////////////////////////////////////////
  // test 1x1 size
  ioregion.SetIndex(0, largestRegion.GetIndex()[0] + largestRegion.GetSize()[0] / 2);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1] + largestRegion.GetSize()[1] / 2);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2] + largestRegion.GetSize()[2] / 2);
  ioregion.SetSize(0, 1);
  ioregion.SetSize(1, 1);
  ioregion.SetSize(2, 1);

  writer->SetIORegion(ioregion);

  std::cout << "=== Updating 1x1x1 IORegion ==" << std::endl;
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  if (!monitor->VerifyAllInputCanStream(1))
  {
    std::cout << monitor;
    return EXIT_FAILURE;
  }

  if (!SameImage(argv[1], argv[2]))
  {
    return EXIT_FAILURE;
  }

  reader->Modified();
  ////////////////////////////////////////////////
  // test 2x2 with odd offset
  ioregion.SetIndex(0, largestRegion.GetIndex()[0] + largestRegion.GetSize()[0] / 2 + 1);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1] + largestRegion.GetSize()[1] / 2 + 1);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2] + largestRegion.GetSize()[2] / 2 + 1);
  ioregion.SetSize(0, 2);
  ioregion.SetSize(1, 2);
  ioregion.SetSize(2, 2);

  writer->SetIORegion(ioregion);

  std::cout << "=== Updating 2x2x2 IORegion with odd offset ==" << std::endl;
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  if (!monitor->VerifyAllInputCanStream(-1))
  {
    std::cout << monitor;
    return EXIT_FAILURE;
  }

  if (!SameImage(argv[1], argv[2]))
  {
    return EXIT_FAILURE;
  }


  reader->Modified();
  ////////////////////////////////////////////////
  // test long skiny
  ioregion.SetIndex(0, largestRegion.GetIndex()[0]);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]);
  ioregion.SetSize(0, 1);
  ioregion.SetSize(1, 1);
  ioregion.SetSize(2, largestRegion.GetSize()[2]);

  writer->SetIORegion(ioregion);

  std::cout << "=== Updating 1x1xlong IORegion ==" << std::endl;
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  if (!monitor->VerifyAllInputCanStream(-1))
  {
    std::cout << monitor;
    return EXIT_FAILURE;
  }

  if (!SameImage(argv[1], argv[2]))
  {
    return EXIT_FAILURE;
  }


  reader->Modified();
  ////////////////////////////////////////////////
  // test long skiny
  ioregion.SetIndex(0, largestRegion.GetIndex()[0]);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]);
  ioregion.SetSize(0, 1);
  ioregion.SetSize(1, largestRegion.GetSize()[1]);
  ioregion.SetSize(2, 1);

  writer->SetIORegion(ioregion);

  std::cout << "=== Updating 1xlongx1 IORegion ==" << std::endl;
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  if (!monitor->VerifyAllInputCanStream(-1))
  {
    std::cout << monitor;
    return EXIT_FAILURE;
  }

  if (!SameImage(argv[1], argv[2]))
  {
    return EXIT_FAILURE;
  }

  reader->Modified();
  ////////////////////////////////////////////////
  // test full region
  ioregion.SetIndex(0, largestRegion.GetIndex()[0]);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]);
  ioregion.SetSize(0, largestRegion.GetSize()[0]);
  ioregion.SetSize(1, largestRegion.GetSize()[1]);
  ioregion.SetSize(2, largestRegion.GetSize()[2]);

  writer->SetIORegion(ioregion);

  std::cout << "=== Updating Full IORegion ==" << std::endl;
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  if (!monitor->VerifyAllInputCanStream(4))
  {
    std::cout << monitor;
    return EXIT_FAILURE;
  }

  if (!SameImage(argv[1], argv[2]))
  {
    return EXIT_FAILURE;
  }

  reader->Modified();

  // Test out of bounds region
  ioregion.SetIndex(0, largestRegion.GetIndex()[0] - 1);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]);
  ioregion.SetSize(0, largestRegion.GetSize()[0]);
  ioregion.SetSize(1, largestRegion.GetSize()[1]);
  ioregion.SetSize(2, largestRegion.GetSize()[2]);

  writer->SetIORegion(ioregion);

  std::cout << "=== Updating out of bounds IORegion ==" << std::endl;
  ITK_TRY_EXPECT_EXCEPTION(writer->Update());


  reader->Modified();

  // Test out of bounds region
  ioregion.SetIndex(0, largestRegion.GetIndex()[0] + largestRegion.GetSize()[0] / 2 + 1);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1] + largestRegion.GetSize()[1] / 2 + 1);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2] + largestRegion.GetSize()[2] / 2 + 1);
  ioregion.SetSize(0, largestRegion.GetSize()[0]);
  ioregion.SetSize(1, largestRegion.GetSize()[1]);
  ioregion.SetSize(2, largestRegion.GetSize()[2] + 1);

  writer->SetIORegion(ioregion);

  std::cout << "=== Updating out of bounds IORegion ==" << std::endl;
  ITK_TRY_EXPECT_EXCEPTION(writer->Update());


  reader->Modified();

  // Test when regions aren't matching
  ImageType::RegionType halfLargestRegion;
  halfLargestRegion.SetIndex(largestRegion.GetIndex());
  halfLargestRegion.SetSize(0, largestRegion.GetSize()[0] / 2);
  halfLargestRegion.SetSize(1, largestRegion.GetSize()[1] / 2);
  halfLargestRegion.SetSize(2, largestRegion.GetSize()[2] / 2);


  monitor->GetOutput()->SetRequestedRegion(halfLargestRegion);

  ioregion.SetIndex(0, largestRegion.GetIndex()[0]);
  ioregion.SetIndex(1, largestRegion.GetIndex()[1]);
  ioregion.SetIndex(2, largestRegion.GetIndex()[2]);
  ioregion.SetSize(0, largestRegion.GetSize()[0] / 3);
  ioregion.SetSize(1, largestRegion.GetSize()[1] / 3);
  ioregion.SetSize(2, largestRegion.GetSize()[2] / 3);


  writer->SetIORegion(ioregion);

  ITK_TRY_EXPECT_NO_EXCEPTION(monitor->Update());
  monitor->VerifyAllInputCanStream(1);

  std::cout << "=== Updating mismatched IORegion ==" << std::endl;
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  if (!monitor->VerifyAllNoUpdate())
  {
    std::cout << monitor;
    return EXIT_FAILURE;
  }


  if (!SameImage(argv[1], argv[2]))
  {
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
