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

#include "itkImageSeriesReader.h"
#include "itkImageFileWriter.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkTestingMacros.h"

/// \brief is comparison with a percentage tolerance
///
/// returns true of  the current value is with in tol percentage of m);
///
///  | u - v | <= e * |u| or |  u - v | <= e * |v|
/// defines a "close with tolerance e" relationship between u and v
/// this is a symmetric comparison
///
/// Also it is check to see if the u an v are both less then
/// epsilon for the type
static bool
IsEqualTolerant(const float lm, const float rm, double tol)
{
  tol = itk::Math::abs(tol);
  float temp = itk::Math::abs(lm - rm);
  return temp <= tol * itk::Math::abs(lm) || temp <= tol * itk::Math::abs(rm) ||
         (itk::Math::abs(lm) < std::numeric_limits<float>::epsilon() &&
          itk::Math::abs(rm) < std::numeric_limits<float>::epsilon());
}

int
itkGDCMSeriesStreamReadImageWriteTest(int argc, char * argv[])
{
  if (argc < 10)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " DicomDirectory  outputFile "
              << " spacingX spacingY spacingZ recursive useSeriesDetails loadSequences loadPrivateTags [ "
                 "force-no-streaming 1|0]"
              << std::endl;
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<unsigned short, 3>;

  unsigned int numberOfDataPieces = 4;

  ImageType::SpacingType expectedSpacing;

  expectedSpacing[0] = std::stod(argv[3]);
  expectedSpacing[1] = std::stod(argv[4]);
  expectedSpacing[2] = std::stod(argv[5]);

  bool forceNoStreaming = true;
  if (argc > 10)
  {
    if (std::stoi(argv[10]) != 1)
    {
      forceNoStreaming = false;
    }
  }

  bool expectedToStream = !forceNoStreaming;

  using ImageIOType = itk::GDCMImageIO;
  using SeriesFileNames = itk::GDCMSeriesFileNames;

  auto gdcmIO = ImageIOType::New();
  auto filenameGenerator = SeriesFileNames::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filenameGenerator, GDCMSeriesFileNames, ProcessObject);


  auto recursive = static_cast<bool>(std::stoi(argv[6]));
  ITK_TEST_SET_GET_BOOLEAN(filenameGenerator, Recursive, recursive);

  auto useSeriesDetails = static_cast<bool>(std::stoi(argv[7]));
  filenameGenerator->SetUseSeriesDetails(useSeriesDetails);
  ITK_TEST_SET_GET_VALUE(useSeriesDetails, filenameGenerator->GetUseSeriesDetails());

  auto loadSequences = static_cast<bool>(std::stoi(argv[8]));
  ITK_TEST_SET_GET_BOOLEAN(filenameGenerator, LoadSequences, loadSequences);

  auto loadPrivateTags = static_cast<bool>(std::stoi(argv[9]));
  ITK_TEST_SET_GET_BOOLEAN(filenameGenerator, LoadPrivateTags, loadPrivateTags);

  // Test exceptions
  const char * pInputDirectory = nullptr;
  ITK_TRY_EXPECT_EXCEPTION(filenameGenerator->SetInputDirectory(pInputDirectory));

  // Exercise warnings
  std::string inputDirectory = "";
  filenameGenerator->SetInputDirectory(inputDirectory);

  inputDirectory = "NotADirectory";
  filenameGenerator->SetInputDirectory(inputDirectory);

  inputDirectory = argv[1];
  filenameGenerator->SetInputDirectory(inputDirectory);

  using ReaderType = itk::ImageSeriesReader<ImageType>;
  auto reader = ReaderType::New();

  const ReaderType::FileNamesContainer & filenames = filenameGenerator->GetInputFileNames();

  reader->SetFileNames(filenames);
  reader->SetImageIO(gdcmIO);


  using MonitorFilter = itk::PipelineMonitorImageFilter<ImageType>;
  auto monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());

  using StreamingFilter = itk::StreamingImageFilter<ImageType, ImageType>;
  auto streamer = StreamingFilter::New();
  streamer->SetInput(monitor->GetOutput());
  streamer->SetNumberOfStreamDivisions(numberOfDataPieces);

  try
  {
    if (forceNoStreaming)
    {
      // no streaming
      reader->UseStreamingOff();
      streamer->Update();
    }
    else
    {
      // stream based on the number of z-slices
      reader->UseStreamingOn();
      reader->UpdateOutputInformation();
      numberOfDataPieces = reader->GetOutput()->GetLargestPossibleRegion().GetSize()[2];
      streamer->SetNumberOfStreamDivisions(numberOfDataPieces);
      streamer->Update();
    }
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    std::cerr << monitor;
    return EXIT_FAILURE;
  }

  // verify things executed as expected
  bool passed = true;
  if (expectedToStream)
  {
    if (!monitor->VerifyAllInputCanStream(numberOfDataPieces))
    {
      passed = false;
    }
  }
  else
  {
    if (!monitor->VerifyAllInputCanNotStream())
    {
      passed = false;
    }
  }

  if (!passed)
  {
    std::cerr << monitor << std::endl;
    std::cerr << "pipeline did not execute as expected!" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Origin: " << reader->GetOutput()->GetOrigin() << std::endl;
  std::cout << "Direction: " << reader->GetOutput()->GetDirection() << std::endl;
  std::cout << "Spacing: " << reader->GetOutput()->GetSpacing() << std::endl;
  std::cout << "Expected Spacing: " << expectedSpacing << std::endl;


  ImageType::SpacingType spacing = reader->GetOutput()->GetSpacing();

  // we only give 4 bits of tolerance, IEEE float a 24-bit mantissa
  const double percentTolerance = 1.0 / static_cast<double>(1U << 18);

  if (!IsEqualTolerant(spacing[0], expectedSpacing[0], percentTolerance) ||
      !IsEqualTolerant(spacing[1], expectedSpacing[1], percentTolerance) ||
      !IsEqualTolerant(spacing[2], expectedSpacing[2], percentTolerance))
  {
    std::cerr << "Spacing does not match expected" << std::endl;
    return EXIT_FAILURE;
  }


  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();

  writer->SetFileName(argv[2]);
  writer->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
