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
#include <iostream>

#include "itkVideoFileReader.h"
#include "itkVideoFileWriter.h"
#include "itkFileListVideoIOFactory.h"
#include "itkTestingMacros.h"

int
itkVideoFileReaderWriterTest(int argc, char * argv[])
{
  // Check parameters
  if (argc != 10)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv)
              << " VideoInput(5 files) ImageOutput IFrameSafe framesPerSecond fourCC" << std::endl;
    return EXIT_FAILURE;
  }

  // Instantiate a new reader
  using PixelType = itk::RGBPixel<unsigned char>;
  constexpr unsigned int NumberOfDimensions = 2;
  using FrameType = itk::Image<PixelType, NumberOfDimensions>;
  using VideoType = itk::VideoStream<FrameType>;
  using VideoReaderType = itk::VideoFileReader<VideoType>;
  using VideoWriterType = itk::VideoFileWriter<VideoType>;

  std::string inFile = "";
  for (int i = 1; i <= 5; ++i)
  {
    inFile = inFile + std::string(argv[i]);
    if (i != 5)
    {
      inFile = inFile + std::string(",");
    }
  }
  auto reader = VideoReaderType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(reader, VideoFileReader, VideoSource);

  reader->SetFileName(inFile.c_str());
  ITK_TEST_SET_GET_VALUE(inFile, std::string(reader->GetFileName()));

  auto iFrameSafe = static_cast<bool>(std::stoi(argv[7]));
  ITK_TEST_SET_GET_BOOLEAN(reader, IFrameSafe, iFrameSafe);

  // I'm still not sure how to handle this right, but for now, just manually
  // register an FileListVideoIO
  itk::ObjectFactoryBase::RegisterFactory(itk::FileListVideoIOFactory::New());

  // Set up the writer
  auto writer = VideoWriterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(writer, VideoFileWriter, TemporalProcessObject);

  writer->SetInput(reader->GetOutput());
  writer->SetFileName(std::string(argv[6]));
  ITK_TEST_SET_GET_VALUE(std::string(argv[6]), writer->GetFileName());

  auto framesPerSecond = static_cast<VideoWriterType::TemporalRatioType>(std::stod(argv[8]));
  writer->SetFramesPerSecond(framesPerSecond);
  ITK_TEST_SET_GET_VALUE(framesPerSecond, writer->GetFramesPerSecond());

  auto fourCC = std::string(argv[9]);
  writer->SetFourCC(fourCC);
  ITK_TEST_SET_GET_VALUE(fourCC, writer->GetFourCC());

  // Call Update on the writer to process the entire video
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
