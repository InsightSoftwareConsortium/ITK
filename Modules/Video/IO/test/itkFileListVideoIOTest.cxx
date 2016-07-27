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
#include <iostream>
#include <fstream>

#include "itkFileListVideoIO.h"
#include "itkImportImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"
#include "itkVideoIOBase.h"

typedef itk::SizeValueType SizeValueType;


int test_FileListVideoIO( const char* input,
                           char* nonVideoInput,
                           char* output,
                           char* itkNotUsed(cameraOutput),
                           unsigned int inWidth,
                           unsigned int inHeight,
                           SizeValueType inNumFrames,
                           double inFpS )
{

  // ITK typedefs
  typedef itk::RGBPixel<char>                  PixelType;
  typedef itk::Image<PixelType, 2>             ImageType;
  typedef itk::ImageFileReader<ImageType>      ReaderType;

  int ret = EXIT_SUCCESS;

  // Create the VideoIO
  itk::FileListVideoIO::Pointer fileListIO = itk::FileListVideoIO::New();

  EXERCISE_BASIC_OBJECT_METHODS( fileListIO, FileListVideoIO, VideoIOBase );

  fileListIO->SetFileName(input);


  std::cout << "FileListVideoIO::CanReadFile..." << std::endl;

  // Test CanReadFile on good file
  if (!fileListIO->CanReadFile(input))
    {
    std::cerr << "Could not read " << input << std::endl;
    ret = EXIT_FAILURE;
    }

  // Test CanReadFile on non-existant file
  std::string nonExistantFile = "Bad/Path/To/Nothing";
  if (fileListIO->CanReadFile(nonExistantFile.c_str()))
    {
    std::cerr << "Should have failed to open \"" << nonExistantFile << "\"" << std::endl;
    ret = EXIT_FAILURE;
    }

  // Test CanReadFile on non-image file list
  if (fileListIO->CanReadFile(nonVideoInput))
    {
    std::cerr << "Should have failed to open \"" << nonVideoInput << "\"" << std::endl;
    ret = EXIT_FAILURE;
    }


  std::cout << "FileListVideoIO::ReadImageInformation..." << std::endl;

  fileListIO->SetFileName(input);
  fileListIO->ReadImageInformation();
  bool infoSet = true;
  std::stringstream paramMessage;
  if (fileListIO->GetDimensions(0) != inWidth)
    {
    infoSet = false;
    paramMessage << "Width mismatch: (expected) " << inWidth << " != (got) "
      << fileListIO->GetDimensions(0) << std::endl;
    }
  if (fileListIO->GetDimensions(1) != inHeight)
    {
    infoSet = false;
    paramMessage << "Height mismatch: (expected) " << inHeight << " != (got) "
      << fileListIO->GetDimensions(1) << std::endl;
    }
  double epsilon = 0.0001;
  if ( !itk::Math::FloatAlmostEqual( fileListIO->GetFramesPerSecond(), inFpS, 10, epsilon) )
    {
    infoSet = false;
    paramMessage << "FpS mismatch: (expected) " << inFpS << " != (got) " << fileListIO->GetFramesPerSecond()
      << std::endl;
    }
  if (fileListIO->GetFrameTotal() != inNumFrames)
    {
    infoSet = false;
    paramMessage << "FrameTotal mismatch: (expected) " << inNumFrames << " != (got) "
      << fileListIO->GetFrameTotal() << std::endl;
    }

  if (!infoSet)
    {
    std::cerr << paramMessage.str();
    ret = EXIT_FAILURE;
    }

  // Exercise other FileListVideoIO methods
  std::cout << "PositionInMSec: " << fileListIO->GetPositionInMSec() << std::endl;
  std::cout << "Ratio: " << fileListIO->GetRatio() << std::endl;
  std::cout << "IFrameInterval: " << fileListIO->GetIFrameInterval() << std::endl;

  std::cout << "FileListVideoIO::Read..." << std::endl;
  std::cout << "Comparing all " << fileListIO->GetFrameTotal() << " frames" << std::endl;

  // Set up ImageFileReader
  ReaderType::Pointer reader = ReaderType::New();

  // Loop through all frames
  std::vector<std::string> filenames = fileListIO->SplitFileNames(input);
  for (SizeValueType i = 0; i < fileListIO->GetFrameTotal(); ++i)
    {
    // Read the image file directly
    reader->SetFileName(filenames[i]);
    reader->Update();

    // Read the image using FileListVideoIO
    size_t bufferSize = fileListIO->GetImageSizeInBytes();
    PixelType* buffer = new PixelType[bufferSize];
    fileListIO->Read(static_cast<void*>(buffer));

    // Compare Spacing, Origin, Direction
    for (unsigned int j = 0; j < ImageType::ImageDimension; ++j)
      {
      if (itk::Math::NotExactlyEquals(fileListIO->GetSpacing(j), reader->GetImageIO()->GetSpacing(j)))
        {
        std::cerr << "Spacing not read correctly" << std::endl;
        ret = false;
        }
      if (itk::Math::NotExactlyEquals(fileListIO->GetOrigin(j), reader->GetImageIO()->GetOrigin(j)))
        {
        std::cerr << "Origin not read correctly" << std::endl;
        ret = false;
        }
      if (fileListIO->GetDirection(j) != reader->GetImageIO()->GetDirection(j))
        {
        std::cerr << "Direction not read correctly" << std::endl;
        ret = false;
        }
      }

    // Compare buffer contents
    if (memcmp(reinterpret_cast<void*>(buffer),
        reinterpret_cast<void*>(reader->GetOutput()->GetBufferPointer()), bufferSize))
      {
      std::cerr << "Frame buffers don't match for frame " << i << std::endl;
      ret = false;
      }
    delete[] buffer;
    }


  std::cout << "FileListVideoIO::SetNextFrameToRead" << std::endl;

  // try seeking to the end
  SizeValueType seekFrame = fileListIO->GetFrameTotal()-1;
  if (!fileListIO->SetNextFrameToRead(seekFrame))
    {
    std::cerr << "Failed to seek to second I-Frame..." << std::endl;
    ret = EXIT_FAILURE;
    }

  // Save the current parameters
  double fps = fileListIO->GetFramesPerSecond();
  unsigned int width = fileListIO->GetDimensions(0);
  unsigned int height = fileListIO->GetDimensions(1);
  const char* fourCC = "MP42";
  unsigned int nChannels = fileListIO->GetNumberOfComponents();

  // Reset the VideoIO
  fileListIO->FinishReadingOrWriting();


  std::cout << "FileListVideoIO::SetWriterParameters..." << std::endl;

  // Reset the saved parameters
  std::vector<itk::SizeValueType> size;
  size.push_back(width);
  size.push_back(height);
  fileListIO->SetWriterParameters(fps, size, fourCC, nChannels, itk::ImageIOBase::UCHAR);

  // Make sure they set correctly
  if (itk::Math::NotExactlyEquals(fileListIO->GetFramesPerSecond(), fps) || fileListIO->GetDimensions(0) != width ||
      fileListIO->GetDimensions(1) != height || fileListIO->GetNumberOfComponents() != nChannels)
    {
    std::cerr << "Didn't set writer parmeters correctly" << std::endl;
    std::cerr << "  FpS -> Got: " << fileListIO->GetFramesPerSecond() << " Expected: " << fps
              << std::endl;
    std::cerr << "  width -> Got: " << fileListIO->GetDimensions(0) << " Expected: "
              << width << std::endl;
    std::cerr << "  height -> Got: " << fileListIO->GetDimensions(1) << " Expected: "
              << height << std::endl;
    std::cerr << "  NChannels -> Got: " << fileListIO->GetNumberOfComponents()
              << " Expected: " << nChannels << std::endl;
    ret = EXIT_FAILURE;
    }


  std::cout << "FileListVideoIO::CanWriteFile..." << std::endl;

  // Test CanWriteFile on good filename
  if (!fileListIO->CanWriteFile(output))
    {
    std::cerr << "CanWriteFile didn't return true correctly" << std::endl;
    ret = EXIT_FAILURE;
    }

  // Test CanWriteFile on bad filename
  if (fileListIO->CanWriteFile("asdfasdfasdf"))
    {
    std::cerr << "CanWriteFile should have failed for bad filename" << std::endl;
    ret = EXIT_FAILURE;
    }


  std::cout << "FileListVideoIO::Write..." << std::endl;

  // Set output filename
  fileListIO->SetFileName( output );

  // Set up a two more VideoIOs to read while we're writing
  itk::FileListVideoIO::Pointer fileListIO2 = itk::FileListVideoIO::New();
  itk::FileListVideoIO::Pointer fileListIO3 = itk::FileListVideoIO::New();
  fileListIO2->SetFileName( input );
  fileListIO2->ReadImageInformation();
  fileListIO3->SetFileName( output );

  // Loop through all frames to read with fileListIO2 and write with fileListIO
  for (unsigned int i = 0; i < inNumFrames; ++i)
    {
    // Set up a buffer to read to
    size_t bufferSize = fileListIO2->GetImageSizeInBytes();
    PixelType* buffer = new PixelType[bufferSize];

    // Read into the buffer
    fileListIO2->Read(static_cast<void*>(buffer));

    // Write out the frame from the buffer
    fileListIO->Write(static_cast<void*>(buffer));

    // Now, read back in from the written file and make sure the buffers match
    fileListIO3->ReadImageInformation();
    PixelType* reReadBuffer = new PixelType[bufferSize];
    fileListIO3->Read(static_cast<void*>(reReadBuffer));
    if (memcmp(reinterpret_cast<void*>(buffer), reinterpret_cast<void*>(reReadBuffer), bufferSize))
      {
      std::cerr << "Didn't write correctly for frame " << i << std::endl;
      ret = EXIT_FAILURE;
      }
    delete[] buffer;
    delete[] reReadBuffer;
    }

  // Finish writing
  fileListIO2->FinishReadingOrWriting();
  fileListIO->FinishReadingOrWriting();

  std::cout << "Test finished" << std::endl;

  return ret;
}

int itkFileListVideoIOTest( int argc, char *argv[] )
{
  if (argc != 13)
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " videoInput(5 files) non-VideoInput videoOutput webcamOutput";
    std::cerr << " width height numFrames FpS" << std::endl;
    return EXIT_FAILURE;
    }

  // Parse the input video files
  std::string inFile = "";
  for( int i = 1; i <= 5; ++i )
    {
    inFile = inFile + std::string(argv[i]);
    if( i != 5 )
      {
      inFile = inFile + std::string(",");
      }
    }
  return test_FileListVideoIO(inFile.c_str(), argv[6], argv[7], argv[8],
                              atoi(argv[9]), atoi(argv[10]),
                              atoi(argv[11]), atof(argv[12]));
}
