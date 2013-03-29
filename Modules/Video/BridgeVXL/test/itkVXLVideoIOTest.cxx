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

#include "itkVXLVideoIO.h"
#include "itkImportImageFilter.h"
#include "itkImageFileWriter.h"

// ITK typedefs
typedef itk::RGBPixel<char>                  PixelType;
typedef itk::ImportImageFilter<PixelType, 2> ImportFilterType;
typedef itk::Image<PixelType, 2>             ImageType;
typedef itk::ImageFileWriter<ImageType>      WriterType;
typedef itk::SizeValueType                   SizeValueType;

//
// Utility function to get an ITK image from an void* buffer
//
ImageType::Pointer itkImageFromBuffer( itk::VXLVideoIO::Pointer vxlIO, void* buffer, size_t bufferSize )
{
  // Set up for incoming image
  ImageType::RegionType region;
  ImageType::SizeType size;
  ImageType::IndexType start;
  size[0] = vxlIO->GetDimensions(0);
  size[1] = vxlIO->GetDimensions(1);
  start.Fill(0);
  region.SetIndex(start);
  region.SetSize(size);
  ImageType::PointType origin;
  ImageType::SpacingType space;
  origin.Fill(0.0);
  space.Fill(1.0);  // May need fixing

  // Use itkImportImageFilter to create an ITK image
  ImportFilterType::Pointer importFilter = ImportFilterType::New();
  importFilter->SetRegion(region);
  importFilter->SetOrigin(origin);
  importFilter->SetSpacing(space);
  importFilter->SetImportPointer(reinterpret_cast<PixelType*>(buffer), bufferSize, false);
  importFilter->Update();
  ImageType::Pointer frame = importFilter->GetOutput();

  return frame;
}

//
// Utility function for comparing output of Read buffer to VXL image
//
// Note: vxlIO should already have called ReadImageInformation
//
bool readCorrectly( itk::VXLVideoIO::Pointer vxlIO, vidl_ffmpeg_istream* stream, SizeValueType frameNumber )
{
  bool ret = true;

  // Set up the buffer for the frame data
  size_t bufferSize = vxlIO->GetImageSizeInBytes();
  PixelType buffer[bufferSize];

  // Read the frame data
  vxlIO->Read(static_cast<void*>(buffer));

  // Open the frame directly with VXL and convert to RGB 24
  vidl_frame_sptr vxlFrame = stream->read_frame();
  vxlFrame = vidl_convert_frame(vxlFrame, vidl_pixel_format_from_string("RGB 24"));

  // Make sure buffers are same sized
  if (vxlFrame->size() != bufferSize)
    {
    std::cerr << "Frame buffer sizes don't match (" << vxlFrame->size()
              << " != " << bufferSize << ")" << std::endl;
    ret = false;
    }

  // Check meta data
  for (unsigned int i = 0; i < ImageType::ImageDimension; ++i)
    {
    if (vxlIO->GetSpacing(i) != 1.0)
      {
      std::cerr << "Frame Spacing not set correctly" << std::endl;
      ret = false;
      }
    if (vxlIO->GetOrigin(i) != 0.0)
      {
      std::cerr << "Frame Origin not set correctly" << std::endl;
      ret = false;
      }
    if (vxlIO->GetDirection(i) != vxlIO->GetDefaultDirection(i))
      {
      std::cerr << "Frame Direction not set correctly" << std::endl;
      ret = false;
      }
    }

  // Compare buffer contents
  if (memcmp(reinterpret_cast<void*>(buffer), vxlFrame->data(), bufferSize))
    {
    std::cerr << "Frame buffers don't match for frame " << frameNumber << std::endl;
    ret = false;
    }

  // Return
  return ret;
}

/*

//
// Utility function to compare two videos frame by frame
//
bool videosMatch(char* file1, char* file2)
{
  itk::VXLVideoIO::Pointer io1 = itk::VXLVideoIO::New();
  itk::VXLVideoIO::Pointer io2 = itk::VXLVideoIO::New();

  io1->SetFileName(file1);
  io2->SetFileName(file2);

  // Make sure files can be read
  if (!io1->CanReadFile(file1) || !io2->CanReadFile(file2))
    {
    std::cerr << "Cannot read specified files" << std::endl;
    return false;
    }

  // Read in the file information for both
  io1->ReadImageInformation();
  io2->ReadImageInformation();

  // Make sure image info matches
  double e = 0.0001;
  if (io1->GetFrameTotal() != io2->GetFrameTotal() ||
      io1->GetDimensions(1) != io2->GetDimensions(1) ||
      io1->GetDimensions(0) != io2->GetDimensions(0) ||
      io1->GetFramesPerSecond() + e < io2->GetFramesPerSecond() ||
      io1->GetFramesPerSecond() - e > io2->GetFramesPerSecond() ||
      io1->GetNumberOfComponents() != io2->GetNumberOfComponents())
    {

    std::cerr << "Frame information doesn't match" << std::endl;
    std::cerr << "  FrameTotal: " << io1->GetFrameTotal() << ", " << io2->GetFrameTotal() << std::endl;
    std::cerr << "  Height: " << io1->GetDimensions(1) << ", " << io2->GetDimensions(1) << std::endl;
    std::cerr << "  Width: " << io1->GetDimensions(0) << ", " << io2->GetDimensions(0) << std::endl;
    std::cerr << "  FpS: " << io1->GetFramesPerSecond() << ", " << io2->GetFramesPerSecond() << std::endl;
    std::cerr << "  NChannels: " << io1->GetNumberOfComponents() << ", " << io2->GetNumberOfComponents() << std::endl;

    return false;
    }

  // Loop through each frame and compare the buffer for exact match
  size_t bufferSize = io1->GetDimensions(1)*io1->GetDimensions(0)*sizeof(PixelType);
  PixelType buffer1[bufferSize];
  PixelType buffer2[bufferSize];
  for (unsigned int i = 0; i < io1->GetFrameTotal(); ++i)
    {
    io1->Read(reinterpret_cast<void*>(buffer1));
    io2->Read(reinterpret_cast<void*>(buffer2));
    if (memcmp(reinterpret_cast<void*>(buffer1), reinterpret_cast<void*>(buffer2), bufferSize))
      {
      std::cerr << "Frame buffers don't match for frame " << i << std::endl;
      return false;
      }
    }

  // Close the readers
  io1->FinishReadingOrWriting();
  io2->FinishReadingOrWriting();

  // return result
  return true;
}
*/

///////////////////////////////////////////////////////////////////////////////
// This tests all of the functionality of the VXLVideoIO
//
// Usage: [Video Input] [Non-Video Input] [Video Output] [Width] [Height]
//            [Num Frames] [FpS]

                   int test_VXLVideoIO ( char* input, char* nonVideoInput, char* output, char* cameraOutput,
                                         unsigned int inWidth, unsigned int inHeight,
                                         SizeValueType inNumFrames, double inFpS )
{

  int ret = EXIT_SUCCESS;

  // Create the VideoIO
  itk::VXLVideoIO::Pointer vxlIO = itk::VXLVideoIO::New();


  //////
  // CanReadFile
  //////
  std::cout << "VXLVideoIO::CanReadFile..." << std::endl;

  // Test CanReadFile on good file
  if (!vxlIO->CanReadFile(input))
    {
    std::cerr << "Could not read " << input << std::endl;
    ret = EXIT_FAILURE;
    }

  // Test CanReadFile on non-existant file
  std::string nonExistantFile = "Bad/Path/To/Nothing";
  if (vxlIO->CanReadFile(nonExistantFile.c_str()))
    {
    std::cerr << "Should have failed to open \"" << nonExistantFile << "\"" << std::endl;
    ret = EXIT_FAILURE;
    }

  // Test CanReadFile on non-video file
  if (vxlIO->CanReadFile(nonVideoInput))
    {
    std::cerr << "Should have failed to open \"" << nonVideoInput << "\"" << std::endl;
    ret = EXIT_FAILURE;
    }

  //////
  // ReadImageInformation
  //////
  std::cout << "VXLVideoIO::ReadImageInformation..." << std::endl;

  vxlIO->SetFileName(input);
  vxlIO->ReadImageInformation();
  bool infoSet = true;
  std::stringstream paramMessage;
  if (vxlIO->GetDimensions(0) != inWidth)
    {
    infoSet = false;
    paramMessage << "Width mismatch: (expected) " << inWidth << " != (got) "
                 << vxlIO->GetDimensions(0) << std::endl;
    }
  if (vxlIO->GetDimensions(1) != inHeight)
    {
    infoSet = false;
    paramMessage << "Height mismatch: (expected) " << inHeight << " != (got) "
                 << vxlIO->GetDimensions(1) << std::endl;
    }
  double epsilon = 0.0001;
  if (vxlIO->GetFramesPerSecond() < inFpS - epsilon || vxlIO->GetFramesPerSecond() > inFpS + epsilon)
    {
    infoSet = false;
    paramMessage << "FpS mismatch: (expected) " << inFpS << " != (got) " << vxlIO->GetFramesPerSecond()
                 << std::endl;
    }
  if (vxlIO->GetFrameTotal() != inNumFrames)
    {
    infoSet = false;
    paramMessage << "FrameTotal mismatch: (expected) " << inNumFrames << " != (got) "
                 << vxlIO->GetFrameTotal() << std::endl;
    }

  if (!infoSet)
    {
    std::cerr << paramMessage.str();
    ret = EXIT_FAILURE;
    }


  //////
  // Read
  //////
  std::cout << "VXLVideoIO::Read..." << std::endl;
  std::cout << "Comparing all " << vxlIO->GetFrameTotal() << " frames" << std::endl;

  // Set up VXL stream
  vidl_ffmpeg_istream* stream = new vidl_ffmpeg_istream();
  stream->open(vxlIO->GetFileName());

  // Loop through all frames
  for (SizeValueType i = 0; i < vxlIO->GetFrameTotal(); ++i)
    {
    if (!readCorrectly(vxlIO, stream, i))
      {
      std::cerr << "Failed to read frame " << i << " correctly" << std::endl;
      ret = EXIT_FAILURE;
      break;
      }
    }

  // delete stream
  delete stream;


  //////
  // SetNextFrameToRead
  //////
  std::cout << "VXLVideoIO::SetNextFrameToRead" << std::endl;

  // Set up the buffer for the frame data so Read can be called
  //size_t bufferSize = vxlIO->GetDimensions(1)*vxlIO->GetDimensions(0)*vxlIO->GetNumberOfComponents()*vxlIO->GetBytesPerPixel();
  size_t bufferSize = vxlIO->GetImageSizeInBytes();
  PixelType buffer[bufferSize];


  // try seeking to an I-Frame
  SizeValueType seekFrame = vxlIO->GetIFrameInterval();
  if (!vxlIO->SetNextFrameToRead(seekFrame))
    {
    std::cerr << "Failed to seek to second I-Frame..." << std::endl;
    ret = EXIT_FAILURE;
    }

  // Read the frame data which updates the current frame correctly
  vxlIO->Read(static_cast<void*>(buffer));

  if (vxlIO->GetCurrentFrame() != seekFrame)
    {
    std::cerr << "Seek to I-Frame didn't end up in the right place" << std::endl;
    ret = EXIT_FAILURE;
    }


  // If there are I-Frame intervals, check behavior
  if (vxlIO->GetIFrameInterval() > 1)
    {

    // try seeking in-between I-Frames
    seekFrame = vxlIO->GetIFrameInterval()/2;
    if (!vxlIO->SetNextFrameToRead(seekFrame))
      {
      std::cerr << "Failed to seek between I-Frames" << std::endl;
      ret = EXIT_FAILURE;
      }
    vxlIO->Read(static_cast<void*>(buffer));
    if (vxlIO->GetCurrentFrame() != vxlIO->GetIFrameInterval())
      {
      std::cerr << "Seek between I-Frames didn't end up in the right place" << std::endl;
      ret = EXIT_FAILURE;
      }

    // try seeking past last I-Frame
    seekFrame = vxlIO->GetLastIFrame() + 1;
    if (vxlIO->SetNextFrameToRead(seekFrame))
      {
      std::cerr << "Did no fail when seeking past the last I-Frame" << std::endl;
      ret = EXIT_FAILURE;
      }

    }

  // Save the current parameters
  double fps = vxlIO->GetFramesPerSecond();
  unsigned int width = vxlIO->GetDimensions(0);
  unsigned int height = vxlIO->GetDimensions(1);
  const char* fourCC = "MP42";
  unsigned int nChannels = vxlIO->GetNumberOfComponents();

  // Reset the VideoIO
  vxlIO->FinishReadingOrWriting();


  //////
  // Test reading from camera -- If webcam 0 can be opened, it will, otherwise this will be skipped
  //
  // Note: For now, this won't do anything since camera is not implemented in VXL
  //////

  // Check to see if camera is available
  if (vxlIO->CanReadCamera( 0 ))
    {

    std::cout << "VXLVideoIO::Read (from camera)..." << std::endl;

    // Set the reader to use the camera
    vxlIO->SetReadFromCamera();

    // Get information from the camera
    try
      {
      vxlIO->ReadImageInformation();
      }
    catch (itk::ExceptionObject & e)
      {
      std::cerr << "Could not read information from the camera" << std::endl;
      ret = EXIT_FAILURE;
      }

    // set up buffer for camera
    //size_t camBufferSize = vxlIO->GetDimensions(1)*vxlIO->GetDimensions(0)*
    //                        vxlIO->GetNumberOfComponents()*vxlIO->GetBytesPerPixel();
    size_t camBufferSize = vxlIO->GetImageSizeInBytes();
    PixelType camBuffer[camBufferSize];

    // Read from the camera
    try
      {
      vxlIO->Read(reinterpret_cast<void*>(camBuffer));
      }
    catch (itk::ExceptionObject & e)
      {
      std::cerr << "Could not read from the camera" << std::endl;
      ret = EXIT_FAILURE;
      }

    // Get an ITK image from the camera's frame
    ImageType::Pointer cameraFrame = itkImageFromBuffer(vxlIO, camBuffer, camBufferSize);

    // Write out the ITK image -- DEBUG
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName(cameraOutput);
    writer->SetInput(cameraFrame);
    writer->Update();

    // Overwirte the file right away so we're not saving pictures of the tester!
    std::ofstream fs;
    fs.open(cameraOutput);
    fs << "EMPTY... deleted picture from webcam\n";
    fs.close();

    // Finish reading
    vxlIO->FinishReadingOrWriting();

    }


  /////////////////////////////////////////////////////////////////////////////
    // Test Writing
    //

    // Create the VideoIO
    itk::VXLVideoIO::Pointer vxlIO_write = itk::VXLVideoIO::New();

    //////
    // SetWriterParameters
    //////
    std::cout << "VXLVIdeoIO::SetWriterParameters..." << std::endl;

    // Reset the saved parameters
    std::vector<itk::SizeValueType> size;
    size.push_back(width);
    size.push_back(height);
    vxlIO_write->SetWriterParameters(fps, size, fourCC, nChannels, itk::ImageIOBase::UCHAR);

    // Make sure they set correctly
    if (vxlIO_write->GetFramesPerSecond() != fps || vxlIO_write->GetDimensions(0) != width ||
        vxlIO_write->GetDimensions(1) != height || vxlIO_write->GetNumberOfComponents() != nChannels)
      {
      std::cerr << "Didn't set writer parmeters correctly" << std::endl;
      ret = EXIT_FAILURE;
      }

    //////
    // CanWriteFile
    //////
    std::cout << "VXLVideoIO::CanWriteFile..." << std::endl;

    // Test CanWriteFile on good filename
    if (!vxlIO_write->CanWriteFile(output))
      {
      std::cerr << "CanWriteFile didn't return true correctly" << std::endl;
      ret = EXIT_FAILURE;
      }

    // Test CanWriteFile on bad filename
    if (vxlIO_write->CanWriteFile("asdfasdfasdf"))
      {
      std::cerr << "CanWriteFile should have failed for bad filename" << std::endl;
      ret = EXIT_FAILURE;
      }


    //////
    // Write
    //////
    std::cout << "VXLVIdeoIO::Write..." << std::endl;

    // Set output filename
    vxlIO_write->SetFileName( output );

    // Set up a second VideoIO to read while we're writing
    itk::VXLVideoIO::Pointer vxlIO_read = itk::VXLVideoIO::New();
    vxlIO_read->SetFileName( input );
    vxlIO_read->ReadImageInformation();

    // Loop through all frames to read with opencvIO2 and write with opencvIO
    // Set up a buffer to read to

    for (unsigned int i = 0; i < inNumFrames; ++i)
      {
      PixelType buffer2[ vxlIO_read->GetImageSizeInBytes() ];
      // Read into the buffer
      vxlIO_read->Read(static_cast<void*>(buffer2));

      // Write out the frame from the buffer
      vxlIO_write->Write(static_cast<void*>(buffer2));
      }


    // Finish writing and reading
    vxlIO_read->FinishReadingOrWriting();
    vxlIO_write->FinishReadingOrWriting();

    std::cout<<"Done !"<<std::endl;
    return ret;
}

int itkVXLVideoIOTest ( int argc, char *argv[] )
{
  if (argc != 9)
    {
    std::cerr << "Usage: [Video Input] [Non-Video Input] [Video Output] [Webcam Output] "
      "[Width] [Height] [Num Frames] [FpS]" << std::endl;
    return EXIT_FAILURE;
    }

  return test_VXLVideoIO(argv[1], argv[2], argv[3], argv[4], atoi(argv[5]), atoi(argv[6]),
                         atoi(argv[7]), atof(argv[8]));
}
