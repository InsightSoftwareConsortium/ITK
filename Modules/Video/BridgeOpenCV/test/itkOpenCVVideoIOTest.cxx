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

#include "itkOpenCVVideoIO.h"
#include "itkImportImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkOpenCVVideoIOFactory.h"

// ITK typedefs
typedef itk::RGBPixel<char>                  PixelType;
typedef itk::ImportImageFilter<PixelType, 2> ImportFilterType;
typedef itk::Image<PixelType, 2>             ImageType;
typedef itk::ImageFileWriter<ImageType>      WriterType;
typedef itk::SizeValueType                   FrameOffsetType;

//
// Utility function to get an ITK image from an void* buffer
//
ImageType::Pointer itkImageFromBuffer( itk::OpenCVVideoIO::Pointer opencvIO, void* buffer, size_t bufferSize )
{
  // Set up for incoming image
  ImageType::RegionType region;
  ImageType::SizeType   size;
  ImageType::IndexType  start;

  size[0] = opencvIO->GetDimensions(0);
  size[1] = opencvIO->GetDimensions(1);
  start.Fill(0);
  region.SetIndex(start);
  region.SetSize(size);
  ImageType::PointType   origin;
  ImageType::SpacingType space;
  origin.Fill(0.0);
  space.Fill(1.0);  // May need fixing

  // Use itkImportImageFilter to create an ITK image
  ImportFilterType::Pointer importFilter = ImportFilterType::New();
  importFilter->SetRegion(region);
  importFilter->SetOrigin(origin);
  importFilter->SetSpacing(space);
  importFilter->SetImportPointer(reinterpret_cast<PixelType *>(buffer), bufferSize, false);
  importFilter->Update();
  ImageType::Pointer frame = importFilter->GetOutput();

  return frame;
}

//
// Utility function for comparing output of Read buffer to OpenCV image
//
// Note: opencvIO should already have called ReadImageInformation
//
bool readCorrectly( itk::OpenCVVideoIO::Pointer opencvIO, CvCapture* capture, FrameOffsetType frameNumber )
{
  bool ret = true;
  // Check meta data
  for( unsigned int i = 0; i < ImageType::ImageDimension; i++ )
    {
    if( opencvIO->GetSpacing(i) != 1.0 )
      {
      std::cerr << "Frame Spacing not set correctly" << std::endl;
      ret = false;
      }
    if( opencvIO->GetOrigin(i) != 0.0 )
      {
      std::cerr << "Frame Origin not set correctly" << std::endl;
      ret = false;
      }
    if( opencvIO->GetDirection(i) != opencvIO->GetDefaultDirection(i) )
      {
      std::cerr << "Frame Direction not set correctly" << std::endl;
      ret = false;
      }
    }

  // Set up the buffer for the frame data
  itk::SizeValueType bufferSize = opencvIO->GetImageSizeInBytes();
  PixelType * buffer = new PixelType[bufferSize];

  // Read the frame data
  opencvIO->Read(static_cast<void *>(buffer) );

  // Open the frame directly with OpenCV
  IplImage* cvFrameBGR = cvQueryFrame(capture);
  IplImage* cvFrameRGB = cvCreateImage(
      cvSize(opencvIO->GetDimensions(0), opencvIO->GetDimensions(1) ),
      IPL_DEPTH_8U, opencvIO->GetNumberOfComponents() );
  cvCvtColor(cvFrameBGR, cvFrameRGB, CV_BGR2RGB);

  // Make sure buffers are same sized
  if( cvFrameRGB->imageSize != (int)bufferSize )
    {
    std::cerr << "Frame buffer sizes don't match. Got: " << bufferSize << ", Expected: "
              << cvFrameRGB->imageSize << std::endl;
    ret = false;
    }

  // Compare buffer contents
  if( memcmp(reinterpret_cast<void *>(buffer), reinterpret_cast<void *>(cvFrameRGB->imageData), bufferSize) )
    {
    std::cerr << "Frame buffers don't match for frame " << frameNumber << std::endl;
    ret = false;
    }

  delete[] buffer;
  // Return
  cvReleaseImage(&cvFrameRGB);
  return ret;
}

//
// Utility function to compare two videos frame by frame
//
bool videosMatch(char* file1, char* file2)
{
  itk::OpenCVVideoIO::Pointer io1 = itk::OpenCVVideoIO::New();
  itk::OpenCVVideoIO::Pointer io2 = itk::OpenCVVideoIO::New();

  io1->SetFileName(file1);
  io2->SetFileName(file2);

  // Make sure files can be read
  if( !io1->CanReadFile(file1) || !io2->CanReadFile(file2) )
    {
    std::cerr << "Cannot read specified files" << std::endl;
    return false;
    }

  // Read in the file information for both
  io1->ReadImageInformation();
  io2->ReadImageInformation();

  // Make sure image info matches
  double e = 0.0001;
  if( io1->GetFrameTotal() != io2->GetFrameTotal() ||
      io1->GetDimensions(0) != io2->GetDimensions(0) ||
      io1->GetDimensions(1) != io2->GetDimensions(1) ||
      io1->GetFramesPerSecond() + e < io2->GetFramesPerSecond() ||
      io1->GetFramesPerSecond() - e > io2->GetFramesPerSecond() ||
      io1->GetNumberOfComponents() != io2->GetNumberOfComponents() )
    {

    std::cerr << "Frame information doesn't match" << std::endl;
    std::cerr << "  FrameTotal: " << io1->GetFrameTotal() << ", " << io2->GetFrameTotal() << std::endl;
    std::cerr << "  Width: " << io1->GetDimensions(0) << ", " << io2->GetDimensions(0)
              << std::endl;
    std::cerr << "  Height: " << io1->GetDimensions(1) << ", " << io2->GetDimensions(1)
              << std::endl;
    std::cerr << "  FpS: " << io1->GetFramesPerSecond() << ", " << io2->GetFramesPerSecond() << std::endl;
    std::cerr << "  NChannels: " << io1->GetNumberOfComponents() << ", " << io2->GetNumberOfComponents() << std::endl;

    return false;
    }

  // Loop through each frame and compare the buffer for exact match
  itk::SizeValueType bufferSize = io1->GetImageSizeInBytes();
  PixelType * buffer1 = new PixelType[bufferSize];
  PixelType * buffer2 = new PixelType[bufferSize];
  for( unsigned int i = 0; i < io1->GetFrameTotal(); ++i )
    {
    io1->Read(reinterpret_cast<void *>(buffer1) );
    io2->Read(reinterpret_cast<void *>(buffer2) );
    if( memcmp(reinterpret_cast<void *>(buffer1), reinterpret_cast<void *>(buffer2), bufferSize) )
      {
      std::cerr << "Frame buffers don't match for frame " << i << std::endl;
      return false;
      }
    }
  delete[] buffer1;
  delete[] buffer2;
  // Close the readers
  io1->FinishReadingOrWriting();
  io2->FinishReadingOrWriting();

  // return result
  return true;
}

//
// This tests all of the functionality of the OpenCVVideoIO
//
// Usage: [Video Input] [Non-Video Input] [Video Output] [Width] [Height]
//            [Num Frames] [FpS]

int test_OpenCVVideoIO( char* input, char* nonVideoInput, char* output, char* cameraOutput,
                        unsigned int inWidth, unsigned int inHeight, FrameOffsetType inNumFrames,
                        double inFpS )
{

  int ret = EXIT_SUCCESS;

  // Create the VideoIO
  itk::OpenCVVideoIO::Pointer opencvIO = itk::OpenCVVideoIO::New();

  //
  // CanReadFile
  //
  std::cout << "OpenCVVideoIO::CanReadFile..." << std::endl;

  // Test CanReadFile on good file
  if( !opencvIO->CanReadFile(input) )
    {
    std::cerr << "Could not read " << input << std::endl;
    ret = EXIT_FAILURE;
    }

  // Test CanReadFile on non-existant file
  std::string nonExistantFile = "Bad/Path/To/Nothing";
  if( opencvIO->CanReadFile(nonExistantFile.c_str() ) )
    {
    std::cerr << "Should have failed to open \"" << nonExistantFile << "\"" << std::endl;
    ret = EXIT_FAILURE;
    }

  // Test CanReadFile on non-video file
  if( opencvIO->CanReadFile(nonVideoInput) )
    {
    std::cerr << "Should have failed to open \"" << nonVideoInput << "\"" << std::endl;
    ret = EXIT_FAILURE;
    }

  //
  // ReadImageInformation
  //
  std::cout << "OpenCVVideoIO::ReadImageInformation..." << std::endl;

  opencvIO->SetFileName(input);
  opencvIO->ReadImageInformation();
  bool              infoSet = true;
  std::stringstream paramMessage;
  if( opencvIO->GetDimensions(0) != inWidth )
    {
    infoSet = false;
    paramMessage << "Width mismatch: (expected) " << inWidth << " != (got) "
                 << opencvIO->GetDimensions(0) << std::endl;
    }
  if( opencvIO->GetDimensions(1) != inHeight )
    {
    infoSet = false;
    paramMessage << "Height mismatch: (expected) " << inHeight << " != (got) "
                 << opencvIO->GetDimensions(1) << std::endl;
    }
  double epsilon = 0.0001;
  if( opencvIO->GetFramesPerSecond() < inFpS - epsilon || opencvIO->GetFramesPerSecond() > inFpS + epsilon )
    {
    infoSet = false;
    paramMessage << "FpS mismatch: (expected) " << inFpS << " != (got) " << opencvIO->GetFramesPerSecond()
                 << std::endl;
    }
  if( opencvIO->GetFrameTotal() != inNumFrames )
    {
    infoSet = false;
    paramMessage << "FrameTotal mismatch: (expected) " << inNumFrames << " != (got) "
                 << opencvIO->GetFrameTotal() << std::endl;
    }

  if( !infoSet )
    {
    std::cerr << paramMessage.str();
    ret = EXIT_FAILURE;
    }

  //
  // Read
  //
  std::cout << "OpenCVVideoIO::Read..." << std::endl;
  std::cout << "Comparing all " << opencvIO->GetFrameTotal() << " frames" << std::endl;

  // Set up OpenCV capture
  CvCapture* capture = cvCaptureFromFile( opencvIO->GetFileName() );
  // Loop through all frames
  for( FrameOffsetType i = 0; i * opencvIO->GetIFrameInterval() < opencvIO->GetFrameTotal(); i++ )
    {
    if( !readCorrectly(opencvIO, capture, i*opencvIO->GetIFrameInterval()) )
      {
      std::cerr << "Failed to read frame " << i*opencvIO->GetIFrameInterval() << " correctly" << std::endl;
      ret = EXIT_FAILURE;
      break;
      }
    }

  // Release capture
  cvReleaseCapture(&capture);

  //
  // SetNextFrameToRead
  //
  std::cout << "OpenCVVideoIO::SetNextFrameToRead" << std::endl;

  // Set up the buffer for the frame data so Read can be called
  itk::SizeValueType bufferSize = opencvIO->GetImageSizeInBytes();
  PixelType * buffer = new PixelType[bufferSize];

  // try seeking to an I-Frame
  // seekFrame is 0-based index of the frame to be captured next
  FrameOffsetType seekFrame = opencvIO->GetIFrameInterval() - 1;
  if( !opencvIO->SetNextFrameToRead(seekFrame) )
    {
    std::cerr << "Failed to seek to second I-Frame..." << std::endl;
    ret = EXIT_FAILURE;
    }

  // Read the frame data which updates the current frame correctly
  opencvIO->Read(static_cast<void *>(buffer) );

  //GetCurrentFrame() returns 0-based index of the frame to be captured next
  if( opencvIO->GetCurrentFrame()-1 != seekFrame )
    {
    std::cerr << "Seek to I-Frame didn't end up in the right place" << std::endl;
    ret = EXIT_FAILURE;
    }

  // If there are I-Frame intervals, check behavior
  if( opencvIO->GetIFrameInterval() > 1 )
    {

    // Try seeking in-between I-Frames
    seekFrame = opencvIO->GetIFrameInterval() / 2;
    if( !opencvIO->SetNextFrameToRead(seekFrame) )
      {
      std::cerr << "Failed to seek between I-Frames" << std::endl;
      ret = EXIT_FAILURE;
      }
    opencvIO->Read(static_cast<void *>(buffer) );
    if( opencvIO->GetCurrentFrame() != opencvIO->GetIFrameInterval() )
      {
      std::cerr << "Seek between I-Frames didn't end up in the right place" << std::endl;
      ret = EXIT_FAILURE;
      }

    delete[] buffer;
    // try seeking past last I-Frame
    seekFrame = opencvIO->GetLastIFrame() + 1;
    if( opencvIO->SetNextFrameToRead(seekFrame) )
      {
      std::cerr << "Did no fail when seeking past the last I-Frame" << std::endl;
      ret = EXIT_FAILURE;
      }

    }

  // Save the current parameters
  double       fps = opencvIO->GetFramesPerSecond();
  unsigned int width = opencvIO->GetDimensions(0);
  unsigned int height = opencvIO->GetDimensions(1);
  const char*  fourCC = "MP42";
  unsigned int nChannels = opencvIO->GetNumberOfComponents();

  // Reset the VideoIO
  opencvIO->FinishReadingOrWriting();

  //
  // Test reading from camera -- If webcam 0 can be opened, it will, otherwise
  // this will be skipped
  //

  // Check to see if camera is available
  if( opencvIO->CanReadCamera( 0 ) )
    {

    std::cout << "OpenCVVideoIO::Read (from camera)..." << std::endl;

    // Set the reader to use the camera
    opencvIO->SetReadFromCamera();

    // Get information from the camera
    try
      {
      opencvIO->ReadImageInformation();
      }
    catch( itk::ExceptionObject & e )
      {
      std::cerr << "Could not read information from the camera" << std::endl;
      std::cerr << "If a camera is present, this test requires it to be on with OpenCV 2" << std::endl;
      std::cerr << e << std::endl;
      return EXIT_FAILURE;
      }

    // Set up buffer for camera
    itk::SizeValueType camBufferSize = opencvIO->GetImageSizeInBytes();
    PixelType * camBuffer = new PixelType[camBufferSize];

    // Read from the camera
    try
      {
      opencvIO->Read(reinterpret_cast<void *>(camBuffer) );
      }
    catch( itk::ExceptionObject & e )
      {
      std::cerr << "Could not read from the camera" << std::endl;
      std::cerr << e << std::endl;
      ret = EXIT_FAILURE;
      }

    // Get an ITK image from the camera's frame
    ImageType::Pointer cameraFrame = itkImageFromBuffer(opencvIO, camBuffer, camBufferSize);
    delete[] camBuffer;

    // Write out the ITK image -- DEBUG
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName(cameraOutput);
    writer->SetInput(cameraFrame);
    writer->Update();

    // Overwrite the file right away so we're not saving pictures of the tester!
    std::ofstream fs;
    fs.open(cameraOutput);
    fs << "EMPTY... deleted picture from webcam\n";
    fs.close();

    // Finish reading
    opencvIO->FinishReadingOrWriting();

    }

  //
  // Test Writing
  //

  //
  // SetWriterParameters
  //
  std::cout << "OpenCVVIdeoIO::SetWriterParameters..." << std::endl;

  // Reset the saved parameters
  std::vector<itk::SizeValueType> size;
  size.push_back(width);
  size.push_back(height);
  opencvIO->SetWriterParameters(fps,
                                size,
                                fourCC,
                                nChannels,
                                itk::ImageIOBase::UCHAR);

  // Make sure they set correctly
  if( opencvIO->GetFramesPerSecond() != fps || opencvIO->GetDimensions(0) != width ||
      opencvIO->GetDimensions(1) != height || opencvIO->GetNumberOfComponents() != nChannels )
    {
    std::cerr << "Didn't set writer parmeters correctly" << std::endl;
    ret = EXIT_FAILURE;
    }

  //
  // CanWriteFile
  //
  std::cout << "OpenCVVideoIO::CanWriteFile..." << std::endl;

  // Test CanWriteFile on good filename
  if( !opencvIO->CanWriteFile(output) )
    {
    std::cerr << "CanWriteFile didn't return true correctly" << std::endl;
    ret = EXIT_FAILURE;
    }

  // Test CanWriteFile on bad filename
  if( opencvIO->CanWriteFile("asdfasdfasdf") )
    {
    std::cerr << "CanWriteFile should have failed for bad filename" << std::endl;
    ret = EXIT_FAILURE;
    }

  //
  // Write
  //
  std::cout << "OpenCVVIdeoIO::Write..." << std::endl;

  // Set output filename
  opencvIO->SetFileName( output );

  // Set up a second VideoIO to read while we're writing
  itk::OpenCVVideoIO::Pointer opencvIO2 = itk::OpenCVVideoIO::New();
  opencvIO2->SetFileName( input );
  opencvIO2->ReadImageInformation();
  // Loop through all frames to read with opencvIO2 and write with opencvIO
  for( unsigned int i = 0; i *opencvIO2->GetIFrameInterval() < inNumFrames; i++ )
    {
    // Set up a buffer to read to
    itk::SizeValueType bufferSizeT = opencvIO2->GetImageSizeInBytes();
    PixelType * bufferT = new PixelType[bufferSizeT];

    // Read into the buffer
    opencvIO2->Read(static_cast<void *>(bufferT) );

    // Write out the frame from the buffer
    opencvIO->Write(static_cast<void *>(bufferT) );
    delete[] bufferT;

    }

  // Finish writing
  opencvIO2->FinishReadingOrWriting();
  opencvIO->FinishReadingOrWriting();

  // DEBUG -- Don't do this for now, need a better comparison method
  // Compare input and output videos to make sure they are identical
  // if (!videosMatch(input, output))
  //  {
  //  std::cerr << "Written video does not match input video" << std::endl;
  //  ret = EXIT_FAILURE;
  //  }

  // DEBUG
  // std::cout << "PIM1 = " << CV_FOURCC('P','I','M','1') << std::endl;
  // std::cout << "MJPG = " << CV_FOURCC('M','J','P','G') << std::endl;
  // std::cout << "MP42 = " << CV_FOURCC('M', 'P', '4', '2') << std::endl;
  // std::cout << "DIV3 = " << CV_FOURCC('D', 'I', 'V', '3') << std::endl;
  // std::cout << "DIVX = " << CV_FOURCC('D', 'I', 'V', 'X') << std::endl;
  // std::cout << "U263 = " << CV_FOURCC('U', '2', '6', '3') << std::endl;
  // std::cout << "I263 = " << CV_FOURCC('I', '2', '6', '3') << std::endl;
  // std::cout << "FLV1 = " << CV_FOURCC('F', 'L', 'V', '1') << std::endl;

  std::cout << "Done !" << std::endl;
  return ret;
}

int itkOpenCVVideoIOTest( int argc, char *argv[] )
{
  if( argc != 9 )
    {
    std::cerr << "Usage: [Video Input] [Non-Video Input] [Video Output] [Webcam Output] "
    "[Width] [Height] [Num Frames] [FpS]" << std::endl;
    return EXIT_FAILURE;
    }

  itk::ObjectFactoryBase::RegisterFactory( itk::OpenCVVideoIOFactory::New() );

  return test_OpenCVVideoIO(argv[1], argv[2], argv[3], argv[4], atoi(argv[5]), atoi(argv[6]),
                            atoi(argv[7]), atof(argv[8]) );
}
