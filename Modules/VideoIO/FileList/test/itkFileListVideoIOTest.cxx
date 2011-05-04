#include <iostream>
#include <fstream>

#include "itkFileListVideoIO.h"
#include "itkImportImageFilter.h"
#include "itkRGBPixel.h"
#include "itkImageFileWriter.h"


// ITK typedefs
typedef itk::RGBPixel<char> PixelType;
typedef itk::ImportImageFilter<PixelType, 2> ImportFilterType;
typedef itk::Image<PixelType, 2> ImageType;
typedef itk::ImageFileWriter<ImageType> WriterType;



///////////////////////////////////////////////////////////////////////////////
// This tests all of the functionality of the FileListVideoIO
//
// Usage: [Video Input] [Non-Video Input] [Video Output] [Width] [Height]
//            [Num Frames] [FpS]

int test_FileListVideoIO ( char* input, char* nonVideoInput, char* output, char* cameraOutput,
                         unsigned int inWidth, unsigned int inHeight, unsigned long inNumFrames,
                         double inFpS )
{

  int ret = EXIT_SUCCESS;

  // Create the VideoIO
  itk::FileListVideoIO::Pointer fileListIO = itk::FileListVideoIO::New();


  //////
  // SetFileName
  //////

  fileListIO->SetFileName(input);


  //////
  // CanReadFile
  //////
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

  //////
  // ReadImageInformation
  //////
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
  if (fileListIO->GetFpS() < inFpS - epsilon || fileListIO->GetFpS() > inFpS + epsilon)
    {
    infoSet = false;
    paramMessage << "FpS mismatch: (expected) " << inFpS << " != (got) " << fileListIO->GetFpS()
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


  //////
  // Read
  //////
  std::cout << "FileListVideoIO::Read..." << std::endl;
  std::cout << "Comparing all " << fileListIO->GetFrameTotal() << " frames" << std::endl;

  // Set up FileList capture
  CvCapture* capture = cvCaptureFromFile( fileListIO->GetFileName() );

  // Loop through all frames
  for (unsigned long i = 0; i < fileListIO->GetFrameTotal(); ++i)
    {
    if (!readCorrectly(fileListIO, capture, i))
      {
      std::cerr << "Failed to read frame " << i << " correctly" << std::endl;
      ret = EXIT_FAILURE;
      break;
      }
    }

  // Release capture
  cvReleaseCapture(&capture);


  //////
  // SetNextFrameToRead
  //////
  std::cout << "FileListVideoIO::SetNextFrameToRead" << std::endl;

  // Set up the buffer for the frame data so Read can be called
  size_t bufferSize = fileListIO->GetImageSizeInBytes();
  PixelType buffer[bufferSize];


  // try seeking to an I-Frame
  unsigned long seekFrame = fileListIO->GetIFrameInterval();
  if (!fileListIO->SetNextFrameToRead(seekFrame))
    {
    std::cerr << "Failed to seek to second I-Frame..." << std::endl;
    ret = EXIT_FAILURE;
    }

  // Read the frame data which updates the current frame correctly
  fileListIO->Read(static_cast<void*>(buffer));

  if (fileListIO->GetCurrentFrame() != seekFrame)
    {
    std::cerr << "Seek to I-Frame didn't end up in the right place" << std::endl;
    ret = EXIT_FAILURE;
    }


  // If there are I-Frame intervals, check behavior
  if (fileListIO->GetIFrameInterval() > 1)
    {

    // try seeking in-between I-Frames
    seekFrame = fileListIO->GetIFrameInterval()/2;
    if (!fileListIO->SetNextFrameToRead(seekFrame))
      {
      std::cerr << "Failed to seek between I-Frames" << std::endl;
      ret = EXIT_FAILURE;
      }
    fileListIO->Read(static_cast<void*>(buffer));
    if (fileListIO->GetCurrentFrame() != fileListIO->GetIFrameInterval())
      {
      std::cerr << "Seek between I-Frames didn't end up in the right place" << std::endl;
      ret = EXIT_FAILURE;
      }

    // try seeking past last I-Frame
    seekFrame = fileListIO->GetLastIFrame() + 1;
    if (fileListIO->SetNextFrameToRead(seekFrame))
      {
      std::cerr << "Did no fail when seeking past the last I-Frame" << std::endl;
      ret = EXIT_FAILURE;
      }

    }

  // Save the current parameters
  double fps = fileListIO->GetFpS();
  unsigned int width = fileListIO->GetDimensions(0);
  unsigned int height = fileListIO->GetDimensions(1);
  const char* fourCC = "MP42";
  unsigned int nChannels = fileListIO->GetNumberOfComponents();

  // Reset the VideoIO
  fileListIO->FinishReadingOrWriting();


  //////
  // Test reading from camera -- If webcam 0 can be opened, it will, otherwise this will be skipped
  //////

  // Check to see if camera is available
  if (fileListIO->CanReadCamera( 0 ))
    {

    std::cout << "FileListVideoIO::Read (from camera)..." << std::endl;

    // Set the reader to use the camera
    fileListIO->SetReadFromCamera();

    // Get information from the camera
    try
      {
      fileListIO->ReadImageInformation();
      }
    catch (itk::ExceptionObject e)
      {
      std::cerr << "Could not read information from the camera" << std::endl;
      ret = EXIT_FAILURE;
      }

    // set up buffer for camera
    size_t camBufferSize = fileListIO->GetImageSizeInBytes();
    PixelType camBuffer[camBufferSize];

    // Read from the camera
    try
      {
      fileListIO->Read(reinterpret_cast<void*>(camBuffer));
      }
    catch (itk::ExceptionObject e)
      {
      std::cerr << "Could not read from the camera" << std::endl;
      ret = EXIT_FAILURE;
      }

    // Get an ITK image from the camera's frame
    ImageType::Pointer cameraFrame = itkImageFromBuffer(fileListIO, camBuffer, camBufferSize);

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
    fileListIO->FinishReadingOrWriting();

    }


  /////////////////////////////////////////////////////////////////////////////
  // Test Writing
  //


  //////
  // SetWriterParameters
  //////
  std::cout << "FileListVIdeoIO::SetWriterParameters..." << std::endl;

  // Reset the saved parameters
  std::vector<unsigned int> size;
  size.push_back(width);
  size.push_back(height);
  fileListIO->SetWriterParameters(fps, size, fourCC, nChannels, itk::ImageIOBase::UCHAR);

  // Make sure they set correctly
  if (fileListIO->GetFpS() != fps || fileListIO->GetDimensions(0) != width ||
      fileListIO->GetDimensions(1) != height || fileListIO->GetNumberOfComponents() != nChannels)
    {
    std::cerr << "Didn't set writer parmeters correctly" << std::endl;
    ret = EXIT_FAILURE;
    }

  //////
  // CanWriteFile
  //////
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


  //////
  // Write
  //////
  std::cout << "FileListVIdeoIO::Write..." << std::endl;

  // Set output filename
  fileListIO->SetFileName( output );

  // Set up a second VideoIO to read while we're writing
  itk::FileListVideoIO::Pointer fileListIO2 = itk::FileListVideoIO::New();
  fileListIO2->SetFileName( input );
  fileListIO2->ReadImageInformation();

  // Loop through all frames to read with fileListIO2 and write with fileListIO
  for (unsigned int i = 0; i < inNumFrames; ++i)
    {

    // Set up a buffer to read to
    size_t bufferSize = fileListIO2->GetImageSizeInBytes();
    PixelType buffer[bufferSize];

    // Read into the buffer
    fileListIO2->Read(static_cast<void*>(buffer));

    // Write out the frame from the buffer
    fileListIO->Write(static_cast<void*>(buffer));

    }

  // Finish writing
  fileListIO2->FinishReadingOrWriting();
  fileListIO->FinishReadingOrWriting();


  //DEBUG -- Don't do this for now, need a better comparison method
  // Compare input and output videos to make sure they are identical
  //if (!videosMatch(input, output))
  //  {
  //  std::cerr << "Written video does not match input video" << std::endl;
  //  ret = EXIT_FAILURE;
  //  }


  //DEBUG
  //std::cout << "PIM1 = " << CV_FOURCC('P','I','M','1') << std::endl;
  //std::cout << "MJPG = " << CV_FOURCC('M','J','P','G') << std::endl;
  //std::cout << "MP42 = " << CV_FOURCC('M', 'P', '4', '2') << std::endl;
  //std::cout << "DIV3 = " << CV_FOURCC('D', 'I', 'V', '3') << std::endl;
  //std::cout << "DIVX = " << CV_FOURCC('D', 'I', 'V', 'X') << std::endl;
  //std::cout << "U263 = " << CV_FOURCC('U', '2', '6', '3') << std::endl;
  //std::cout << "I263 = " << CV_FOURCC('I', '2', '6', '3') << std::endl;
  //std::cout << "FLV1 = " << CV_FOURCC('F', 'L', 'V', '1') << std::endl;



  std::cout<<"Done !"<<std::endl;
  return ret;
}

int itkFileListVideoIOTest ( int argc, char *argv[] )
{
  if (argc != 9)
    {
    std::cerr << "Usage: [Video Input] [Non-Video Input] [Video Output] [Webcam Output] "
      "[Width] [Height] [Num Frames] [FpS]" << std::endl;
    return EXIT_FAILURE;
    }

  return test_FileListVideoIO(argv[1], argv[2], argv[3], argv[4], atoi(argv[5]), atoi(argv[6]),
                            atoi(argv[7]), atof(argv[8]));
}

