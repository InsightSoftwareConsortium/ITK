#include <iostream>

#include "itkVideoImageSet.h"
#include "itkFileListVideoIO.h"
#include "itkRGBPixel.h"
#include "itkImageFileWriter.h"


//DEBUG
#include "itkTemporalRegion.h"
#include "itkImageRegion.h"

/**
 * This test is basically a duplicate of RingBufferImageSetTest with a few
 * additions to test the video specific methods
 */
int itkVideoImageSetTest ( int argc, char *argv[] )
{
  // Create TemporalRegion
  itk::TemporalRegion tRegion;
  tRegion.SetFrameStart(4);
  tRegion.SetFrameDuration(10);





/*
  //////
  // Check arguments
  //////
  if (argc != 4)
    {
    std::cout << "Usage: " << argv[0] << " [input file] [char output file] [float output file]"
      << std::endl;
    return EXIT_FAILURE;
    }


  //////
  // Set up Video
  //////
  typedef itk::RGBPixel< unsigned char >     PixelType;
  const unsigned int Dimension =             2;
  const unsigned int NumberOfBuffers =       3;
  typedef itk::Image< PixelType, Dimension > ImageType;

  typedef itk::VideoImageSet< PixelType, Dimension, NumberOfBuffers >
    VideoImageSetType;

  typedef itk::VideoImageSet< float, Dimension, NumberOfBuffers >
    FloatVideoImageSetType;

  VideoImageSetType::Pointer video = VideoImageSetType::New();

  // Test Allocate failure with no VideoIO
  try
    {
    video->Allocate();
    std::cerr << "Did not fail to allocate with no VideoIO set" << std::endl;
    return EXIT_FAILURE;
    }
  catch (itk::ExceptionObject e) {}


  //////
  // Set up VideoIO and test Allocate
  //////
  itk::FileListVideoIO::Pointer filelistIO = itk::FileListVideoIO::New();

  // Test Allocate failure with closed VideoIO
  try
    {
    video->Allocate();
    std::cerr << "Did not fail to allocate with closed VideoIO set" << std::endl;
    return EXIT_FAILURE;
    }
  catch (itk::ExceptionObject e) {}

  // Make sure the file given can be read
  if (!filelistIO->CanReadFile(argv[1]))
    {
    std::cerr << "Cannot read file: " << argv[1] << std::endl;
    return EXIT_FAILURE;
    }

  // Get frame information for file
  filelistIO->SetFileName(argv[1]);
  filelistIO->ReadImageInformation();

  // Test Allocate with valid file information and matching pixels
  video->SetVideoIO(filelistIO);
  try
    {
    video->Allocate();
    }
  catch (itk::ExceptionObject e)
    {
    std::cerr << "Failed to allocate with valid input" << std::endl;
    return EXIT_FAILURE;
    }

  //////
  // Test Buffering 5 frames
  //////
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  for (unsigned int i = 0; i < 5; ++i)
    {
    video->MoveHeadForward();
    video->BufferNextImage();

    // Make sure current frame is correct
    if (filelistIO->GetCurrentFrame() != i)
      {
      std::cerr << "Video not accurately advancing frames" << std::endl;
      return EXIT_FAILURE;
      }

    // Test GetCurrentPositionFrame
    if (video->GetCurrentPositionFrame() != filelistIO->GetCurrentFrame())
      {
      std::cerr << "Not reporting current frame correctly" << std::endl;
      return EXIT_FAILURE;
      }

    // Test GetNumberOfFrames
    if (video->GetNumberOfFrames() != filelistIO->GetFrameTotal())
      {
      std::cerr << "Not reporting number of frames correctly" << std::endl;
      return EXIT_FAILURE;
      }

    // Test GetFpS
    if (video->GetFpS() != filelistIO->GetFpS())
      {
      std::cerr << "Not reporting FpS correctly" << std::endl;
      return EXIT_FAILURE;
      }

    // Test GetCurrentPositionRatio
    double ratio = (double)(video->GetCurrentPositionFrame()) /
                    (double)(video->GetNumberOfFrames()-1);
    double eps = .0001;
    if (video->GetCurrentPositionRatio() > ratio + eps ||
        video->GetCurrentPositionRatio() < ratio - eps)
      {
      std::cerr << "Not reporting ratio correctly. (expected)" << ratio << " != (got)"
        << video->GetCurrentPositionRatio() << std::endl;
      return EXIT_FAILURE;
      }

    // Test GetCurrentPositionMSec
    double MSpF = 1.0/video->GetFpS() * 1000;
    double msec = (double)(video->GetCurrentPositionFrame()) * MSpF;
    if (video->GetCurrentPositionMSec() > msec + eps ||
        video->GetCurrentPositionMSec() < msec - eps)
      {
      std::cerr << "Not reporting MSec correctly. (expected)" << msec << " != (got)"
        << video->GetCurrentPositionMSec() << std::endl;
      return EXIT_FAILURE;
      }

    // Check buffer validity and test writing output (not checking output values)
    for (int j = 0; j < (int)NumberOfBuffers; ++j)
      {
      if (i < NumberOfBuffers && j > (int)i)
        {
        if (video->BufferIsValid(-j))
          {
          std::cerr << "Incorrectly reported valid buffer" << std::endl;
          return EXIT_FAILURE;
          }
        }
      else if (!video->BufferIsValid(-j))
        {
        std::cerr << "Incorrectly reported invalid buffer" << std::endl;
        return EXIT_FAILURE;
        }
      if (video->BufferIsValid(-j))
        {
        writer->SetFileName(argv[2]);
        writer->SetInput(video->GetBufferedImage(-j));
        writer->Update();
        }
      }
    }


  //////
  // Test handling of different pixel types
  //////

  // Test Allocate with valid file information and mismatched pixel type
  FloatVideoImageSetType::Pointer floatVideo =
    FloatVideoImageSetType::New();
  itk::FileListVideoIO::Pointer filelistIO2 = itk::FileListVideoIO::New();
  filelistIO2->SetFileName(argv[1]);
  filelistIO2->ReadImageInformation();
  floatVideo->SetVideoIO(filelistIO2);
  try
    {
    floatVideo->Allocate();
    }
  catch (itk::ExceptionObject e)
    {
    std::cerr << "Failed to allocate with float pixel type" << std::endl;
    return EXIT_FAILURE;
    }

  // Try reading an image as float
  floatVideo->MoveHeadForward();
  floatVideo->BufferNextImage();
  typedef itk::Image<float, Dimension> FloatImageType;
  typedef itk::ImageFileWriter<FloatImageType> FloatWriterType;
  FloatWriterType::Pointer fwriter = FloatWriterType::New();
  fwriter->SetFileName(argv[3]);
  fwriter->SetInput(floatVideo->GetBufferedImage(0));
  fwriter->Update();

*/

  return EXIT_SUCCESS;
}
