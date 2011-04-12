#include <iostream>

#include "itkRingBufferImageSet.h"
#include "itkFileListVideoIO.h"
#include "itkRGBPixel.h"
#include "itkImageFileWriter.h"


int itkRingBufferImageSetTest ( int argc, char *argv[] )
{

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
  // Set up Ring Buffer
  //////
  typedef itk::RGBPixel< unsigned char >     PixelType;
  const unsigned int Dimension =             2;
  const unsigned int NumberOfBuffers =       3;
  typedef itk::Image< PixelType, Dimension > ImageType;

  typedef itk::RingBufferImageSet< PixelType, Dimension, NumberOfBuffers >
    RingBufferImageSetType;

  typedef itk::RingBufferImageSet< float, Dimension, NumberOfBuffers >
    FloatRingBufferImageSetType;

  RingBufferImageSetType::Pointer ringBuffer = RingBufferImageSetType::New();

  // Test Allocate failure with no VideoIO
  try
    {
    ringBuffer->Allocate();
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
    ringBuffer->Allocate();
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
  ringBuffer->SetVideoIO(filelistIO);
  try
    {
    ringBuffer->Allocate();
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
    ringBuffer->MoveHeadForward();
    ringBuffer->BufferNextImage();

    // Make sure current frame is correct
    if (filelistIO->GetCurrentFrame() != i)
      {
      std::cerr << "RingBuffer not accurately advancing frames" << std::endl;
      return EXIT_FAILURE;
      }

    // Check buffer validity and test writing output (not checking output values)
    for (int j = 0; j < (int)NumberOfBuffers; ++j)
      {
      if (i < NumberOfBuffers && j > (int)i)
        {
        if (ringBuffer->BufferIsValid(-j))
          {
          std::cerr << "Incorrectly reported valid buffer" << std::endl;
          return EXIT_FAILURE;
          }
        }
      else if (!ringBuffer->BufferIsValid(-j))
        {
        std::cerr << "Incorrectly reported invalid buffer" << std::endl;
        return EXIT_FAILURE;
        }
      if (ringBuffer->BufferIsValid(-j))
        {
        writer->SetFileName(argv[2]);
        writer->SetInput(ringBuffer->GetBufferedImage(-j));
        writer->Update();
        }
      }
    }


  //////
  // Test advancing using Update()
  //////

  // Seek back to the beginning
  filelistIO->SetNextFrameToRead(0);

  for (unsigned int i = 0; i < 5; ++i)
    {
    writer->SetFileName(argv[2]);
    writer->SetInput(ringBuffer->GetOutput());
    writer->Update();

    // Make sure the current frame got updated
    if (filelistIO->GetCurrentFrame() != i)
      {
      std::cerr << "RingBuffer not accurately advancing frames when updating" << std::endl;
      return EXIT_FAILURE;
      }
    }


  //////
  // Test handling of different pixel types
  //////

  // Test Allocate with valid file information and mismatched pixel type
  FloatRingBufferImageSetType::Pointer floatRingBuffer =
    FloatRingBufferImageSetType::New();
  itk::FileListVideoIO::Pointer filelistIO2 = itk::FileListVideoIO::New();
  filelistIO2->SetFileName(argv[1]);
  filelistIO2->ReadImageInformation();
  floatRingBuffer->SetVideoIO(filelistIO2);
  try
    {
    floatRingBuffer->Allocate();
    }
  catch (itk::ExceptionObject e)
    {
    std::cerr << "Failed to allocate with float pixel type" << std::endl;
    return EXIT_FAILURE;
    }

  // Try reading an image as float
  floatRingBuffer->MoveHeadForward();
  floatRingBuffer->BufferNextImage();
  typedef itk::Image<float, Dimension> FloatImageType;
  typedef itk::ImageFileWriter<FloatImageType> FloatWriterType;
  FloatWriterType::Pointer fwriter = FloatWriterType::New();
  fwriter->SetFileName(argv[3]);
  fwriter->SetInput(floatRingBuffer->GetBufferedImage(0));
  fwriter->Update();



  return EXIT_SUCCESS;
}
