#include <iostream>
#include <sstream>

#include "itkVideoFileReader.h"
#include "itkVideoFileWriter.h"
#include "itkRGBPixel.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkFileListVideoIOFactory.h"

//DEBUG
#include "itkImageFileWriter.h"

int itkVideoFileReaderWriterTest ( int argc, char *argv[] )
{
  if (argc != 3)
    {
    std::cerr << "Usage: [Video Input] [Image Output]" << std::endl;
    return EXIT_FAILURE;
    }


  //
  // Instantiate a new reader
  //
  typedef itk::RGBPixel<unsigned char>                PixelType;
  const unsigned int NumberOfDimensions =             2;
  typedef itk::Image< PixelType, NumberOfDimensions > FrameType;
  typedef itk::VideoStream< FrameType >               VideoType;
  typedef itk::VideoFileReader< VideoType >           VideoReaderType;
  //typedef itk::VideoFileWriter< VideoType >           VideoWriterType;

  VideoReaderType::Pointer reader = VideoReaderType::New();
  reader->SetFileName(argv[1]);

  // I'm still not sure how to handle this right, but for now, just manually
  // register an FileListVideoIO
  itk::ObjectFactoryBase::RegisterFactory( itk::FileListVideoIOFactory::New() );

  //////
  //DEBUG
  //////

  // Loop through each frame, request it and write it out
  typedef itk::ImageFileWriter<FrameType> ImageWriterType;
  ImageWriterType::Pointer imageWriter = ImageWriterType::New();
  for (unsigned long i = 0; i < reader->GetNumberOfFrames(); ++i)
    {
    // Set the requested temporal region
    itk::TemporalRegion request;
    request.SetFrameStart(i);
    request.SetFrameDuration(1);
    reader->GetOutput()->SetRequestedTemporalRegion(request);

    // Update the reader
    reader->Update();

    // Write out the output
    std::stringstream ss;
    ss << "/home/gabe/Desktop/out" << i << ".mha";
    imageWriter->SetFileName(ss.str());
    imageWriter->SetInput(reader->GetOutput()->GetFrame(i));
    imageWriter->Update();
    }


/*
  // Set up a filter to pass the resulting frames through
  typedef itk::RecursiveGaussianImageFilter<ImageType, ImageType> FilterType;
  FilterType::Pointer gaussFilter = FilterType::New();
  gaussFilter->SetSigma(10);

  // Set up video writer
  VideoWriterType::Pointer writer = VideoWriterType::New();
  writer->SetFileName(argv[2]);

  // Connect the pipeline
  gaussFilter->SetInput(reader->GetOutput());
  writer->SetInput(gaussFilter->GetOutput());
  writer->SetFpS(reader->GetFpS());
  writer->SetFourCC("MP42");  // For now, just set manually

  // Loop through all of the frames and write them out
  for (unsigned int i = 0; i < reader->GetNumberOfFrames(); ++i)
    {

    // Mark the filter modified so it is out of date
    gaussFilter->Modified();

    // Write the frame
    writer->Update();
    }

  // Finish writing the video
  writer->FinishWriting();

*/
  return EXIT_SUCCESS;
}
