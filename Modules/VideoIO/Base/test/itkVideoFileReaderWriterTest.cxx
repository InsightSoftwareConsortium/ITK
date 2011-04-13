#include <iostream>
#include <sstream>

#include "itkVideoFileReader.h"
#include "itkVideoFileWriter.h"
#include "itkRGBPixel.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkFileListVideoIOFactory.h"


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
  typedef itk::Image< PixelType, NumberOfDimensions > ImageType;
  typedef itk::VideoFileReader< ImageType >           VideoReaderType;
  typedef itk::VideoFileWriter< ImageType >           VideoWriterType;

  VideoReaderType::Pointer reader = VideoReaderType::New();
  reader->SetFileName(argv[1]);

  // I'm still not sure how to handle this right, but for now, just manually
  // register an FileListVideoIO
  itk::ObjectFactoryBase::RegisterFactory( itk::FileListVideoIOFactory::New() );

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


  return EXIT_SUCCESS;
}
