#include <iostream>

#include "itkOpenCVVideoCapture.h"
#include "itkImportImageFilter.h"
#include "itkRGBPixel.h"
#include "itkImageFileWriter.h"


// ITK typedefs
typedef itk::RGBPixel<char> PixelType;
typedef itk::ImportImageFilter<PixelType, 2> ImportFilterType;
typedef itk::Image<PixelType, 2> ImageType;
typedef itk::ImageFileWriter<ImageType> WriterType;

int itkOpenCVVideoCaptureTest ( int argc, char *argv[] )
{
  std::cout << "STUB" << std::endl;
  return EXIT_SUCCESS;
}
