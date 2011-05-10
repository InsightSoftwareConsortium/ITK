#include <iostream>

#include "vidl_itk_istream.h"
#include "itkRGBPixel.h"
#include "itkVideoStream.h"
#include "itkVideoFileReader.h"
#include "itkVXLVideoIOFactory.h"


// ITK typedefs
typedef unsigned char                                 ScalarPixelType;
typedef itk::Image<ScalarPixelType, 2>                ScalarFrameType;
typedef itk::VideoStream< ScalarFrameType >           ScalarVideoStreamType;
typedef itk::VideoFileReader< ScalarVideoStreamType > scalarReaderType;
typedef itk::RGBPixel<unsigned char>                  RGBPixelType;
typedef itk::Image<RGBPixelType, 2>                   RGBFrameType;
typedef itk::VideoStream< RGBFrameType >              RGBVideoStreamType;
typedef itk::VideoFileReader< RGBVideoStreamType >    rgbReaderType;

int vidl_itk_istreamTest ( int argc, char *argv[] )
{
  //
  // Set up a new vidl_itk_istream
  //
  typedef itk::vidl_itk_istream< ScalarVideoStreamType > vidl_itk_istream_type;
  vidl_itk_istream_type* scalar_stream = new vidl_itk_istream_type();

  // Clean up
  delete scalar_stream;

  return EXIT_SUCCESS;
}
