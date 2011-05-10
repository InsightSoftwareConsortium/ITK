#include <iostream>

#include "vidl_itk_istream.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
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


//
// Helper function to test pixel type
//
template<class TPixelType>
bool TestFormat(vidl_pixel_format expectedFormat)
{
  typedef TPixelType                        PixelType;
  typedef itk::Image<PixelType, 2>          FrameType;
  typedef itk::VideoStream<FrameType>       VideoType;
  typedef itk::vidl_itk_istream<VideoType>  StreamType;

  StreamType* stream = new StreamType();
  bool out = (stream->format() == expectedFormat);
  delete stream;
  return out;
}

#define TestFormatMacro(PixelType, expectedFormat)                          \
  if (!TestFormat<PixelType>(expectedFormat))                               \
    {                                                                       \
    std::cerr << "format() did not return expected result for pixel type "  \
              << typeid(PixelType).name() << std::endl;                     \
      return EXIT_FAILURE;                                                  \
    }


//
// Main test body
//
int vidl_itk_istreamTest ( int argc, char *argv[] )
{
  //
  // Test supported pixel formats
  //

  // Scalar types
  TestFormatMacro(bool, VIDL_PIXEL_FORMAT_MONO_1);
  TestFormatMacro(char, VIDL_PIXEL_FORMAT_MONO_8);
  TestFormatMacro(unsigned char, VIDL_PIXEL_FORMAT_MONO_8);
  TestFormatMacro(short, VIDL_PIXEL_FORMAT_MONO_16);
  TestFormatMacro(unsigned short, VIDL_PIXEL_FORMAT_MONO_16);
  TestFormatMacro(float, VIDL_PIXEL_FORMAT_MONO_F32);

  // RGB(A) types
  TestFormatMacro(itk::RGBPixel<char>, VIDL_PIXEL_FORMAT_RGB_24);
  TestFormatMacro(itk::RGBPixel<unsigned char>, VIDL_PIXEL_FORMAT_RGB_24);
  TestFormatMacro(itk::RGBAPixel<char>, VIDL_PIXEL_FORMAT_RGBA_32);
  TestFormatMacro(itk::RGBAPixel<unsigned char>, VIDL_PIXEL_FORMAT_RGBA_32);
  TestFormatMacro(itk::RGBPixel<float>, VIDL_PIXEL_FORMAT_RGB_F32);

  // Unsupported types
  TestFormatMacro(int, VIDL_PIXEL_FORMAT_UNKNOWN);
  TestFormatMacro(unsigned int, VIDL_PIXEL_FORMAT_UNKNOWN);
  TestFormatMacro(double, VIDL_PIXEL_FORMAT_UNKNOWN);
  TestFormatMacro(itk::RGBPixel<int>, VIDL_PIXEL_FORMAT_UNKNOWN);

  //
  // Set up a new vidl_itk_istream
  //
  typedef itk::vidl_itk_istream< ScalarVideoStreamType > vidl_itk_istream_type;
  vidl_itk_istream_type* scalar_stream = new vidl_itk_istream_type();

  // Clean up
  delete scalar_stream;

  return EXIT_SUCCESS;
}
