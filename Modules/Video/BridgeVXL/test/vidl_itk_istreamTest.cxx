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

#include "vidl_itk_istream.hxx"
#include "itkVideoFileReader.h"
#include "itkVXLVideoIOFactory.h"
#include "vidl/vidl_ffmpeg_ostream.h"


//
// Helper function to test pixel type
//
template<typename TPixelType>
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
// Templated test
//
template<typename TPixelType>
int vidl_itk_istreamTestWithPixelType(char* argv[], vidl_pixel_format expectedFormat)
{
  // typedefs
  typedef TPixelType                        PixelType;
  typedef itk::Image<PixelType, 2>          FrameType;
  typedef itk::VideoStream<FrameType>       VideoType;
  typedef itk::vidl_itk_istream<VideoType>  StreamType;
  typedef itk::VideoFileReader<VideoType>   ReaderType;

  // Test the pixel format
  if (!TestFormat<PixelType>(expectedFormat))
    {
    std::cerr << "format() did not return expected result for pixel type "
              << typeid(PixelType).name() << std::endl;
      return EXIT_FAILURE;
    }

  // Set up VideoFileReader
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Set up new istream and connect it
  StreamType* istream = new StreamType();
  istream->open(reader->GetOutput());

  // Check width and height
  unsigned int width = istream->width();
  unsigned int height = istream->height();
  if (width != static_cast<unsigned int>(atoi(argv[3])) ||
      height != static_cast<unsigned int>(atoi(argv[4])))
    {
    std::cerr << "(px: " << typeid(PixelType).name()
              << ") dimensions not reporting correctly. Got [" << width << "," << height
              << "] Expected [" << atoi(argv[3]) << "," << atoi(argv[4]) << "]" << std::endl;
    delete istream;
    return EXIT_FAILURE;
    }

  // Set up vidl_ffmpeg_ostream
  vidl_ffmpeg_ostream_params parameters;
  parameters.frame_rate_ = 24;
  parameters.ni_ = istream->width();
  parameters.nj_ = istream->height();
  parameters.encoder_ = vidl_ffmpeg_ostream_params::MSMPEG4V2;
  vidl_ffmpeg_ostream* ostream = new vidl_ffmpeg_ostream(argv[2], parameters);

  // Read the entire video and write it back out
  bool keepReading = true;
  while(keepReading)
    {
    vidl_frame_sptr outFrame = istream->read_frame();
    if (outFrame)
      {
      ostream->write_frame(outFrame);
      }
    else
      {
      keepReading = false;
      }
    }

  // Return success
  delete istream;
  //delete ostream;   //BUG?  Can not close ostream
  return EXIT_SUCCESS;
}

#define TemplatedTestMacro(PixelType, expectedFormat)                                         \
  if ( vidl_itk_istreamTestWithPixelType<PixelType>( argv, expectedFormat ) == EXIT_FAILURE ) \
    {                                                                                         \
    return EXIT_FAILURE;                                                                      \
    }

//
// Main test body
//
int vidl_itk_istreamTest ( int argc, char *argv[] )
{
  //
  // Check parameters
  //
  if (argc < 5)
    {
    std::cerr << "Usage: " << argv[0] << " input_file output_file width height" << std::endl;
    return EXIT_FAILURE;
    }


  // Register a VXLVideoIO. This should be fixed eventually
  itk::ObjectFactoryBase::RegisterFactory( itk::VXLVideoIOFactory::New() );

  //
  // Test all supported pixel formats
  //

  // Scalar types
  TemplatedTestMacro(bool, VIDL_PIXEL_FORMAT_MONO_1);
  TemplatedTestMacro(char, VIDL_PIXEL_FORMAT_MONO_8);
  TemplatedTestMacro(unsigned char, VIDL_PIXEL_FORMAT_MONO_8);
  TemplatedTestMacro(short, VIDL_PIXEL_FORMAT_MONO_16);
  TemplatedTestMacro(unsigned short, VIDL_PIXEL_FORMAT_MONO_16);
  TemplatedTestMacro(float, VIDL_PIXEL_FORMAT_MONO_F32);

  // RGB(A) types
  TemplatedTestMacro(itk::RGBPixel<char>, VIDL_PIXEL_FORMAT_RGB_24);
  TemplatedTestMacro(itk::RGBPixel<unsigned char>, VIDL_PIXEL_FORMAT_RGB_24);
  TemplatedTestMacro(itk::RGBAPixel<char>, VIDL_PIXEL_FORMAT_RGBA_32);
  TemplatedTestMacro(itk::RGBAPixel<unsigned char>, VIDL_PIXEL_FORMAT_RGBA_32);
  TemplatedTestMacro(itk::RGBPixel<float>, VIDL_PIXEL_FORMAT_RGB_F32);

  //
  // Test format returned for unsupported types
  //
  TestFormatMacro(int, VIDL_PIXEL_FORMAT_UNKNOWN);
  TestFormatMacro(unsigned int, VIDL_PIXEL_FORMAT_UNKNOWN);
  TestFormatMacro(double, VIDL_PIXEL_FORMAT_UNKNOWN);
  TestFormatMacro(itk::RGBPixel<int>, VIDL_PIXEL_FORMAT_UNKNOWN);

  return EXIT_SUCCESS;
}
