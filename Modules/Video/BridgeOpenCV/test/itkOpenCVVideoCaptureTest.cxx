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

#include "itkOpenCVVideoCapture.h"
#include "itkVideoFileReader.h"
#include "itkOpenCVVideoIOFactory.h"

#include "opencv2/core/version.hpp"
#if !defined( CV_VERSION_EPOCH )
// OpenCV 3.x
#include "opencv2/videoio/videoio_c.h"
#include "opencv2/imgproc/imgproc_c.h" // cvCvtColor, CV_RGB2BGR, ...
#endif

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
// Main test
//
int itkOpenCVVideoCaptureTest ( int argc, char *argv[] )
{
  //
  // Check arguments
  //
  if (argc != 6)
    {
    std::cerr << "Usage: " << argv[0] << " input_video scalar_output_video RGB_output_video "
              << "width height" << std::endl;
    return EXIT_FAILURE;
    }

  itk::ObjectFactoryBase::RegisterFactory( itk::OpenCVVideoIOFactory::New() );

  //
  // Test with scalars
  //

  // Set up an itk reader
  itk::ObjectFactoryBase::RegisterFactory( itk::OpenCVVideoIOFactory::New() );
  scalarReaderType::Pointer scalarReader = scalarReaderType::New();
  scalarReader->SetFileName(argv[1]);

  // Set up OpenCVVideoCapture
  typedef itk::OpenCVVideoCapture<ScalarVideoStreamType> ScalarCaptureType;
  ScalarCaptureType* scalarCap = new ScalarCaptureType();
  scalarCap->open(scalarReader->GetOutput());

  // Check FourCC
  scalarCap->set(CV_CAP_PROP_FOURCC, CV_FOURCC('M','P','4','2'));
  if ((int)scalarCap->get(CV_CAP_PROP_FOURCC) != CV_FOURCC('M','P','4','2'))
    {
    std::cerr << "FourCC not reporting correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // Check FpS
  double eps = 0.0001;
  if (scalarCap->get(CV_CAP_PROP_FPS) > 24 + eps || scalarCap->get(CV_CAP_PROP_FPS) < 24 - eps)
    {
    std::cerr << "FpS not reporting correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // Check width and height
  if ((int)(scalarCap->get(CV_CAP_PROP_FRAME_WIDTH)) != atoi(argv[4]) ||
      (int)(scalarCap->get(CV_CAP_PROP_FRAME_HEIGHT)) != atoi(argv[5]))
    {
    std::cerr << "Frame dimensions not reporting correctly. Got ["
              << scalarCap->get(CV_CAP_PROP_FRAME_WIDTH) << ","
              << scalarCap->get(CV_CAP_PROP_FRAME_HEIGHT) << "] Expected: ["
              << atoi(argv[4]) << "," << atoi(argv[5]) << "]" << std::endl;
    return EXIT_FAILURE;
    }

  // Set up OpenCV VideoWriter
  cv::VideoWriter scalarWriter(argv[2],
                               (int)scalarCap->get(CV_CAP_PROP_FOURCC),
                               scalarCap->get(CV_CAP_PROP_FPS),
                               cv::Size(scalarCap->get(CV_CAP_PROP_FRAME_WIDTH),
                                        scalarCap->get(CV_CAP_PROP_FRAME_HEIGHT)),
                               false);

  // Loop through the frames and write
  cv::Mat outFrame;
  while (scalarCap->read(outFrame))
    {
    scalarWriter << outFrame;
    }

  // Clean up
  delete scalarCap;

  //
  // Test with RGB
  //

  // Set up an itk reader
  itk::ObjectFactoryBase::RegisterFactory( itk::OpenCVVideoIOFactory::New() );
  rgbReaderType::Pointer rgbReader = rgbReaderType::New();
  rgbReader->SetFileName(argv[1]);

  // Set up OpenCVVideoCapture
  typedef itk::OpenCVVideoCapture<RGBVideoStreamType> RGBCaptureType;
  RGBCaptureType* rgbCap = new RGBCaptureType();
  rgbCap->open(rgbReader->GetOutput());

  // Check FourCC
  if ((int)rgbCap->get(CV_CAP_PROP_FOURCC) != CV_FOURCC('M', 'P', '4', '2'))
    {
    std::cerr << "FourCC not reporting correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // Check FpS
  if (rgbCap->get(CV_CAP_PROP_FPS) > 24 + eps || rgbCap->get(CV_CAP_PROP_FPS) < 24 - eps)
    {
    std::cerr << "FpS not reporting correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // Set up OpenCV VideoWriter
  cv::VideoWriter rgbWriter(argv[3],
                               (int)rgbCap->get(CV_CAP_PROP_FOURCC),
                               rgbCap->get(CV_CAP_PROP_FPS),
                               cv::Size(rgbCap->get(CV_CAP_PROP_FRAME_WIDTH),
                                        rgbCap->get(CV_CAP_PROP_FRAME_HEIGHT)),
                               true);

  // Loop through the frames and write
  while (rgbCap->read(outFrame))
    {
    rgbWriter << outFrame;
    }

  // Clean up
  delete rgbCap;

  return EXIT_SUCCESS;
}
