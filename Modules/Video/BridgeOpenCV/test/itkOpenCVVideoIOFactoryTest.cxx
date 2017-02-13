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

#include "itkVideoIOFactory.h"
#include "itkOpenCVVideoIOFactory.h"

#include "opencv2/core/version.hpp"
#if defined( CV_VERSION_EPOCH )
// OpenCV 2.4.x
#include "highgui.h"
#else
// OpenCV 3.x
#include "opencv2/videoio/videoio_c.h"
#endif

typedef itk::SizeValueType SizeValueType;

///////////////////////////////////////////////////////////////////////////////
// This tests all of the functionality of the OpenCVVideoIO
//
// Usage: [Video Input] [Non-Video Input] [Video Output] [Width] [Height]
//            [Num Frames] [FpS]

int test_OpenCVVideoIOFactory ( char* input, char* output, SizeValueType cameraNumber )
{

  int ret = EXIT_SUCCESS;

  //////
  // Register the OpenCVVideoIOFactory.
  //
  // There's something strange going on here that makes the factories not be
  // registered by default because of the order in which the includes happen.
  // The real strangeness seems to be in ITK's system with the modularized
  // framework since none of the factories get reigstered by default.
  itk::ObjectFactoryBase::RegisterFactory( itk::OpenCVVideoIOFactory::New() );


  //////
  // Create the VideoIOBase for reading from a file
  //////
  std::cout << "Trying to create IO for reading from file..." << std::endl;
  itk::VideoIOBase::Pointer ioReadFile = itk::VideoIOFactory::CreateVideoIO(
                                    itk::VideoIOFactory::ReadFileMode, input);
  if (!ioReadFile)
    {
    std::cerr << "Did not create valid VideoIO for reading from file " << std::endl;
    ret = EXIT_FAILURE;
    }

  //////
  // Create the VideoIOBase for reading from a camera
  //////

  // Use openCV to see if we can even try to open the camera
  CvCapture* cameraCapture = cvCaptureFromCAM( cameraNumber );
  if (cameraCapture != ITK_NULLPTR)
    {
    std::cout << "Trying to create IO for reading from camera " << cameraNumber << "..." << std::endl;

    // Release the OpenCV capture
    cvReleaseCapture(&cameraCapture);

    std::stringstream ss;
    ss << cameraNumber;
    itk::VideoIOBase::Pointer ioReadCamera = itk::VideoIOFactory::CreateVideoIO(
                                      itk::VideoIOFactory::ReadCameraMode, ss.str().c_str());
    if (!ioReadCamera)
      {
      std::cerr << "Did not create valid VideoIO for reading from camera" << std::endl;
      ret = EXIT_FAILURE;
      }
    }

  //////
  // Create the VideoIOBase for writing to a file
  //////
  std::cout << "Trying to create IO for writing to file..." << std::endl;
  itk::VideoIOBase::Pointer ioWrite = itk::VideoIOFactory::CreateVideoIO(
                                       itk::VideoIOFactory::WriteMode, output);
  if (!ioWrite)
    {
    std::cerr << "Did not create valid VideoIO for writing " << std::endl;
    ret = EXIT_FAILURE;
    }

  std::cout<<"Done !"<<std::endl;
  return ret;
}

int itkOpenCVVideoIOFactoryTest ( int argc, char *argv[] )
{
  if (argc != 4)
    {
    std::cerr << "Usage: [Video Input] [Video Output] [Webcam Number]" << std::endl;
    return EXIT_FAILURE;
    }

  itk::ObjectFactoryBase::RegisterFactory( itk::OpenCVVideoIOFactory::New() );

  return test_OpenCVVideoIOFactory(argv[1], argv[2], atoi(argv[3]));
}
