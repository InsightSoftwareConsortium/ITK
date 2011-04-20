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

#include "itkVideoStream.h"

/**
 * Test the basic functionality of temporal data objects
 */
int itkVideoStreamTest( int argc, char* argv[] )
{
  // Set up typedefs
  const unsigned int Dimension =             2;
  typedef unsigned char                      PixelType;
  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::VideoStream< ImageType >      VideoType;


  // Instantiate a new VideoStream
  VideoType::Pointer video = VideoType::New();

  // Check dimension
  if (video->GetFrameDimension() != Dimension)
    {
    std::cerr << "GetFrameDimesion failed" << std::endl;
    return EXIT_FAILURE;
    }
  if (VideoType::FrameDimension != Dimension)
    {
    std::cerr << "VideoType::FrameDimension failed" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
