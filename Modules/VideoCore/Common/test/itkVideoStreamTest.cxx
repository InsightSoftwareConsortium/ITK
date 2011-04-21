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
  typedef itk::Image< PixelType, Dimension > FrameType;
  typedef itk::VideoStream< FrameType >      VideoType;


  // Instantiate a new VideoStream
  VideoType::Pointer video1 = VideoType::New();

  // Check dimension
  if (video1->GetFrameDimension() != Dimension)
    {
    std::cerr << "GetFrameDimesion failed" << std::endl;
    return EXIT_FAILURE;
    }
  if (VideoType::FrameDimension != Dimension)
    {
    std::cerr << "VideoType::FrameDimension failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Create a second VideoStream
  VideoType::Pointer video2 = VideoType::New();

  // Use GetFrameBuffer and SetFrameBuffer to make video1 point to the same buffer as video2
  video1->SetFrameBuffer(video2->GetFrameBuffer());
  if (video1->GetFrameBuffer() != video2->GetFrameBuffer())
    {
    std::cerr << "Failed to properly Get/Set frame buffer" << std::endl;
    return EXIT_FAILURE;
    }

  // Reset video1
  video1 = VideoType::New();

  // Set up regions for video2
  itk::TemporalRegion largestRegion;
  itk::TemporalRegion requestedRegion;
  itk::TemporalRegion bufferedRegion;
  largestRegion.SetFrameStart(0);
  largestRegion.SetFrameDuration(10);
  requestedRegion.SetFrameStart(1);
  requestedRegion.SetFrameDuration(8);
  bufferedRegion.SetFrameStart(0);
  bufferedRegion.SetFrameDuration(3);
  video2->SetLargestPossibleTemporalRegion(largestRegion);
  video2->SetRequestedTemporalRegion(requestedRegion);
  video2->SetBufferedTemporalRegion(bufferedRegion);

  // Test setting number of buffers on video2
  VideoType::BufferType::Pointer frameBuffer = video2->GetFrameBuffer();
  frameBuffer->SetNumberOfBuffers(4);

  // Check that number of buffers got set right
  frameBuffer = video2->GetFrameBuffer();
  if (frameBuffer->GetNumberOfBuffers() != 4)
    {
    std::cerr << "Number of buffers not correctly set" << std::endl;
    return EXIT_FAILURE;
    }

  // Test appending frames to video2
  video2->AppendFrame(FrameType::New().GetPointer());
  video2->AppendFrame(FrameType::New().GetPointer());
  video2->AppendFrame(FrameType::New().GetPointer());

  // Make sure frames actually got buffered (frames at offset 0, -1, -2 should
  // be full and -3 should be empty)
  if (!frameBuffer->BufferIsFull(0) ||
      !frameBuffer->BufferIsFull(-1) ||
      !frameBuffer->BufferIsFull(-2) ||
      frameBuffer->BufferIsFull(-3))
    {
    std::cerr << "Frames not correctly appended" << std::endl;
    return EXIT_FAILURE;
    }

  // Graft video2 onto video1 and check results
  video1->Graft(video2);
  if (video1->GetLargestPossibleTemporalRegion() != video2->GetLargestPossibleTemporalRegion() ||
      video1->GetRequestedTemporalRegion() != video2->GetRequestedTemporalRegion() ||
      video1->GetBufferedTemporalRegion() != video2->GetBufferedTemporalRegion())
    {
    std::cerr << "Graft failed to copy meta information" << std::endl;
    return EXIT_FAILURE;
    }
  if (video1->GetFrameBuffer() != video2->GetFrameBuffer())
    {
    std::cerr << "Graft failed to graft frame buffer" << std::endl;
    return EXIT_FAILURE;
    }

  // For good measure, make sure that video1's frame buffer has 4 buffers
  if (video1->GetFrameBuffer()->GetNumberOfBuffers() != 4)
    {
    std::cerr << "Problem with graft's handling of frame buffer" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
