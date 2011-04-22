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


// Set up typedefs
static const unsigned int Dimension =      2;
typedef unsigned char                      PixelType;
typedef itk::Image< PixelType, Dimension > FrameType;
typedef itk::VideoStream< FrameType >      VideoType;

/** Set up a spatial region with the given dimensions */
FrameType::RegionType SetUpSpatialRegion(unsigned int x, unsigned int y)
{
  FrameType::RegionType out;
  FrameType::RegionType::SizeType size;
  FrameType::RegionType::IndexType start;
  size[0] = x;
  size[1] = y;
  start.Fill( 0 );
  out.SetSize( size );
  out.SetIndex( start );
  return out;
}

/**
 * Test the basic functionality of temporal data objects
 */
int itkVideoStreamTest( int argc, char* argv[] )
{
  //////
  // Test Instantiation and grafting
  //////

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
  FrameType::Pointer frame1 = FrameType::New();
  FrameType::Pointer frame2 = FrameType::New();
  FrameType::Pointer frame3 = FrameType::New();
  video2->AppendFrame(frame1);
  video2->AppendFrame(frame2);
  video2->AppendFrame(frame3);

  // Test retreiving frames
  FrameType::Pointer outFrame3 = video2->GetFrame(0);
  FrameType::Pointer outFrame2 = video2->GetFrame(-1);
  FrameType::Pointer outFrame1 = video2->GetFrame(-2);
  if (outFrame3 != frame3 || outFrame2 != frame2 || outFrame1 != frame1)
    {
    std::cerr << "Frames not retreived correctly" << std::endl;
    return EXIT_FAILURE;
    }

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


  //////
  // Test Initialization, region setting, and allocation
  //////

  video1 = VideoType::New();

  // Set the buffered temporal region
  VideoType::TemporalRegionType bufferedTemporalRegion;
  unsigned long numFrames = 5;
  bufferedTemporalRegion.SetFrameStart( 0 );
  bufferedTemporalRegion.SetFrameDuration( numFrames );
  video1->SetBufferedTemporalRegion( bufferedTemporalRegion );

  // Initialize all frames in the buffered temporal region
  video1->InitializeEmptyFrames();

  // Set the buffered spatial region for each frame
  FrameType::RegionType largestSpatialRegion = SetUpSpatialRegion(100, 100);
  FrameType::RegionType requestedSpatialRegion = SetUpSpatialRegion(40, 40);
  FrameType::RegionType bufferedSpatialRegion = SetUpSpatialRegion(50, 40);
  video1->SetAllLargestPossibleSpatialRegions( largestSpatialRegion );
  video1->SetAllRequestedSpatialRegions( requestedSpatialRegion );
  video1->SetAllBufferedSpatialRegions( bufferedSpatialRegion );

  // Make sure regions were set correctly
  for (unsigned long i = 1; i <= numFrames; ++i)
    {
    FrameType* frame = video1->GetFrame(i);
    if (frame->GetLargestPossibleRegion() != largestSpatialRegion)
      {
      std::cerr << "Frame " << i << " largest possible spatial region not set correctly"
                << std::endl;
      return EXIT_FAILURE;
      }
    if (frame->GetRequestedRegion() != requestedSpatialRegion)
      {
      std::cerr << "Frame " << i << " requested spatial region not set correctly"
                << std::endl;
      return EXIT_FAILURE;
      }
    if (frame->GetBufferedRegion() != bufferedSpatialRegion)
      {
      std::cerr << "Frame " << i << " buffered spatial region not set correctly"
                << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Allocate memory for the frames
  video1->Allocate();

  // Try to set and get a pixel on each frame
  FrameType::IndexType idx;
  idx[0] = 49;
  idx[1] = 39;
  PixelType pxVal = 35;
  for (unsigned long i = 1; i <= numFrames; ++i)
    {
    FrameType* frame = video1->GetFrame(i);
    frame->SetPixel(idx, pxVal);
    }
  for (unsigned long i = 1; i <= numFrames; ++i)
    {
    FrameType* frame = video1->GetFrame(i);
    if (frame->GetPixel(idx) != pxVal)
      {
      std::cerr << "Error setting/getting pixel for frame " << i << std::endl;
      return EXIT_FAILURE;
      }
    }


  return EXIT_SUCCESS;

}
