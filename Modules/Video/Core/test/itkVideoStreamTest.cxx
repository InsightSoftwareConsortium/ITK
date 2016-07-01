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
static ITK_CONSTEXPR_VAR unsigned int Dimension = 2;
typedef unsigned char                      PixelType;
typedef itk::Image< PixelType, Dimension > FrameType;
typedef itk::VideoStream< FrameType >      VideoType;
typedef itk::SizeValueType                 SizeValueType;

/** Set up a spatial region with the given dimensions */
FrameType::RegionType SetUpSpatialRegion(unsigned int x, unsigned int y)
{
  FrameType::RegionType            out;
  FrameType::RegionType::SizeType  size;
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
int itkVideoStreamTest( int, char* [] )
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

  // Use GetFrameBuffer and SetFrameBuffer to make video1 point to the same
  // buffer as video2
  video1->SetFrameBuffer(video2->GetFrameBuffer() );
  if (video1->GetFrameBuffer() != video2->GetFrameBuffer() )
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
  video2->SetFrame(0, frame1);
  video2->SetFrame(1, frame2);
  video2->SetFrame(2, frame3);

  // Test retreiving frames
  FrameType::Pointer outFrame1 = video2->GetFrame(0);
  FrameType::Pointer outFrame2 = video2->GetFrame(1);
  FrameType::Pointer outFrame3 = video2->GetFrame(2);
  if (outFrame3 != frame3 || outFrame2 != frame2 || outFrame1 != frame1)
    {
    std::cerr << "Frames not retreived correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // Make sure frames actually got buffered
  if (!frameBuffer->BufferIsFull(0) ||
      !frameBuffer->BufferIsFull(1) ||
      !frameBuffer->BufferIsFull(2) ||
      frameBuffer->BufferIsFull(3) )
    {
    std::cerr << "Frames not correctly appended" << std::endl;
    return EXIT_FAILURE;
    }

  // Graft video2 onto video1 and check results
  video1->Graft(video2);
  if (video1->GetLargestPossibleTemporalRegion() != video2->GetLargestPossibleTemporalRegion() ||
      video1->GetRequestedTemporalRegion() != video2->GetRequestedTemporalRegion() ||
      video1->GetBufferedTemporalRegion() != video2->GetBufferedTemporalRegion() )
    {
    std::cerr << "Graft failed to copy meta information" << std::endl;
    return EXIT_FAILURE;
    }
  if (video1->GetFrameBuffer() != video2->GetFrameBuffer() )
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
  VideoType::TemporalRegionType temporalRegion;
  SizeValueType                 startFrame = 0;
  SizeValueType                 numFrames = 5;
  temporalRegion.SetFrameStart( startFrame );
  temporalRegion.SetFrameDuration( numFrames );
  video1->SetLargestPossibleTemporalRegion( temporalRegion );
  video1->SetRequestedTemporalRegion( temporalRegion );
  video1->SetBufferedTemporalRegion( temporalRegion );

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
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    if (video1->GetFrameLargestPossibleSpatialRegion(i) != largestSpatialRegion)
      {
      std::cerr << "Frame " << i << " largest possible spatial region not set correctly"
                << std::endl;
      return EXIT_FAILURE;
      }
    if (video1->GetFrameRequestedSpatialRegion(i) != requestedSpatialRegion)
      {
      std::cerr << "Frame " << i << " requested spatial region not set correctly"
                << std::endl;
      return EXIT_FAILURE;
      }
    if (video1->GetFrameBufferedSpatialRegion(i) != bufferedSpatialRegion)
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
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    FrameType* frame = video1->GetFrame(i);
    frame->SetPixel(idx, i);
    }
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    FrameType* frame = video1->GetFrame(i);
    if (frame->GetPixel(idx) != i)
      {
      std::cerr << "Error setting/getting pixel for frame " << i << std::endl;
      return EXIT_FAILURE;
      }
    }

  //////
  // Test InitializeNewFrames with larger requested region. Make sure buffered
  // frames are still valid
  //////

  temporalRegion.SetFrameDuration(temporalRegion.GetFrameDuration() + 1);
  video1->SetLargestPossibleTemporalRegion(temporalRegion);
  video1->SetRequestedTemporalRegion(temporalRegion);
  video1->InitializeEmptyFrames();

  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    FrameType* frame = video1->GetFrame(i);
    if (frame->GetPixel(idx) != i)
      {
      std::cerr << "Error setting/getting pixel for frame " << i << std::endl;
      return EXIT_FAILURE;
      }
    }

  //////
  // Test meta data caching
  //////

  // Reset video1
  video1 = VideoType::New();
  itk::TemporalRegion tempReg = video1->GetLargestPossibleTemporalRegion();
  if (tempReg.GetFrameStart() != 0 || tempReg.GetFrameDuration() != 0)
    {
    std::cerr << "video1 initialized with non-empty temporal region" << std::endl;
    return EXIT_FAILURE;
    }

  // Set the cached meta-data for a non-buffered frame
  FrameType::RegionType            spatReg;
  FrameType::RegionType::SizeType  sz;
  FrameType::RegionType::IndexType start;
  FrameType::SpacingType           space;
  FrameType::PointType             orgn;
  FrameType::DirectionType         direction;
  sz[0] = 10;
  sz[1] = 20;
  start.Fill(0);
  spatReg.SetSize(sz);
  spatReg.SetIndex(start);
  space[0] = 0.1;
  space[1] = 0.5;
  orgn[0] = 5.432;
  orgn[1] = -23.4;
  direction[0][0] = 1;
  direction[0][1] = 2;
  direction[1][0] = -2;
  direction[1][1] = 1;
  video1->SetFrameLargestPossibleSpatialRegion(0, spatReg);
  video1->SetFrameRequestedSpatialRegion(0, spatReg);
  video1->SetFrameBufferedSpatialRegion(0, spatReg);
  video1->SetFrameSpacing(0, space);
  video1->SetFrameOrigin(0, orgn);
  video1->SetFrameDirection(0, direction);

  // Check retrieval while still unbuffered
  if (video1->GetFrameLargestPossibleSpatialRegion(0) != spatReg)
    {
    std::cerr << "video1 LargestPossibleSpatialRegion not cached correctly" << std::endl;
    return EXIT_FAILURE;
    }
  if (video1->GetFrameRequestedSpatialRegion(0) != spatReg)
    {
    std::cerr << "video1 RequestedSpatialRegion not cached correctly" << std::endl;
    return EXIT_FAILURE;
    }
  if (video1->GetFrameBufferedSpatialRegion(0) != spatReg)
    {
    std::cerr << "video1 BufferedSpatialRegion not cached correctly" << std::endl;
    return EXIT_FAILURE;
    }
  if (video1->GetFrameSpacing(0) != space)
    {
    std::cerr << "video1 Spacing not cached correctly" << std::endl;
    return EXIT_FAILURE;
    }
  if (video1->GetFrameOrigin(0) != orgn)
    {
    std::cerr << "video1 Origin not cached correctly" << std::endl;
    return EXIT_FAILURE;
    }
  if (video1->GetFrameDirection(0) != direction)
    {
    std::cerr << "video1 Direction not cached correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // Check meta data after frame initialized
  tempReg.SetFrameStart(0);
  tempReg.SetFrameDuration(1);
  video1->SetLargestPossibleTemporalRegion(tempReg);
  video1->SetRequestedTemporalRegion(tempReg);
  video1->SetBufferedTemporalRegion(tempReg);
  video1->InitializeEmptyFrames();
  FrameType* frame = video1->GetFrame(0);
  if (frame->GetLargestPossibleRegion() != spatReg)
    {
    std::cerr << "frame LargestPossibleRegion not initialized correctly" << std::endl;
    return EXIT_FAILURE;
    }
  if (frame->GetRequestedRegion() != spatReg)
    {
    std::cerr << "frame RequestedRegion not initialized correctly" << std::endl;
    return EXIT_FAILURE;
    }
  if (frame->GetBufferedRegion() != spatReg)
    {
    std::cerr << "frame BufferedRegion not initialized correctly" << std::endl;
    return EXIT_FAILURE;
    }
  if (frame->GetSpacing() != space)
    {
    std::cerr << "frame Spacing not initialized correctly" << std::endl;
    return EXIT_FAILURE;
    }
  if (frame->GetOrigin() != orgn)
    {
    std::cerr << "frame Origin not initialized correctly" << std::endl;
    return EXIT_FAILURE;
    }
  if (frame->GetDirection() != direction)
    {
    std::cerr << "frame Direction not initialized correctly" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
