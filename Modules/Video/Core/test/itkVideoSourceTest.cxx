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

#include "itkVideoSource.h"
#include "itkImageRegionIterator.h"

// Set up typedefs for test
const unsigned int Dimension =                   2;
typedef unsigned char                      PixelType;
typedef itk::Image< PixelType, Dimension > FrameType;
typedef itk::VideoStream< FrameType >      VideoType;
typedef itk::SizeValueType                 SizeValueType;

namespace itk
{
namespace VideoSourceTest
{
/** \class DummyVideoSource
 * Provide dummy implementation of VideoSource that just sets all pixels to 1
 */
template<typename TOutputVideoStream>
class DummyVideoSource : public VideoSource<TOutputVideoStream>
{
public:

  /** Standard class typedefs */
  typedef TOutputVideoStream                        OutputVideoStreamType;
  typedef DummyVideoSource< OutputVideoStreamType > Self;
  typedef VideoSource< OutputVideoStreamType >      Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;
  typedef WeakPointer< const Self >                 ConstWeakPointer;

  typedef typename TOutputVideoStream::FrameType OutputFrameType;
  typedef typename OutputFrameType::RegionType   OutputFrameSpatialRegionType;

  itkNewMacro(Self);

  itkTypeMacro(DummyVideoSource, VideoSource);

protected:

  /** Constructor */
  DummyVideoSource()
  {
    this->TemporalProcessObject::m_UnitInputNumberOfFrames = 1;
    this->TemporalProcessObject::m_UnitOutputNumberOfFrames = 1;
    this->TemporalProcessObject::m_FrameSkipPerOutput = 1;
    this->TemporalProcessObject::m_InputStencilCurrentFrameIndex = 1;
    this->m_Mutex = itk::MutexLock::New();
  }

  /** Override ThreadedGenerateData to set all pixels in the requested region
   * to 1 */
  virtual void ThreadedGenerateData(
    const OutputFrameSpatialRegionType& outputRegionForThread,
    int threadId) ITK_OVERRIDE
  {

    // Print out your threadId
    this->m_Mutex->Lock();
    std::cout << "Working on thread " << threadId << std::endl;
    this->m_Mutex->Unlock();

    OutputVideoStreamType* video = this->GetOutput();
    typename OutputVideoStreamType::TemporalRegionType requestedTemporalRegion =
      video->GetRequestedTemporalRegion();
    SizeValueType startFrame = requestedTemporalRegion.GetFrameStart();
    SizeValueType frameDuration = requestedTemporalRegion.GetFrameDuration();

    // Just as a check, throw an exception if the duration isn't equal to the
    // unit output size
    if (frameDuration != this->TemporalProcessObject::m_UnitOutputNumberOfFrames)
      {
      itkExceptionMacro(<< "Trying to generate output of non-unit size. Got: "
                        << frameDuration << " Expected: "
                        << this->TemporalProcessObject::m_UnitOutputNumberOfFrames);
      }

    for (SizeValueType i = startFrame; i < startFrame + frameDuration; ++i)
      {
      OutputFrameType*                          frame = video->GetFrame(i);
      itk::ImageRegionIterator<OutputFrameType> iter(frame, outputRegionForThread);
      while(!iter.IsAtEnd() )
        {
        // Set the pixel to 1
        iter.Set(1);
        ++iter;
        }
      }
  }

  itk::MutexLock::Pointer m_Mutex;

};

/**
 * Create a new empty frame
 */
FrameType::Pointer CreateEmptyFrame()
{
  FrameType::Pointer out = FrameType::New();

  FrameType::RegionType largestRegion;
  FrameType::SizeType   sizeLR;
  FrameType::IndexType  startLR;

  startLR.Fill(0);
  sizeLR[0] = 50;
  sizeLR[1] = 40;
  largestRegion.SetSize(sizeLR);
  largestRegion.SetIndex(startLR);
  out->SetLargestPossibleRegion(largestRegion);

  FrameType::RegionType requestedRegion;
  FrameType::SizeType   sizeReq;
  FrameType::IndexType  startReq;
  startReq.Fill(2);
  sizeReq[0] = 20;
  sizeReq[1] = 10;
  requestedRegion.SetSize(sizeReq);
  requestedRegion.SetIndex(startReq);
  out->SetRequestedRegion(requestedRegion);

  out->Allocate();

  return out;
}

} // end namespace VideoSourceTest
} // end namespace itk

/**
 * Test the basic functionality of temporal data objects
 */
int itkVideoSourceTest( int, char* [] )
{

  //////
  // Test Instantiation
  //////
  typedef itk::VideoSourceTest::DummyVideoSource< VideoType > VideoSourceType;
  VideoSourceType::Pointer videoSource = VideoSourceType::New();

  //////
  // Test Graft
  //////

  // Create a VideoStream
  VideoType::Pointer  video = VideoType::New();
  itk::TemporalRegion largestRegion;
  itk::TemporalRegion requestedRegion;
  itk::TemporalRegion bufferedRegion;
  largestRegion.SetFrameStart(0);
  largestRegion.SetFrameDuration(10);
  requestedRegion.SetFrameStart(2);
  requestedRegion.SetFrameDuration(5);
  bufferedRegion.SetFrameStart(1);
  bufferedRegion.SetFrameDuration(8);
  video->SetLargestPossibleTemporalRegion(largestRegion);
  video->SetRequestedTemporalRegion(requestedRegion);
  video->SetBufferedTemporalRegion(bufferedRegion);
  FrameType::Pointer frame;
  for (SizeValueType i = bufferedRegion.GetFrameStart();
       i < bufferedRegion.GetFrameStart() + bufferedRegion.GetFrameDuration();
       ++i)
    {
    frame = itk::VideoSourceTest::CreateEmptyFrame();
    video->SetFrame(i, frame);
    }

  // Graft video onto output of VideoSource
  videoSource->GraftOutput(video);

  // Check that graft worked
  VideoType::Pointer videoOut = videoSource->GetOutput();
  if (videoOut->GetLargestPossibleTemporalRegion() != video->GetLargestPossibleTemporalRegion() ||
      videoOut->GetRequestedTemporalRegion() != video->GetRequestedTemporalRegion() ||
      videoOut->GetBufferedTemporalRegion() != video->GetBufferedTemporalRegion() )
    {
    std::cerr << "Graft failed to copy meta information" << std::endl;
    return EXIT_FAILURE;
    }
  if (videoOut->GetFrameBuffer() != video->GetFrameBuffer() )
    {
    std::cerr << "Graft failed to assign frame buffer correctly" << std::endl;
    return EXIT_FAILURE;
    }

  //////
  // Test ThreadedGenerateData System
  //////

  // Reset videoSource
  videoSource = VideoSourceType::New();

  // Set the requested regions on videoSource's output
  VideoType::Pointer output = videoSource->GetOutput();
  output->SetRequestedTemporalRegion(requestedRegion);
  output->InitializeEmptyFrames();
  FrameType::RegionType spatialRegion = frame->GetRequestedRegion();
  output->SetAllRequestedSpatialRegions(spatialRegion);

  // Call update to set the requested spatial region to 1 for each requested
  // frame
  videoSource->Update();

  // Check the pixel values of the output
  SizeValueType frameStart = requestedRegion.GetFrameStart();
  SizeValueType numFrames = requestedRegion.GetFrameDuration();
  for (SizeValueType i = frameStart; i < frameStart + numFrames; ++i)
    {
    frame = videoSource->GetOutput()->GetFrame(i);
    FrameType::RegionType region = frame->GetRequestedRegion();
    itk::ImageRegionIterator<FrameType> iter( frame, region);
    while (!iter.IsAtEnd() )
      {
      if (iter.Get() != 1)
        {
        std::cerr << "Pixel not set correctly" << std::endl;
        return EXIT_FAILURE;
        }
      ++iter;
      }

      // Make sure (0,0) which was outside the requested spatial region didn't
      // get set
      if (region.GetNumberOfPixels() > 0)
       {
       FrameType::IndexType idx;
       idx.Fill(0);
       if (frame->GetPixel(idx) == 1)
          {
          std::cerr << "Pixel outside requested spatial region set to 1" << std::endl;
          return EXIT_FAILURE;
          }
       }
     }

  //////
  // Test that the output has the proper number of buffers when no requested
  // temporal region manually set
  //////

  // Reset videoSource and the requsted temporal region of tdo
  videoSource = VideoSourceType::New();
  videoSource->UpdateOutputInformation();

  // Make sure the requested temporal region of videoSource's output is empty
  itk::TemporalRegion emptyRegion;
  if (videoSource->GetOutput()->GetRequestedTemporalRegion() != emptyRegion)
    {
    std::cerr << "videoSource's output's requested temporal region not empty before propagate"
              << std::endl;
    return EXIT_FAILURE;
    }

  // Propagate the request
  videoSource->PropagateRequestedRegion(videoSource->GetOutput() );

  // Since the largest possible region's duration is infinte, the request
  // should have duration 1
  if (videoSource->GetOutput()->GetRequestedTemporalRegion().GetFrameDuration() != 1)
    {
    std::cerr << "videoSource's output's requested temporal region not set "
              << "correctly after propagate for with infinte largest region" << std::endl;
    return EXIT_FAILURE;
    }

  // Artificially set the output's largest possible temporal region duration
  itk::TemporalRegion largestTempRegion =
    videoSource->GetOutput()->GetLargestPossibleTemporalRegion();
  unsigned int newNumBuffers = 25;
  largestTempRegion.SetFrameDuration(newNumBuffers);
  videoSource->GetOutput()->SetLargestPossibleTemporalRegion(largestTempRegion);
  videoSource->GetOutput()->SetRequestedTemporalRegion(emptyRegion);

  // No propagate again and make sure 25 buffers have been set
  videoSource->PropagateRequestedRegion(videoSource->GetOutput() );
  if (videoSource->GetOutput()->GetNumberOfBuffers() != newNumBuffers)
    {
    std::cerr << "Number of buffers not set correctly after propagate. Got: "
              << videoSource->GetOutput()->GetNumberOfBuffers() << " Expected: " << newNumBuffers
              << std::endl;
    return EXIT_FAILURE;
    }

  //////
  // Return Successfully
  //////
  return EXIT_SUCCESS;

}
