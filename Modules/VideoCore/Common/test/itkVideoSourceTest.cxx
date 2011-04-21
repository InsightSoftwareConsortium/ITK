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

namespace itk
{
namespace test
{
/** \class DummyVideoSource
 * Provide dummy implementation of VideoSource that just sets all pixels to 1
 */
template<class TOutputVideoStream>
class DummyVideoSource : public VideoSource<TOutputVideoStream>
{
public:

  /** Standard class typedefs */
  typedef TOutputVideoStream                         OutputVideoStreamType;
  typedef DummyVideoSource< OutputVideoStreamType >  Self;
  typedef VideoSource< OutputVideoStreamType >       Superclass;
  typedef SmartPointer< Self >                       Pointer;
  typedef SmartPointer< const Self >                 ConstPointer;
  typedef WeakPointer< const Self >                  ConstWeakPointer;

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
    }

  /** Override ThreadedGenerateData to increment a counter */
  virtual void ThreadedGenerateData(
                const OutputFrameSpatialRegionType& outputRegionForThread,
                int threadId)
    {
    //itk::
    }
};


} // end namespace test
} // end namespace itk


// Set up typedefs
const unsigned int Dimension =                   2;
typedef unsigned char                            PixelType;
typedef itk::Image< PixelType, Dimension >       FrameType;
typedef itk::VideoStream< FrameType >            VideoType;
typedef itk::test::DummyVideoSource< VideoType > VideoSourceType;

/**
 * Create a new empty frame
 */
FrameType::Pointer CreateEmptyFrame()
{
  FrameType::Pointer out = FrameType::New();
  FrameType::RegionType region;
  FrameType::SizeType size;
  FrameType::IndexType start;

  start.Fill(0);
  size[0] = 50;
  size[1] = 40;
  region.SetSize(size);
  region.SetIndex(start);
  out->SetLargestPossibleRegion(region);
  out->Allocate();

  return out;
}

/**
 * Test the basic functionality of temporal data objects
 */
int itkVideoSourceTest( int argc, char* argv[] )
{

  //////
  // Test Instantiation
  //////

  VideoSourceType::Pointer videoSource = VideoSourceType::New();


  //////
  // Test Graft
  //////

  // Create a VideoStream
  VideoType::Pointer video = VideoType::New();
  itk::TemporalRegion largestRegion;
  itk::TemporalRegion requestedRegion;
  itk::TemporalRegion bufferedRegion;
  largestRegion.SetFrameStart(0);
  largestRegion.SetFrameDuration(10);
  requestedRegion.SetFrameStart(1);
  requestedRegion.SetFrameDuration(8);
  bufferedRegion.SetFrameStart(0);
  bufferedRegion.SetFrameDuration(3);
  video->SetLargestPossibleTemporalRegion(largestRegion);
  video->SetRequestedTemporalRegion(requestedRegion);
  video->SetBufferedTemporalRegion(bufferedRegion);
  FrameType::Pointer frame1 = CreateEmptyFrame();
  FrameType::Pointer frame2 = CreateEmptyFrame();
  FrameType::Pointer frame3 = CreateEmptyFrame();
  video->AppendFrame(frame1);
  video->AppendFrame(frame2);
  video->AppendFrame(frame3);

  // Graft video onto output of VideoSource
  videoSource->GraftOutput(video);

  // Check that graft worked
  VideoType::Pointer videoOut = videoSource->GetOutput();
  if (videoOut->GetLargestPossibleTemporalRegion() != video->GetLargestPossibleTemporalRegion() ||
      videoOut->GetRequestedTemporalRegion() != video->GetRequestedTemporalRegion() ||
      videoOut->GetBufferedTemporalRegion() != video->GetBufferedTemporalRegion())
    {
    std::cerr << "Graft failed to copy meta information" << std::endl;
    return EXIT_FAILURE;
    }
  if (videoOut->GetFrameBuffer() != video->GetFrameBuffer())
    {
    std::cerr << "Graft failed to assign frame buffer correctly" << std::endl;
    return EXIT_FAILURE;
    }


  //////
  // Test ThreadedGenerateDataSystem
  //////

  // Reset videoSource
  videoSource = VideoSourceType::New();

  // Set the requested regions on videoSource's output
  VideoType::Pointer output = videoSource->GetOutput();
  FrameType::RegionType spatialRegion = frame1->GetLargestPossibleRegion();
  output->SetFrameRequestedSpatialRegion(0, spatialRegion);
  videoSource->Update();


  //////
  // Return Successfully
  //////
  return EXIT_SUCCESS;

}
