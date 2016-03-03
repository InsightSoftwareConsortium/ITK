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

#include "itkVideoToVideoFilter.h"
#include "itkImageRegionIterator.h"
#include "itkTestingMacros.h"

// typedefs for test
const unsigned int Dimension =                          2;
typedef unsigned char                            InputPixelType;
typedef itk::Image< InputPixelType, Dimension >  InputFrameType;
typedef itk::VideoStream< InputFrameType >       InputVideoType;
typedef float                                    OutputPixelType;
typedef itk::Image< OutputPixelType, Dimension > OutputFrameType;
typedef itk::VideoStream< OutputFrameType >      OutputVideoType;
typedef itk::SizeValueType                       SizeValueType;

namespace itk
{
namespace VideoToVideoFilterTest
{

/**
 * Create a new frame and fill it with the indicated value
 */
InputFrameType::Pointer CreateInputFrame(InputPixelType val)
{
  InputFrameType::Pointer out = InputFrameType::New();

  InputFrameType::RegionType largestRegion;
  InputFrameType::SizeType   sizeLR;
  InputFrameType::IndexType  startLR;

  startLR.Fill(0);
  sizeLR[0] = 50;
  sizeLR[1] = 40;
  largestRegion.SetSize(sizeLR);
  largestRegion.SetIndex(startLR);
  out->SetRegions(largestRegion);

  out->Allocate();

  // Fill with the desired value
  itk::ImageRegionIterator<InputFrameType> iter(out, largestRegion);
  while(!iter.IsAtEnd() )
    {
    iter.Set(val);
    ++iter;
    }

  return out;
}

/** \class DummyVideoToVideoFilter
 * \brief A simple implementation of VideoTOVideoFilter for the test
 */
template< typename TInputVideoStream, typename TOutputVideoStream >
class DummyVideoToVideoFilter :
  public VideoToVideoFilter< TInputVideoStream, TOutputVideoStream >
{
public:

  /** Standard class typedefs */
  typedef TInputVideoStream                                InputVideoStreamType;
  typedef TOutputVideoStream                               OutputVideoStreamType;
  typedef DummyVideoToVideoFilter< InputVideoStreamType,
                                   OutputVideoStreamType > Self;
  typedef VideoToVideoFilter< TInputVideoStream,
                             TOutputVideoStream >          Superclass;
  typedef SmartPointer< Self >                             Pointer;
  typedef SmartPointer< const Self >                       ConstPointer;
  typedef WeakPointer< const Self >                        ConstWeakPointer;

  typedef typename TInputVideoStream::FrameType  InputFrameType;
  typedef typename InputFrameType::RegionType    InputFrameSpatialRegionType;
  typedef typename TOutputVideoStream::FrameType OutputFrameType;
  typedef typename OutputFrameType::RegionType   OutputFrameSpatialRegionType;

  itkNewMacro(Self);

  itkTypeMacro(DummyVideoToVideoFilter, VideoToVideoFilter);

protected:

  /** Constructor */
  DummyVideoToVideoFilter()
  {
    this->TemporalProcessObject::m_UnitInputNumberOfFrames = 2;
    this->TemporalProcessObject::m_UnitOutputNumberOfFrames = 1;
    this->TemporalProcessObject::m_FrameSkipPerOutput = 1;
    this->TemporalProcessObject::m_InputStencilCurrentFrameIndex = 1;
  }

  /** Override ThreadedGenerateData */
  virtual void ThreadedGenerateData(
    const OutputFrameSpatialRegionType& outputRegionForThread,
    int threadId) ITK_OVERRIDE
  {
    const InputVideoStreamType* input = this->GetInput();
    OutputVideoStreamType*      output = this->GetOutput();

    typename OutputVideoStreamType::TemporalRegionType outReqTempRegion =
      output->GetRequestedTemporalRegion();
    SizeValueType outputStart = outReqTempRegion.GetFrameStart();
    SizeValueType outputDuration = outReqTempRegion.GetFrameDuration();

    typename InputVideoStreamType::TemporalRegionType inReqTempRegion =
      input->GetRequestedTemporalRegion();
    SizeValueType inputStart = inReqTempRegion.GetFrameStart();
    SizeValueType inputDuration = inReqTempRegion.GetFrameDuration();

    // Print out your threadId
    std::cout << "Working on thread " << threadId << std::endl;
    std::cout << "  input: " << inputStart << " -> " << inputDuration << std::endl;
    std::cout << "  output: " << outputStart << " -> " << outputDuration << std::endl;

    // Just as a check, throw an exception if the durations aren't equal to the
    // unit output sizes
    if (outputDuration != this->TemporalProcessObject::m_UnitOutputNumberOfFrames)
      {
      itkExceptionMacro(<< "Trying to generate output of non-unit size. Got: "
                        << outputDuration << " Expected: "
                        << this->TemporalProcessObject::m_UnitOutputNumberOfFrames);
      }
    if (inputDuration < this->TemporalProcessObject::m_UnitInputNumberOfFrames)
      {
      itkExceptionMacro(<< "Input buffered region smaller than unit size. Got: "
                        << inputDuration << " Expected: "
                        << this->TemporalProcessObject::m_UnitInputNumberOfFrames);
      }

    // Get the two input frames and average them in the requested spatial region
    // of the
    // output frame
    const InputFrameType*                         inFrame0 = input->GetFrame(inputStart);
    const InputFrameType*                         inFrame1 = input->GetFrame(inputStart+1);
    OutputFrameType*                              outFrame = output->GetFrame(outputStart);
    itk::ImageRegionConstIterator<InputFrameType> inIter0(inFrame0, outputRegionForThread);
    itk::ImageRegionConstIterator<InputFrameType> inIter1(inFrame1, outputRegionForThread);
    itk::ImageRegionIterator<OutputFrameType>     outIter(outFrame, outputRegionForThread);
    while(!outIter.IsAtEnd() )
      {
      // Average input pixel values
      OutputPixelType val = ( (OutputPixelType)inIter0.Get() + (OutputPixelType)inIter1.Get() ) / 2;
      outIter.Set(val);
      ++outIter;
      ++inIter0;
      ++inIter1;
      }
  }

};

} // end namespace VideoToVideoFilterTest
} // end namespace itk

/**
 * Test the basic functionality of temporal data objects
 */
int itkVideoToVideoFilterTest( int, char* [] )
{

  //////
  // Set up new filter
  //////

  // Instantiate a filter
  typedef itk::VideoToVideoFilterTest::
  DummyVideoToVideoFilter< InputVideoType, OutputVideoType > VideoFilterType;
  VideoFilterType::Pointer filter = VideoFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, DummyVideoToVideoFilter, VideoToVideoFilter );

  // Set up an input video stream
  InputVideoType::Pointer inputVideo = InputVideoType::New();
  itk::TemporalRegion     inputLargestTemporalRegion;
  SizeValueType           inputStart = 0;
  SizeValueType           inputDuration = 10;
  inputLargestTemporalRegion.SetFrameStart(inputStart);
  inputLargestTemporalRegion.SetFrameDuration(inputDuration);
  inputVideo->SetLargestPossibleTemporalRegion(inputLargestTemporalRegion);

  // Fill the input with frames
  inputVideo->SetNumberOfBuffers(inputDuration);
  for (SizeValueType i = inputStart; i < inputStart + inputDuration; ++i)
    {
    inputVideo->SetFrame(i, itk::VideoToVideoFilterTest::CreateInputFrame(i) );
    }
  inputVideo->SetBufferedTemporalRegion(inputLargestTemporalRegion);

  //////
  // Connect input to filter and update
  //////

  // Connect input
  filter->SetInput(inputVideo);
  filter->UpdateOutputInformation();
  filter->GetOutput()->SetRequestedTemporalRegion(
    filter->GetOutput()->GetLargestPossibleTemporalRegion() );

  // Set up the requested spatial region on the output frames
  OutputFrameType::RegionType outputRequestedSpatialRegion;
  OutputFrameType::SizeType   size;
  OutputFrameType::IndexType  start;
  size[0] = inputVideo->GetFrame(0)->GetLargestPossibleRegion().GetSize()[0]/2;
  size[1] = inputVideo->GetFrame(0)->GetLargestPossibleRegion().GetSize()[1]/2;
  start[0] = inputVideo->GetFrame(0)->GetLargestPossibleRegion().GetSize()[0]/4;
  start[1] = inputVideo->GetFrame(0)->GetLargestPossibleRegion().GetSize()[1]/4;
  outputRequestedSpatialRegion.SetSize(size);
  outputRequestedSpatialRegion.SetIndex(start);
  filter->GetOutput()->SetAllRequestedSpatialRegions(outputRequestedSpatialRegion);

  // Set the number of frame buffers on the output so that we get all output
  // frames buffered at the end
  filter->GetOutput()->SetNumberOfBuffers(
    filter->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameDuration() );

  // Update the filter
  filter->SetNumberOfThreads(1);
  filter->Update();

  // Report on output buffers
  std::cout << "Number of output buffers: " << filter->GetOutput()->GetNumberOfBuffers() << std::endl;

  // Make sure results are correct in the requested spatial region
  SizeValueType outputStart =
    filter->GetOutput()->GetRequestedTemporalRegion().GetFrameStart();
  SizeValueType outputDuration =
    filter->GetOutput()->GetRequestedTemporalRegion().GetFrameDuration();
  for (SizeValueType i = outputStart; i < outputStart + outputDuration; ++i)
    {
    std::cout << "Checking frame: " << i << std::endl;

    const OutputFrameType*                         frame = filter->GetOutput()->GetFrame(i);
    itk::ImageRegionConstIterator<OutputFrameType> iter(frame, frame->GetRequestedRegion() );

    OutputPixelType expectedVal = ( (OutputPixelType)(i)-1.0 + (OutputPixelType)(i) )/2.0;
    OutputPixelType epsilon = .00001;
    while (!iter.IsAtEnd() )
      {
      if (iter.Get() < expectedVal - epsilon || iter.Get() > expectedVal + epsilon)
        {
        std::cerr << "Filter didn't set values correctly. Got: "
                  << iter.Get() << " Expected: " << expectedVal << std::endl;
        return EXIT_FAILURE;
        }
      ++iter;
      }

    // Make sure nothing set outside of requested spatial region
    OutputFrameType::IndexType idx;
    idx.Fill(0);
    if (frame->GetRequestedRegion().IsInside(idx))
      {
      std::cerr << "Filter set pixel outside of requested region" << std::endl;
      return EXIT_FAILURE;
      }
    }

  //////
  // Test that the output's spatial region request gets properly set to the
  // largest possible region if none set manually
  //////

  // Reset the filter
  filter = VideoFilterType::New();
  filter->SetInput(inputVideo);
  filter->UpdateOutputInformation();

  // Make sure the requested spatial regions are empty
  SizeValueType startFrame =
    filter->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameStart();
  SizeValueType numFrames =
    filter->GetOutput()->GetLargestPossibleTemporalRegion().GetFrameDuration();
  if (numFrames == 0)
    {
    std::cerr << "Output's largest possible temporal region not set correctly" << std::endl;
    }
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    for (unsigned int j = 0; j < Dimension; ++j)
      {
      if (filter->GetOutput()->GetFrameRequestedSpatialRegion(i).GetSize()[j] != 0)
        {
        std::cerr << "Output's requested spatial region not empty for frame " << i
                  << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  // Now, propagate the requested region and make sure the requested spatial
  // regions match the largest possible
  filter->PropagateRequestedRegion(filter->GetOutput() );
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    if (filter->GetOutput()->GetFrameRequestedSpatialRegion(i) !=
        filter->GetOutput()->GetFrameLargestPossibleSpatialRegion(i) ||
        filter->GetOutput()->GetFrameRequestedSpatialRegion(i).GetSize()[0] == 0)
      {
      std::cerr << "Output's requested spatial region not set correctly after propagation "
                << "for frame " << i << std::endl;
      return EXIT_FAILURE;
      }
    }

  //////
  // Return Successfully
  //////
  return EXIT_SUCCESS;

}
