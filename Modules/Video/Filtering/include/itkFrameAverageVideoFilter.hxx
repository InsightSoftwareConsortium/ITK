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
#ifndef itkFrameAverageVideoFilter_hxx
#define itkFrameAverageVideoFilter_hxx

#include "itkFrameAverageVideoFilter.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"

namespace itk
{

//
// Constructor
//
template<typename TInputVideoStream, typename TOutputVideoStream>
FrameAverageVideoFilter<TInputVideoStream, TOutputVideoStream>::
FrameAverageVideoFilter()
{
  // Default to average over 2 frames
  this->TemporalProcessObject::m_UnitInputNumberOfFrames = 2;

  // Always output a single frame
  this->TemporalProcessObject::m_UnitOutputNumberOfFrames = 1;

  // The output frame gets the number of the first frame in the set
  this->TemporalProcessObject::m_InputStencilCurrentFrameIndex = 0;

  // Move forward one frame of input for every frame of output
  this->TemporalProcessObject::m_FrameSkipPerOutput = 1;
}


//
// PrintSelf
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
FrameAverageVideoFilter<TInputVideoStream, TOutputVideoStream>::
PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent
     << "NumberOfFrames: " << this->TemporalProcessObject::m_UnitInputNumberOfFrames
     << std::endl;
}


//
// SetNumberOfFrames
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
FrameAverageVideoFilter<TInputVideoStream, TOutputVideoStream>::
SetNumberOfFrames(SizeValueType numFrames)
{
  this->TemporalProcessObject::m_UnitInputNumberOfFrames = numFrames;
  this->Modified();
}


//
// GetNumberOfFrames
//
template<typename TInputVideoStream, typename TOutputVideoStream>
SizeValueType
FrameAverageVideoFilter<TInputVideoStream, TOutputVideoStream>::
GetNumberOfFrames()
{
  return this->TemporalProcessObject::m_UnitInputNumberOfFrames;
}


//
// ThreadedGenerateData
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
FrameAverageVideoFilter<TInputVideoStream, TOutputVideoStream>::
ThreadedGenerateData(const OutputFrameSpatialRegionType& outputRegionForThread,
                     int itkNotUsed(threadId))
{
  // Get the input and output video streams
  const InputVideoStreamType* input = this->GetInput();
  OutputVideoStreamType* output = this->GetOutput();
  SizeValueType numFrames = this->TemporalProcessObject::m_UnitInputNumberOfFrames;

  // Get output frame number
  typename OutputVideoStreamType::TemporalRegionType outReqTempRegion =
    output->GetRequestedTemporalRegion();
  SizeValueType outputFrameNumber = outReqTempRegion.GetFrameStart();

  typename InputVideoStreamType::TemporalRegionType inReqTempRegion =
    input->GetRequestedTemporalRegion();
  SizeValueType inputStart = inReqTempRegion.GetFrameStart();
  SizeValueType inputDuration = inReqTempRegion.GetFrameDuration();

  // Make sure we've got the right duration
  if (inputDuration != numFrames)
    {
    itkExceptionMacro("Incorrect number of input frames");
    }

  // Get iterators for the input frames
  typedef ImageRegionConstIterator<InputFrameType> IterType;
  std::vector< IterType > inputIters;
  for (SizeValueType i = inputStart; i < inputStart + numFrames; ++i)
    {
    OutputFrameSpatialRegionType inputRegion;
    inputRegion.SetSize(outputRegionForThread.GetSize());
    inputRegion.SetIndex(outputRegionForThread.GetIndex());
    inputIters.push_back(IterType(input->GetFrame(i), inputRegion));
    }

  // Get the output frame and its iterator
  OutputFrameType* outFrame = output->GetFrame(outputFrameNumber);
  itk::ImageRegionIterator<OutputFrameType> outIter(outFrame, outputRegionForThread);

  // Average the input frames at each pixel of the output region
  typedef typename NumericTraits<OutputPixelType>::RealType OutputPixelRealType;
  while(!outIter.IsAtEnd())
    {
    // Get values for each input frame
    OutputPixelRealType val = 0;
    for (SizeValueType i = 0; i < numFrames; ++i)
      {
      val += (OutputPixelRealType)(inputIters[i].Get()) / (OutputPixelRealType)numFrames;
      }

    // Compute the average and set the output pixel's value
    outIter.Set((OutputPixelType)val);

    // Update the iterators
    ++outIter;
    for (SizeValueType i = 0; i < numFrames; ++i)
      {
      ++inputIters[i];
      }
    }
}

} // end namespace itk

#endif
