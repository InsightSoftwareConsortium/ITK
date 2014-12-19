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
#ifndef itkFrameDifferenceVideoFilter_hxx
#define itkFrameDifferenceVideoFilter_hxx

#include "itkFrameDifferenceVideoFilter.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"

namespace itk
{

//
// Constructor
//
template<typename TInputVideoStream, typename TOutputVideoStream>
FrameDifferenceVideoFilter<TInputVideoStream, TOutputVideoStream>::
FrameDifferenceVideoFilter()
{
  // Default to differencing adjacent frames
  this->TemporalProcessObject::m_UnitInputNumberOfFrames = 2;

  // Always output one frame
  this->TemporalProcessObject::m_UnitOutputNumberOfFrames = 1;

  // Use the frame number of the last frame in the stencil
  this->TemporalProcessObject::m_InputStencilCurrentFrameIndex = 0;

  // Move forward one frame of input for every frame of output
  this->TemporalProcessObject::m_FrameSkipPerOutput = 1;
}


//
// PrintSelf
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
FrameDifferenceVideoFilter<TInputVideoStream, TOutputVideoStream>::
PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent
     << "FrameOffset: " << this->TemporalProcessObject::m_UnitInputNumberOfFrames - 1
     << std::endl;
}


//
// SetFrameOffset
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
FrameDifferenceVideoFilter<TInputVideoStream, TOutputVideoStream>::
SetFrameOffset(SizeValueType numFrames)
{
  // Expand the input stencil
  this->TemporalProcessObject::m_UnitInputNumberOfFrames = numFrames + 1;

  // Mark modified
  this->Modified();
}


//
// GetFrameOffset
//
template<typename TInputVideoStream, typename TOutputVideoStream>
SizeValueType
FrameDifferenceVideoFilter<TInputVideoStream, TOutputVideoStream>::
GetFrameOffset()
{
  return this->TemporalProcessObject::m_UnitInputNumberOfFrames - 1;
}


//
// ThreadedGenerateData
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
FrameDifferenceVideoFilter<TInputVideoStream, TOutputVideoStream>::
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
  typedef ImageRegionConstIterator<InputFrameType> ConstIterType;
  OutputFrameSpatialRegionType inputRegion;
  inputRegion.SetSize(outputRegionForThread.GetSize());
  inputRegion.SetIndex(outputRegionForThread.GetIndex());
  ConstIterType I0Iter(input->GetFrame(inputStart), inputRegion);
  ConstIterType I1Iter(input->GetFrame(inputStart + numFrames - 1), inputRegion);

  // Get the output frame and its iterator
  OutputFrameType* outFrame = output->GetFrame(outputFrameNumber);
  itk::ImageRegionIterator<OutputFrameType> outIter(outFrame, outputRegionForThread);

  // Average the input frames at each pixel of the output region
  typedef typename NumericTraits<InputPixelType>::RealType InputPixelRealType;
  while(!outIter.IsAtEnd())
    {
    // Compute the signed difference as a real value
    InputPixelRealType val = static_cast<InputPixelRealType>(I0Iter.Get()) -
                             static_cast<InputPixelRealType>(I1Iter.Get());

    // Square it
    val *= val;

    // Set the output
    outIter.Set(static_cast<OutputPixelType>(val));

    // Update the iterators
    ++outIter;
    ++I0Iter;
    ++I1Iter;
    }
}


} // end namespace itk

#endif
