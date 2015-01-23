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
#ifndef itkDecimateFramesVideoFilter_hxx
#define itkDecimateFramesVideoFilter_hxx

#include "itkDecimateFramesVideoFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

//
// Constructor
//
template<typename TVideoStream>
DecimateFramesVideoFilter<TVideoStream>::
DecimateFramesVideoFilter()
{
  this->TemporalProcessObject::m_UnitInputNumberOfFrames = 1;
  this->TemporalProcessObject::m_UnitOutputNumberOfFrames = 1;
  this->TemporalProcessObject::m_InputStencilCurrentFrameIndex = 0;

  // Set the frame skip amount using our SetPreservedFrameSpacing method.
  // Default to move forward 2 frames every time (keep every other frame)
  this->SetPreservedFrameSpacing(2);
}


//
// PrintSelf
//
template<typename TVideoStream>
void
DecimateFramesVideoFilter<TVideoStream>::
PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent
     << "NumberOfFrames: " << this->TemporalProcessObject::m_UnitInputNumberOfFrames
     << std::endl;
}


//
// SetPreservedFrameSpacing
//
template<typename TVideoStream>
void
DecimateFramesVideoFilter<TVideoStream>::
SetPreservedFrameSpacing(SizeValueType numFrames)
{
  this->TemporalProcessObject::m_FrameSkipPerOutput = numFrames;
  this->Modified();
}


//
// GetPreservedFrameSpacing
//
template<typename TVideoStream>
SizeValueType
DecimateFramesVideoFilter<TVideoStream>::
GetPreservedFrameSpacing()
{
  return this->TemporalProcessObject::m_FrameSkipPerOutput;
}


//
// ThreadedGenerateData
//
template<typename TVideoStream>
void
DecimateFramesVideoFilter<TVideoStream>::
ThreadedGenerateData(const FrameSpatialRegionType& outputRegionForThread,
                     int itkNotUsed(threadId))
{
  // Get the input and output video streams
  const InputVideoStreamType* input = this->GetInput();
  OutputVideoStreamType* output = this->GetOutput();

  // Get input and output frame numbers
  typename OutputVideoStreamType::TemporalRegionType outReqTempRegion =
    output->GetRequestedTemporalRegion();
  SizeValueType outFrameNum = outReqTempRegion.GetFrameStart();

  typename InputVideoStreamType::TemporalRegionType inReqTempRegion =
    input->GetRequestedTemporalRegion();
  SizeValueType inFrameNum = inReqTempRegion.GetFrameStart();

  // Since we want to support only returning a requested spatial region of the
  // input frame, we do the pass-through the slow way using iterators rather
  // than just copying memory.

  // Get iterators for requested region of input and output frames
  typedef ImageRegionConstIterator<FrameType> InputIterType;
  typedef ImageRegionIterator<FrameType>      OutputIterType;
  InputIterType inIter(input->GetFrame(inFrameNum), outputRegionForThread);
  OutputIterType outIter(output->GetFrame(outFrameNum), outputRegionForThread);

  // Pass the values from input to output
  while(!outIter.IsAtEnd())
    {
    // Compute the average and set the output pixel's value
    outIter.Set(inIter.Get());

    // Update the iterators
    ++outIter;
    ++inIter;
    }
}

} // end namespace itk

#endif
