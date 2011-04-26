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
#ifndef __itkVideoToVideoFilter_txx
#define __itkVideoToVideoFilter_txx

#include "itkImageToImageFilter.h"

namespace itk
{

//-CONSTRUCTOR PRINT-----------------------------------------------------------

//
// Constructor
//
template<class TInputVideoStream, class TOutputVideoStream>
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
VideoToVideoFilter()
{
  this->SetNumberOfRequiredInputs(1);
}

//-PUBLIC METHODS--------------------------------------------------------------

//
// SetInput( videoStream )
//
template<class TInputVideoStream, class TOutputVideoStream>
void
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
SetInput(const TInputVideoStream* videoStream)
{
  this->SetInput(0, const_cast< InputVideoStreamType* >(videoStream));
}

//
// SetInput( idx, videoStream )
//
template<class TInputVideoStream, class TOutputVideoStream>
void
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
SetInput(unsigned int idx, const TInputVideoStream* videoStream)
{
  this->TemporalProcessObject::SetNthInput( idx,
                                const_cast< InputVideoStreamType* >(videoStream) );
}

//
// GetInput()
//
template<class TInputVideoStream, class TOutputVideoStream>
const TInputVideoStream*
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
GetInput() const
{
  if (this->GetNumberOfInputs() < 1)
    {
    return NULL;
    }
  return static_cast< const InputVideoStreamType* >(this->ProcessObject::GetInput(0));
}

//
// GetInput(idx)
//
template<class TInputVideoStream, class TOutputVideoStream>
const TInputVideoStream*
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
GetInput(unsigned int idx) const
{
  return static_cast< const InputVideoStreamType* >(this->ProcessObject::GetInput(idx));
}

//-PROTECTED METHODS-----------------------------------------------------------

//
// GenerateInputRequestedRegion
//
template<class TInputVideoStream, class TOutputVideoStream>
void
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
GenerateInputRequestedRegion()
{
  // Call superclass's version to propagate temporal region
  Superclass::GenerateInputRequestedRegion();

  // Get the spatial region from the output frame
  unsigned long outputStart =
    this->GetOutput()->GetRequestedTemporalRegion().GetFrameStart();
  OutputFrameSpatialRegionType outputRegion =
    this->GetOutput()->GetFrameRequestedSpatialRegion(outputStart);

  // Convert to input spatial region (TODO: handle dificult cases)
  InputFrameSpatialRegionType inputRegion;
  inputRegion.SetSize(outputRegion.GetSize());
  inputRegion.SetIndex(outputRegion.GetIndex());

  // Create input spatial regions for each frame of each input
  for (unsigned int i = 0; i < this->GetNumberOfInputs(); ++i)
    {
    // Get the input and it's requeted temporal region
    InputVideoStreamType* input = dynamic_cast<InputVideoStreamType*>(
      this->ProcessObject::GetInput(i));
    if (!input)
      {
      continue;
      }
    TemporalRegion inRequestedTemporalRegion = input->GetRequestedTemporalRegion();

    // Loop over all frames in the temporal region
    unsigned long inputStart = inRequestedTemporalRegion.GetFrameStart();
    unsigned long numFrames = inRequestedTemporalRegion.GetFrameDuration();
    for (unsigned long j = inputStart; j < inputStart + numFrames; ++j)
      {
      // Get the input's frame
      InputFrameType* frame = input->GetFrame(j);
      frame->SetRequestedRegion( inputRegion );
      }
    }
}

} // end namespace itk

#endif
