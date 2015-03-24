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
#ifndef itkVideoToVideoFilter_hxx
#define itkVideoToVideoFilter_hxx

#include "itkImageToImageFilter.h"

#include "itkVideoToVideoFilter.h"

namespace itk
{

//-CONSTRUCTOR PRINT-----------------------------------------------------------

//
// Constructor
//
template<typename TInputVideoStream, typename TOutputVideoStream>
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
VideoToVideoFilter()
{
  this->SetNumberOfRequiredInputs(1);
}
template<typename TInputVideoStream, typename TOutputVideoStream>
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>
::~VideoToVideoFilter()
{
}

template<typename TInputVideoStream, typename TOutputVideoStream>
void
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

//-PUBLIC METHODS--------------------------------------------------------------

//
// SetInput( videoStream )
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
SetInput(const TInputVideoStream* videoStream)
{
  // We keep this const_cast because in actuality, we do want to be able to
  // change the requested regions on the input so we need a non-const version
  this->SetInput(0, const_cast< InputVideoStreamType* >(videoStream));
}

//
// SetInput( idx, videoStream )
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
SetInput(unsigned int idx, const TInputVideoStream* videoStream)
{
  // We keep this const_cast because in actuality, we do want to be able to
  // change the requested regions on the input so we need a non-const version
  this->TemporalProcessObject::SetNthInput( idx,
                                const_cast< InputVideoStreamType* >(videoStream) );
}

//
// GetInput()
//
template<typename TInputVideoStream, typename TOutputVideoStream>
const TInputVideoStream*
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
GetInput() const
{
  if (this->GetNumberOfInputs() < 1)
    {
    return ITK_NULLPTR;
    }
  return static_cast< const InputVideoStreamType* >(this->ProcessObject::GetInput(0));
}

//
// GetInput(idx)
//
template<typename TInputVideoStream, typename TOutputVideoStream>
const TInputVideoStream*
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
GetInput(unsigned int idx) const
{
  return static_cast< const InputVideoStreamType* >(this->ProcessObject::GetInput(idx));
}

//
// UpdateOutputInformation
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
UpdateOutputInformation()
{
  // Call superclass's version
  Superclass::UpdateOutputInformation();

  // Get the input
  const InputVideoStreamType* input = this->GetInput();

  // Get first input frame's largest possible spatial region
  SizeValueType firstInputFrameNum =
    input->GetLargestPossibleTemporalRegion().GetFrameStart();
  InputFrameSpatialRegionType inputRegion =
    input->GetFrameLargestPossibleSpatialRegion(firstInputFrameNum);

  // Propagate this spatial region to output frames
  this->GetOutput()->SetAllLargestPossibleSpatialRegions(inputRegion);
}

//-PROTECTED METHODS-----------------------------------------------------------

//
// GetInput (non-const)
//
template<typename TInputVideoStream, typename TOutputVideoStream>
TInputVideoStream*
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
GetInput()
{
  return GetInput(0);
}

//
// GetInput (non-const)
//
template<typename TInputVideoStream, typename TOutputVideoStream>
TInputVideoStream*
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
GetInput(unsigned int idx)
{
  return static_cast< InputVideoStreamType* >(this->ProcessObject::GetInput(idx));
}

//
// GenerateOutputRequestedRegion
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
GenerateOutputRequestedRegion(DataObject* output)
{
  // Call Superclass's version
  Superclass::GenerateOutputRequestedRegion(output);

  // Go through the requested temporal region and for any frame that doesn't
  // have a requested spatial region, set it to the largest possible
  SizeValueType outFrameStart =
    this->GetOutput()->GetRequestedTemporalRegion().GetFrameStart();
  SizeValueType outFrameDuration =
    this->GetOutput()->GetRequestedTemporalRegion().GetFrameDuration();
  for (SizeValueType i = outFrameStart; i < outFrameStart + outFrameDuration; ++i)
    {
    // Get the requested spatial region for this frame
    OutputFrameSpatialRegionType spatialRegion =
      this->GetOutput()->GetFrameRequestedSpatialRegion(i);

    // Check if the region has 0 size for all dimensions
    bool validRegion = false;
    for (unsigned int j = 0; j < OutputFrameType::ImageDimension; ++j)
      {
      if (spatialRegion.GetSize()[j])
        {
        validRegion = true;
        break;
        }
      }

    // If region has zero size, set it to match the largest possible region
    if (!validRegion)
      {
      this->GetOutput()->SetFrameRequestedSpatialRegion(i,
        this->GetOutput()->GetFrameLargestPossibleSpatialRegion(i));
      }
    }
}

//
// GenerateInputRequestedRegion
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
GenerateInputRequestedRegion()
{
  // Call superclass's version to propagate temporal region
  Superclass::GenerateInputRequestedRegion();

  // Get the spatial region from the output frame
  SizeValueType outputStart =
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
    SizeValueType inputStart = inRequestedTemporalRegion.GetFrameStart();
    SizeValueType numFrames = inRequestedTemporalRegion.GetFrameDuration();
    for (SizeValueType j = inputStart; j < inputStart + numFrames; ++j)
      {
      // Set the requested spatial region on the input
      input->SetFrameRequestedSpatialRegion(j, inputRegion);
      }
    }
}

//
// BeforeTemporalStreamingGenerateData
//
template<typename TInputVideoStream, typename TOutputVideoStream>
void
VideoToVideoFilter<TInputVideoStream, TOutputVideoStream>::
BeforeTemporalStreamingGenerateData()
{
  InputVideoStreamType* input = this->GetInput();
  input->SetMinimumBufferSize(this->TemporalProcessObject::m_UnitInputNumberOfFrames);
}

} // end namespace itk

#endif
