/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageToVideoFilter_hxx
#define itkImageToVideoFilter_hxx


#include "itkExtractImageFilter.h"
#include "itkMacro.h"
#include "itkRealTimeInterval.h"
#include "itkRealTimeStamp.h"
#include "itkVideoStream.h"

namespace itk
{
template <typename TInputImage, typename TOutputVideoStream>
ImageToVideoFilter<TInputImage, TOutputVideoStream>::ImageToVideoFilter()
{
  this->SetNumberOfRequiredInputs(1);
}

template <typename TInputImage, typename TOutputVideoStream>
void
ImageToVideoFilter<TInputImage, TOutputVideoStream>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <typename TInputImage, typename TOutputVideoStream>
void
ImageToVideoFilter<TInputImage, TOutputVideoStream>::SetInput(const TInputImage * image)
{
  // We keep this const_cast because in actuality, we do want to be able to
  // change the requested regions on the input so we need a non-const version
  this->SetInput(0, const_cast<InputImageType *>(image));
}

template <typename TInputImage, typename TOutputVideoStream>
void
ImageToVideoFilter<TInputImage, TOutputVideoStream>::SetInput(unsigned int idx, const TInputImage * image)
{
  // We keep this const_cast because in actuality, we do want to be able to
  // change the requested regions on the input so we need a non-const version
  this->TemporalProcessObject::SetNthInput(idx, const_cast<InputImageType *>(image));
}

template <typename TInputImage, typename TOutputVideoStream>
const TInputImage *
ImageToVideoFilter<TInputImage, TOutputVideoStream>::GetInput() const
{
  if (this->GetNumberOfInputs() < 1)
  {
    return nullptr;
  }
  return static_cast<const InputImageType *>(this->ProcessObject::GetInput(0));
}

template <typename TInputImage, typename TOutputVideoStream>
const TInputImage *
ImageToVideoFilter<TInputImage, TOutputVideoStream>::GetInput(unsigned int idx) const
{
  return static_cast<const InputImageType *>(this->ProcessObject::GetInput(idx));
}

template <typename TInputImage, typename TOutputVideoStream>
TInputImage *
ImageToVideoFilter<TInputImage, TOutputVideoStream>::GetInput()
{
  return GetInput(0);
}

template <typename TInputImage, typename TOutputVideoStream>
TInputImage *
ImageToVideoFilter<TInputImage, TOutputVideoStream>::GetInput(unsigned int idx)
{
  return static_cast<InputImageType *>(this->ProcessObject::GetInput(idx));
}

template <typename TInputImage, typename TOutputVideoStream>
void
ImageToVideoFilter<TInputImage, TOutputVideoStream>::GenerateOutputInformation()
{
  // Get the input
  const InputImageType * input = this->GetInput();

  // Get first input frame's largest possible spatial region
  InputImageRegionType                 inputRegion = input->GetLargestPossibleRegion();
  typename InputImageType::SizeType    inputSize = inputRegion.GetSize();
  typename InputImageType::IndexType   inputIndex = inputRegion.GetIndex();
  typename InputImageType::SpacingType inputSpacing = input->GetSpacing();
  typename InputImageType::PointType   inputOrigin = input->GetOrigin();

  // Set temporal frame + duration from user-defined frame axis in input image.
  // Must set temporal region first before spatial metadata can be set for each frame.
  OutputTemporalRegionType outputTemporalRegion;
  outputTemporalRegion.SetFrameStart(inputIndex[m_FrameAxis]);
  outputTemporalRegion.SetFrameDuration(inputSize[m_FrameAxis]);

  // Interpret input temporal axis spatial spacing + origin in time domain.
  RealTimeStamp realStart;
  realStart += RealTimeInterval(
    static_cast<RealTimeInterval::SecondsDifferenceType>(inputOrigin[m_FrameAxis]),
    static_cast<RealTimeInterval::MicroSecondsDifferenceType>(std::fmod(inputOrigin[m_FrameAxis], 1) * 1e6));

  auto             realDurationRaw = inputSpacing[m_FrameAxis] * inputSize[m_FrameAxis];
  RealTimeInterval realDuration(
    static_cast<RealTimeInterval::SecondsDifferenceType>(realDurationRaw),
    static_cast<RealTimeInterval::MicroSecondsDifferenceType>(std::fmod(realDurationRaw, 1) * 1e6));

  outputTemporalRegion.SetRealStart(realStart);
  outputTemporalRegion.SetRealDuration(realDuration);
  this->GetOutput()->SetLargestPossibleTemporalRegion(outputTemporalRegion);

  // Set the output spatial region from the input image's largest spatial region,
  // discarding along the user-defined image axis
  OutputFrameSpatialRegionType outputSpatialRegion;
  IndexValueType               outputIdx;
  IndexValueType               inputIdx;
  for (inputIdx = 0, outputIdx = 0; inputIdx < InputImageRegionType::ImageDimension; inputIdx++)
  {
    if (inputIdx != m_FrameAxis)
    {
      SizeValueType  axisSize = inputSize[inputIdx];
      IndexValueType axisStart = inputIndex[inputIdx];
      outputSpatialRegion.SetSize(outputIdx, axisSize);
      outputSpatialRegion.SetIndex(outputIdx, axisStart);
      ++outputIdx;
    }
  }

  // Propagate this spatial region to output frames
  this->GetOutput()->SetAllLargestPossibleSpatialRegions(outputSpatialRegion);
  this->GetOutput()->SetRequestedRegionToLargestPossibleRegion();

  // Propagate physical spacing and origin
  typename OutputVideoStreamType::FrameType::SpacingType outputSpacing;
  typename OutputVideoStreamType::FrameType::PointType   outputOrigin;
  for (inputIdx = 0, outputIdx = 0; inputIdx < InputImageType::ImageDimension; inputIdx++)
  {
    if (inputIdx != m_FrameAxis)
    {
      outputSpacing.SetElement(outputIdx, inputSpacing[inputIdx]);
      outputOrigin.SetElement(outputIdx, inputOrigin[inputIdx]);
      ++outputIdx;
    }
  }
  this->GetOutput()->SetAllFramesSpacing(outputSpacing);

  // Propagate spatial direction
  typename InputImageType::DirectionType                   inputDirection = input->GetDirection();
  typename OutputVideoStreamType::FrameType::DirectionType outputDirection;
  for (IndexValueType iinput = 0, ioutput = 0; iinput < InputImageType::ImageDimension; ++iinput)
  {
    for (IndexValueType jinput = 0, joutput = 0; jinput < InputImageType::ImageDimension; ++jinput)
    {
      if (iinput != m_FrameAxis && jinput != m_FrameAxis)
      {
        outputDirection(ioutput, joutput) = inputDirection(iinput, jinput);
        ++joutput;
      }
      // Check that time is axis-aligned and accessor index matches spatial axis order
      else if (iinput == jinput)
      {
        itkAssertOrThrowMacro(inputDirection(iinput, jinput) == 1, "Not axis aligned!");
      }
      else
      {
        itkAssertOrThrowMacro(inputDirection(iinput, jinput) == 0, "Not axis aligned!");
      }
    }
    if (iinput != m_FrameAxis)
    {
      ++ioutput;
    }
  }
  this->GetOutput()->SetAllFramesDirection(outputDirection);

  // Propagate pixel components (for vector images)
  this->GetOutput()->SetAllFramesNumberOfComponentsPerPixel(input->GetNumberOfComponentsPerPixel());
}

// The default implementation of UpdateOutputInformation in VideoSource attempts to set the
// largest possible output temporal region from the input, but this is best handled
// in GenerateOutputInformation(). Here we override to call the base
// UpdateOutputInformation() implementation in ProcessObject.
template <typename TInputImage, typename TOutputVideoStream>
void
ImageToVideoFilter<TInputImage, TOutputVideoStream>::UpdateOutputInformation()
{
  ProcessObject::UpdateOutputInformation();
}

template <typename TInputImage, typename TOutputVideoStream>
void
ImageToVideoFilter<TInputImage, TOutputVideoStream>::GenerateOutputRequestedRegion(DataObject * output)
{

  // Check that output is a VideoStream object
  auto * vsOutput = dynamic_cast<OutputVideoStreamType *>(output);
  if (vsOutput == nullptr)
  {
    itkExceptionMacro(<< "itk::ImageToVideoFilter::GenerateOutputRequestedRegion() "
                      << "cannot cast " << typeid(output).name() << " to " << typeid(OutputVideoStreamType *).name());
  }

  vsOutput->SetRequestedTemporalRegion(vsOutput->GetLargestPossibleTemporalRegion());

  // Go through the requested temporal region and for any frame that doesn't
  // have a requested spatial region, set it to the largest possible
  SizeValueType outFrameStart = vsOutput->GetRequestedTemporalRegion().GetFrameStart();
  SizeValueType outFrameDuration = vsOutput->GetRequestedTemporalRegion().GetFrameDuration();
  for (SizeValueType i = outFrameStart; i < outFrameStart + outFrameDuration; ++i)
  {
    // Get the requested spatial region for this frame
    OutputFrameSpatialRegionType spatialRegion = vsOutput->GetFrameRequestedSpatialRegion(i);

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
      vsOutput->SetFrameRequestedSpatialRegion(i, vsOutput->GetFrameLargestPossibleSpatialRegion(i));
    }
  }
}

template <typename TInputImage, typename TOutputVideoStream>
void
ImageToVideoFilter<TInputImage, TOutputVideoStream>::GenerateInputRequestedRegion()
{
  this->GetInput()->SetRequestedRegion(this->GetInput()->GetLargestPossibleRegion());
}

template <typename TInputImage, typename TOutputVideoStream>
void
ImageToVideoFilter<TInputImage, TOutputVideoStream>::GenerateData()
{
  // Rely on SuperClass implementation to allocate output frames
  this->AllocateOutputs();

  // Set each frame in output to an image slice in the input image
  InputImageType *     input = this->GetInput();
  InputImageRegionType inputRegion = input->GetLargestPossibleRegion();

  // Graft input image slices onto output frames
  OutputVideoStreamType * output = this->GetOutput();
  SizeValueType           outputStartFrame = output->GetRequestedTemporalRegion().GetFrameStart();
  SizeValueType           outputDuration = output->GetRequestedTemporalRegion().GetFrameDuration();
  for (auto idx = outputStartFrame; idx < outputStartFrame + outputDuration; idx++)
  {
    InputImageRegionType inputSliceRegion = inputRegion;
    inputSliceRegion.SetSize(m_FrameAxis, 0);
    inputSliceRegion.SetIndex(m_FrameAxis, idx);

    using ExtractFilterType = typename itk::ExtractImageFilter<InputImageType, OutputFrameType>;
    auto extractFilter = ExtractFilterType::New();
    extractFilter->SetDirectionCollapseToSubmatrix();

    extractFilter->SetInput(input);
    extractFilter->SetExtractionRegion(inputSliceRegion);
    extractFilter->Update();

    output->GetFrame(idx)->Graft(extractFilter->GetOutput());
  }
}
} // end namespace itk

#endif // itkImageToVideoFilter_hxx
