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

#ifndef itkVideoFileWriter_hxx
#define itkVideoFileWriter_hxx

#include "itkVideoFileWriter.h"

#include "itkNumericTraits.h"
#include "itkTemporalDataObject.h"
#include "itkMath.h"

namespace itk
{

template< typename TInputVideoStream >
VideoFileWriter< TInputVideoStream >
::VideoFileWriter() :
  m_FileName(""),
  m_VideoIO(ITK_NULLPTR),
  m_FramesPerSecond(24),
  m_FourCC("MP42"),
  m_NumberOfComponents(0)
{
  // TemporalProcessObject inherited members
  this->TemporalProcessObject::m_UnitInputNumberOfFrames = 1;
  this->TemporalProcessObject::m_UnitOutputNumberOfFrames = 1;
  this->TemporalProcessObject::m_FrameSkipPerOutput = 1;
  this->TemporalProcessObject::m_InputStencilCurrentFrameIndex = 0;
}

template< typename TInputVideoStream >
VideoFileWriter< TInputVideoStream >
::~VideoFileWriter()
{
  this->FinishWriting();
}

template< typename TInputVideoStream >
void
VideoFileWriter< TInputVideoStream >
::SetInput( const VideoStreamType* input )
{
  this->ProcessObject::SetNthInput(0, const_cast<VideoStreamType*>(input));
}

template< typename TInputVideoStream >
const typename VideoFileWriter< TInputVideoStream >::VideoStreamType*
VideoFileWriter< TInputVideoStream >
::GetInput()
{
  if (this->GetNumberOfInputs() < 1)
    {
    return ITK_NULLPTR;
    }

  return static_cast<VideoStreamType*>(this->ProcessObject::GetInput(0));
}

template< typename TInputVideoStream >
void
VideoFileWriter< TInputVideoStream >
::SetVideoIO(IOBasePointer videoIO)
{
  m_VideoIO = videoIO;
}

template< typename TInputVideoStream >
void
VideoFileWriter< TInputVideoStream >
::Write()
{
  //
  // Make sure everything is set correctly
  //

  // Make sure input is available
  const VideoStreamType* input = this->GetInput();
  if (input == ITK_NULLPTR)
    {
    itkExceptionMacro("No input to writer");
    }

  // Make sure FileName is specified
  if (m_FileName == "")
    {
    itkExceptionMacro("No FileName set for writer");
    }

  // Make sure FramesPerSecond and FourCC have been set
  if (Math::ExactlyEquals(m_FramesPerSecond, NumericTraits< TemporalRatioType >::ZeroValue()) || m_FourCC.length() == 0)
    {
    itkExceptionMacro("Cannot write with FramesPerSecond or FourCC unset");
    }


  //
  // Trigger the first phase of the pipeline. We don't need to propagate the
  // requested region because that will get redone during the data generation
  // phase using temporal streaming.
  //

  // Update the output information upstream
  VideoStreamType* nonConstInput = const_cast<VideoStreamType*>(this->GetInput());
  nonConstInput->UpdateOutputInformation();


  //
  // Set up the writer using information propagated from upstream
  //

  // Initialize writing information
  if (!this->InitializeOutputParameters())
    {
    itkExceptionMacro("Could not initialize output parameters for writing");
    }

  // Initialize VideoIO if necessary
  if (m_VideoIO.IsNull() && !this->InitializeVideoIO())
    {
    itkExceptionMacro("Could not create VideoIO");
    }

  // Set output information
  m_VideoIO->SetWriterParameters( m_FramesPerSecond,
                                  m_Dimensions,
                                  m_FourCC.c_str(),
                                  m_NumberOfComponents,
                                  m_ComponentType );
  m_VideoIO->SetFileName(m_FileName);

  // If no OutputTemporalRegion specified, get the largest one possible and
  // use that.
  if (m_OutputTemporalRegion.GetFrameDuration() == 0)
    {
    m_OutputTemporalRegion = input->GetLargestPossibleTemporalRegion();
    }

  // FIXME: For now we will always request the entire spatial region of each
  // frame as output
  SizeValueType frameStart = m_OutputTemporalRegion.GetFrameStart();
  SizeValueType numFrames = m_OutputTemporalRegion.GetFrameDuration();
  for (SizeValueType i = frameStart; i < frameStart + numFrames; ++i)
    {
    nonConstInput->SetFrameRequestedSpatialRegion(i,
      input->GetFrameLargestPossibleSpatialRegion(i));
    }

  // Propagate the requested regions
  nonConstInput->PropagateRequestedRegion();

  // Set the output's temporal regions to match the input's. The output object
  // doesn't actually get used, but its temporal regions are used by the
  // temporal streaming in TemporalProcessObject's GenerateData. We set the
  // buffered region to an empty region so that the entire requested region is
  // unbuffered and gets written.
  TemporalDataObject::Pointer output = TemporalDataObject::New();
  output->SetLargestPossibleTemporalRegion(m_OutputTemporalRegion);
  output->SetRequestedTemporalRegion(m_OutputTemporalRegion);
  output->SetBufferedTemporalRegion(TemporalRegion());
  this->ProcessObject::SetNthOutput(0, output);


  //
  // Trigger the data generation phase of the pipeline
  //

  // Notify observers
  this->InvokeEvent( StartEvent() );

  // Write the data
  this->GenerateData();

  // Notify observers
  this->InvokeEvent( EndEvent() );

  // Finish writing the file
  this->FinishWriting();
}

template< typename TInputVideoStream >
void
VideoFileWriter< TInputVideoStream >
::FinishWriting()
{
  if (!m_VideoIO.IsNull())
    {
    m_VideoIO->FinishReadingOrWriting();
    }
}

template< typename TInputVideoStream >
void
VideoFileWriter< TInputVideoStream >
::Update()
{
  this->Write();
}

template< typename TInputVideoStream >
void
VideoFileWriter< TInputVideoStream >
::UpdateLargestPossibleRegion()
{
  const VideoStreamType* input = this->GetInput();
  if (input == ITK_NULLPTR)
    {
    itkExceptionMacro("No input to writer");
    }
  m_OutputTemporalRegion = input->GetLargestPossibleTemporalRegion();
  this->Write();
}

template< typename TInputVideoStream >
void
VideoFileWriter< TInputVideoStream >
::TemporalStreamingGenerateData()
{
  // Get a non-const pointer to the input and output
  const VideoStreamType* input = dynamic_cast<const VideoStreamType*>(this->GetInput());
  TemporalDataObject* output = dynamic_cast<TemporalDataObject*>(this->GetOutput(0));
  if (!output)
    {
    itkExceptionMacro("Could not cast output to TemporalDataObject");
    }

  // Get the frame we're going to write
  SizeValueType frameNum = output->GetRequestedTemporalRegion().GetFrameStart();
  const FrameType* frame = input->GetFrame(frameNum);
  if (!frame)
    {
    itkExceptionMacro("Could not get input frame " << frameNum << " for writing");
    }

  // Write the frame out
  m_VideoIO->Write(static_cast<const void*>(frame->GetBufferPointer()));
}

template< typename TInputVideoStream >
bool
VideoFileWriter< TInputVideoStream >
::InitializeOutputParameters()
{
  // InputImage and VideoIO must be valid
  if (this->GetInput() == ITK_NULLPTR)
    {
    return false;
    }

  // Get the frame number for the current frame
  SizeValueType frameNum = this->GetInput()->GetRequestedTemporalRegion().GetFrameStart();

  // Get a non-const pointer so we can get spatial regions (VideoStream isn't const correct)
  const VideoStreamType* input = this->GetInput();

  // Set dimensions
  m_Dimensions.empty();
  typename FrameType::SizeType size =
    input->GetFrameLargestPossibleSpatialRegion(frameNum).GetSize();
  for (unsigned int i = 0; i < FrameType::ImageDimension; ++i)
    {
    m_Dimensions.push_back(size[i]);
    }

  // Set NumberOfComponents. At this point, just handle RGB and RGBA
  // non-scalar pixels
  m_NumberOfComponents =
    itk::NumericTraits<PixelType>::MeasurementVectorType::Length;

  return true;
}

template< typename TInputVideoStream >
bool
VideoFileWriter< TInputVideoStream >
::InitializeVideoIO()
{
  if (m_FileName.length() != 0)
    {
    m_VideoIO = itk::VideoIOFactory::CreateVideoIO(
                             itk::VideoIOFactory::WriteMode, m_FileName.c_str());

    // Return true if a VideoIO was successfully created
    if (!m_VideoIO.IsNull())
      {
      // Get the pixel type
      m_ComponentType = m_VideoIO->GetComponentType();

      return true;
      }
    else
      {
      return false;
      }
    }
  else
    {
    return false;
    }
}

template< typename TInputVideoStream >
void
VideoFileWriter< TInputVideoStream >
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << this->m_FileName << std::endl;
  if (!m_VideoIO.IsNull())
    {
    os << indent << "VideoIO:" << std::endl;
   this->m_VideoIO->Print(os, indent.GetNextIndent());
    }
}

} // end namespace itk

#endif
