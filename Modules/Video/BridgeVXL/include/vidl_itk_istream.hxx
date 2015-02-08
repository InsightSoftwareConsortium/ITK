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
#ifndef __vidl_itk_istream_hxx
#define __vidl_itk_istream_hxx

#include "itkNumericTraits.h"
#include "vidl_itk_istream.h"

namespace itk
{

//
// Constructor
//
template< typename TVideoStream >
vidl_itk_istream< TVideoStream >::vidl_itk_istream()
{
  m_VideoStream = ITK_NULLPTR;
  m_AdvanceCalled = false;
}

//
// Constructor from VideoStream
//
template< typename TVideoStream >
vidl_itk_istream< TVideoStream >::vidl_itk_istream(TVideoStream* videoStream)
{
  m_VideoStream = videoStream;
  m_AdvanceCalled = false;
}

//
// open
//
template< typename TVideoStream >
bool
vidl_itk_istream< TVideoStream >::open(TVideoStream* videoStream)
{
  m_VideoStream = videoStream;
  return (m_VideoStream == ITK_NULLPTR);
}

//
// is_valid
//
template< typename TVideoStream >
bool
vidl_itk_istream< TVideoStream >::is_valid() const
{
  // Make sure the image is 2D
  if (Dimensions != 2)
    {
    return false;
    }

  return (m_VideoStream == ITK_NULLPTR || m_AdvanceCalled);
}

//
// is_seekable
//
template< typename TVideoStream >
bool
vidl_itk_istream< TVideoStream >::is_seekable() const
{
  // return false if not open
  if (!this->is_open())
    {
    return false;
    }

  // Make sure largest region information is available
  m_VideoStream->UpdateOutputInformation();

  // Return false if duration of largest possible temporal region is infinite,
  // true otherwise
  return (m_VideoStream->GetLargestPossibleTemporalRegion().GetFrameDuration() !=
          ITK_INFINITE_FRAME_DURATION);
}

//
// num_frames
//
template< typename TVideoStream >
int
vidl_itk_istream< TVideoStream >::num_frames() const
{
  // return -1 if not open or not seekable (this calls UpdateOutputInformation)
  if (!this->is_open() || !this->is_seekable())
    {
    return -1;
    }

  // return the frame duration of the largest possible temporal region
  return m_VideoStream->GetLargestPossibleTemporalRegion().GetFrameDuration();
}

//
// frame_number
//
template< typename TVideoStream >
unsigned int
vidl_itk_istream< TVideoStream >::frame_number() const
{
  // return static_cast<unsigned int>(-1) if not open or not valid
  if (!this->is_open() || !this->is_valid())
    {
    return static_cast<unsigned int>(-1);
    }

  // return the frame number from the start of the requested temporal region
  return m_VideoStream->GetRequestedTemporalRegion().GetFrameStart();
}

//
// width
//
template< typename TVideoStream >
unsigned int
vidl_itk_istream< TVideoStream >::width() const
{
  // return false if not open
  if (!this->is_open())
    {
    return -1;
    }

  // Make sure largest region information is available
  m_VideoStream->UpdateOutputInformation();

  // return the width of the largest possible spatial region
  return m_VideoStream->GetFrameLargestPossibleSpatialRegion(0).GetSize()[0];
}

//
// height
//
template< typename TVideoStream >
unsigned int
vidl_itk_istream< TVideoStream >::height() const
{
  // return false if not open
  if (!this->is_open())
    {
    return -1;
    }

  // Make sure largest region information is available
  m_VideoStream->UpdateOutputInformation();

  // return the width of the largest possible spatial region
  return m_VideoStream->GetFrameLargestPossibleSpatialRegion(0).GetSize()[1];
}

//
// format
//
template< typename TVideoStream >
vidl_pixel_format
vidl_itk_istream< TVideoStream >::format() const
{
  // We need to know about the primitive type used for the pixel
  typedef typename itk::NumericTraits< PixelType >::ValueType PixelValueType;

  // Get the number of challenls for the pixel
  unsigned int channels = itk::NumericTraits< PixelType >::MeasurementVectorType::Dimension;

  //
  // Return the proper format based on typeid and number of channels
  //

  // bool
  if (typeid(PixelValueType) == typeid(bool))
    {
    if (channels == 1)
      {
      return VIDL_PIXEL_FORMAT_MONO_1;
      }
    else
      {
      return VIDL_PIXEL_FORMAT_UNKNOWN;
      }
    }

  // char / unsigned char
  else if (typeid(PixelValueType) == typeid(char) ||
           typeid(PixelValueType) == typeid(unsigned char))
    {
    if (channels == 1)
      {
      return VIDL_PIXEL_FORMAT_MONO_8;
      }
    else if (channels == 3)
      {
      return VIDL_PIXEL_FORMAT_RGB_24;
      }
    else if (channels == 4)
      {
      return VIDL_PIXEL_FORMAT_RGBA_32;
      }
    else
      {
      return VIDL_PIXEL_FORMAT_UNKNOWN;
      }
    }

  // short / unsigned short
  else if (typeid(PixelValueType) == typeid(short) ||
           typeid(PixelValueType) == typeid(unsigned short))
    {
    if (channels == 1)
      {
      return VIDL_PIXEL_FORMAT_MONO_16;
      }
    else
      {
      return VIDL_PIXEL_FORMAT_UNKNOWN;
      }
    }

  // float
  else if (typeid(PixelValueType) == typeid(float))
    {
    if (channels == 1)
      {
      return VIDL_PIXEL_FORMAT_MONO_F32;
      }
    else if (channels == 3)
      {
      return VIDL_PIXEL_FORMAT_RGB_F32;
      }
    else
      {
      return VIDL_PIXEL_FORMAT_UNKNOWN;
      }
    }

  // unknown
  else
    {
    return VIDL_PIXEL_FORMAT_UNKNOWN;
    }
}

//
// duration
//
template< typename TVideoStream >
double
vidl_itk_istream< TVideoStream >::duration() const
{
  // return false if not open
  if (!this->is_open())
    {
    return -1;
    }

  // Make sure largest region information is available
  m_VideoStream->UpdateOutputInformation();

  // return the real time duration of the largest temporal region in seconds
  // NOTE: until real time is properly handled in itk video, this won't return
  //       a correct value
  itkWarningMacro("Real Time is not currently supported correctly in itk video. Result is "
                  "likely to be inaccurate.");
  return m_VideoStream->GetLargestPossibleTemporalRegion().GetRealDuration().GetTimeInSeconds();
}

//
// advance
//
template< typename TVideoStream >
bool
vidl_itk_istream< TVideoStream >::advance()
{
  // Make sure it's open
  if (!this->is_open())
    {
    return false;
    }

  // Update largest region information
  m_VideoStream->UpdateOutputInformation();

  // We re-implement seeking forward because we want to be able to advance even
  // if the duration is infinite (and hence the video isn't seekable)
  TemporalRegion currentRequest = m_VideoStream->GetRequestedTemporalRegion();
  FrameOffsetType currentFrame = currentRequest.GetFrameStart();

  // we can't advance if we're at the end
  itk::IdentifierType firstFrame = m_VideoStream->GetLargestPossibleTemporalRegion().GetFrameStart();
  FrameOffsetType frameDuration = m_VideoStream->GetLargestPossibleTemporalRegion().GetFrameDuration();
  if (frameDuration == 0 || currentFrame >= firstFrame + frameDuration - 2)
    {
    return false;
    }

  // If we haven't called advance yet, go to the first frame
  if (!m_AdvanceCalled)
    {
    currentRequest.SetFrameStart(firstFrame);
    m_AdvanceCalled = true;
    }
  // otherwise, move forward one frame
  else
    {
    currentRequest.SetFrameStart(currentFrame + 1);
    }
  currentRequest.SetFrameDuration(1);
  m_VideoStream->SetRequestedTemporalRegion(currentRequest);

  // Return success
  return true;
}

//
// read_frame
//
template< typename TVideoStream >
vidl_frame_sptr
vidl_itk_istream< TVideoStream >::read_frame()
{
  if (this->advance())
    {
    return this->current_frame();
    }
  else
    {
    return ITK_NULLPTR;
    }
}

//
// current_frame
//
template< typename TVideoStream >
vidl_frame_sptr
vidl_itk_istream< TVideoStream >::current_frame()
{
  // Return if not valid
  if (!this->is_valid())
    {
    return ITK_NULLPTR;
    }

  // Make sure the VideoSource's data is up to date
  m_VideoStream->Update();

  // Get the frame
  FrameType* frame = m_VideoStream->GetFrame(this->frame_number());

  // Set up the output vidl_frame_sptr
  vidl_frame_sptr output_frame = new vidl_shared_frame(static_cast<void*>(frame->GetBufferPointer()),
    this->width(), this->height(), this->format());

  // Return frame
  return output_frame;
}

//
// seek_frame
//
template< typename TVideoStream >
bool
vidl_itk_istream< TVideoStream >::seek_frame(unsigned int frameNumber)
{
  // return false if not open, not seekable, or frame number out of bounds
  if (!this->is_open() || !this->is_seekable() ||
      frameNumber >= static_cast<unsigned int>(this->num_frames()))
    {
    return false;
    }

  // set the start of the requested temporal region to frameNumber and the
  // duration to 1
  TemporalRegion request = m_VideoStream->GetRequestedTemporalRegion();
  request.SetFrameStart(frameNumber);
  request.SetFrameDuration(1);
  m_VideoStream->SetRequestedTemporalRegion(request);
  return true;
}


} // end namespace itk

#endif
