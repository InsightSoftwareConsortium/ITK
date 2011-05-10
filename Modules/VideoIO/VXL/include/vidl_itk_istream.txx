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
#ifndef __itk_vidl_istream_txx
#define __itk_vidl_istream_txx

namespace itk
{

//
// Constructor
//
template< class TVideoStream >
vidl_itk_istream< TVideoStream >::vidl_itk_istream()
{
  m_VideoStream = NULL;
  m_AdvanceCalled = false;
}

//
// Constructor from VideoStream
//
template< class TVideoStream >
vidl_itk_istream< TVideoStream >::vidl_itk_istream(TVideoStream* videoStream)
{
  m_VideoStream = videoStream;
  m_AdvanceCalled = false;
}

//
// open
//
template< class TVideoStream >
bool
vidl_itk_istream< TVideoStream >::open(TVideoStream* videoStream)
{
  m_VideoStream = videoStream;
  return videoStream == NULL;
}

//
// is_valid
//
template< class TVideoStream >
bool
vidl_itk_istream< TVideoStream >::is_valid() const
{
  return (m_VideoStream == NULL || m_AdvanceCalled);
}

//
// is_seekable
//
template< class TVideoStream >
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
template< class TVideoStream >
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
template< class TVideoStream >
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
template< class TVideoStream >
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
template< class TVideoStream >
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
template< class TVideoStream >
vidl_pixel_format
vidl_itk_istream< TVideoStream >::format() const
{
  itkWarningMacro("STUB");
  return (vidl_pixel_format)0;
}

//
// duration
//
template< class TVideoStream >
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
template< class TVideoStream >
bool
vidl_itk_istream< TVideoStream >::advance()
{
  itkWarningMacro("STUB");
  return false;
}

//
// read_frame
//
template< class TVideoStream >
vidl_frame_sptr
vidl_itk_istream< TVideoStream >::read_frame()
{
  this->advance();
  return this->current_frame();
}

//
// current_frame
//
template< class TVideoStream >
vidl_frame_sptr
vidl_itk_istream< TVideoStream >::current_frame()
{
  itkWarningMacro("STUB");
  return NULL;
}

//
// seek_frame
//
template< class TVideoStream >
bool
vidl_itk_istream< TVideoStream >::seek_frame(unsigned int frame_number)
{
  // return false if not open, not seekable, or frame number out of bounds
  if (!this->is_open() || !this->is_seekable() ||
      frame_number >= static_cast<unsigned int>(this->num_frames()))
    {
    return false;
    }

  // set the start of the requested temporal region to frame_number and the
  // duration to 1
  TemporalRegion request = m_VideoStream->GetRequestedTemporalRegion();
  request.SetFrameStart(frame_number);
  request.SetFrameDuration(1);
  m_VideoStream->SetRequestedTemporalRegion(request);
  return true;

}


} // end namespace itk

#endif
