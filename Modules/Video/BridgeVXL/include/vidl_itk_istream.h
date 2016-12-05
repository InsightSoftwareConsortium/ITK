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
#ifndef __vidl_itk_istream_h
#define __vidl_itk_istream_h

#include "vidl/vidl_istream.h"
#include "itkVideoStream.h"

namespace itk
{

/** \class vidl_itk_istream
 * \brief implementation of VXL's vidl_istream using an itk VideoStream
 *
 * This implementation of VXL's vidl_istream can be placed at the end of an ITK
 * video pipeline and allow the processed frames to be accessed in the standard
 * VXL istream pattern.
 *
 * TODO: Move this to Video-Core-VXL
 *
 * \ingroup ITKVideoBridgeVXL
 */
template< typename TVideoStream >
class ITK_TEMPLATE_EXPORT vidl_itk_istream : public vidl_istream
{
public:

  /**-CONSTRUCTORS AND DESTRUCTOR--------------------------------------------*/

  /** Typedefs */
  typedef TVideoStream                        VideoStreamType;
  typedef vidl_itk_istream< VideoStreamType > Self;
  typedef typename VideoStreamType::FrameType FrameType;
  typedef ::itk::SizeValueType                FrameOffsetType;
  typedef typename FrameType::PixelType       PixelType;
  static ITK_CONSTEXPR_VAR unsigned int Dimensions =      FrameType::ImageDimension;

  /** Constructor - default */
  vidl_itk_istream();

  /** Constructor - from a VideoStream */
  vidl_itk_istream(VideoStreamType* videoStream);

  /** Destructor */
  virtual ~vidl_itk_istream() {}

  /** ITK's type info */
  itkTypeMacro(vidl_itk_istream, vidl_istream);


  /**-OPEN CLOSE-------------------------------------------------------------*/

  /** Open from a VideoStream */
  virtual bool open(VideoStreamType* videoStream);

  /** Close the stream. For our purposes, this just means set the VideoStream
   * pointer to ITK_NULLPTR */
  virtual void close() { m_VideoStream = ITK_NULLPTR; }


  /**-STREAM INFORMATION-----------------------------------------------------*/

  /** Return whether or not the VideoStream is null */
  virtual bool is_open() const { return m_VideoStream != ITK_NULLPTR; }

  /** Return true if the stream is in a valid state. To comply with vxl's
   * standard, this will return false until advance() has been called at least
   * once. */
  virtual bool is_valid() const;

  /** Return whether or not the stream is seekable. For us, it is only not
   * seekable if the largest possible temporal region has infinite duration */
  virtual bool is_seekable() const;

  /** Return the nuber of frames. Returns -1 if non-seekable */
  virtual int num_frames() const;

  /** Return the current frame number. Before advance() has been called, this
   * will return static_cast<unsigned int>(-1) */
  virtual unsigned int frame_number() const;

  /** Frame width */
  virtual unsigned int width() const;

  /** Frame height */
  virtual unsigned int height() const;

  /** Return pixel type as a vidl_pixel_format */
  virtual vidl_pixel_format format() const;

  /** Return frame rate. For use this will always return 0.0 since we don't use
   * a fixed framerate for VideoStream */
  virtual double frame_rate() const { return 0.0; }

  /** Return duration in seconds */
  virtual double duration() const;


  /**-STREAM MANIPULATION----------------------------------------------------*/

  /** Advance to the next frame but don't update the VideoStream */
  virtual bool advance();

  /** Return the next frame from the stream (advance and update) */
  virtual vidl_frame_sptr read_frame();

  /** Return the current frame (update) */
  virtual vidl_frame_sptr current_frame();

  /** Seek to the given frame */
  virtual bool seek_frame(unsigned int frameNumber);

protected:

  /** Internal VideoStream pointer */
  VideoStreamType* m_VideoStream;

  /** Keep track of whether or not advance has been called yet */
  bool m_AdvanceCalled;

};  // end class vidl_itk_istream


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "vidl_itk_istream.hxx"
#endif

#endif
