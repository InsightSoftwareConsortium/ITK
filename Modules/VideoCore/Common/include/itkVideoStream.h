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
#ifndef __itkVideoStream_h
#define __itkVideoStream_h

#include "itkTemporalDataObject.h"
#include "itkImage.h"

namespace itk
{

/** \class VideoStream
 * \brief A DataObject that holds a buffered portion of a video
 *
 * The function of VideoStream is to provide an Image-specific subclass of
 * TemporalDataObject. It provides several convenient typedefs to get common
 * attributes of the frames.
 */
template<class TFrameType>
class ITK_EXPORT VideoStream : public TemporalDataObject
{
public:

  /** Standard class typedefs */
  typedef VideoStream                Self;
  typedef TemporalDataObject         Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef WeakPointer< const Self >  ConstWeakPointer;

  typedef TFrameType                 FrameType;
  typedef RingBuffer<FrameType>      BufferType;

  typedef typename FrameType::RegionType    SpatialRegionType;
  typedef typename FrameType::IndexType     IndexType;
  typedef typename FrameType::PixelType     PixelType;
  typedef typename FrameType::PointType     PointType;
  typedef typename FrameType::SpacingType   SpacingType;
  typedef typename FrameType::SizeType      SizeType;
  typedef typename FrameType::DirectionType DirectionType;

  /** Access the spacial dimensionality of the frames */
  itkStaticConstMacro(FrameDimension, unsigned int, FrameType::ImageDimension);
  static unsigned int GetFrameDimension()
    { return FrameType::ImageDimension; }

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VideoStream, TemporalDataObject);

  /** Provide access to the internal frame buffer object */
  BufferType* GetFrameBuffer()
    { return reinterpret_cast<BufferType*>(m_DataObjectBuffer.GetPointer()); }
  const BufferType* GetFrameBuffer() const
    { return reinterpret_cast<BufferType*>(m_DataObjectBuffer.GetPointer()); }

  /** Set the internal pixel buffer */
  void SetFrameBuffer(BufferType* buffer);

  /** Append the supplied frame to the end of the video */
  void AppendFrame(FrameType* frame);

  /** Get the frame at the given offset from the current head. A negative
   * offset means backward in time while a positive offset means forward in
   * time. For video streams that have been filled from oldest frame to newest,
   * the current frame will be the latest in time, so only negative offsets
   * will return the expected result. */
  FrameType* GetFrame(int offset);

  /** Set the LargestPossibleRegion of a frame */
  void SetFrameLargestPossibleSpatialRegion(int offset, SpatialRegionType region)
    { this->GetFrame(offset)->SetLargestPossibleRegion(region); }

  /** Set the RequestedRegion of a frame */
  void SetFrameRequestedSpatialRegion(int offset, SpatialRegionType region)
    { this->GetFrame(offset)->SetRequestedRegion(region); }

  /** Set the BufferedRegion of a frame */
  void SetFrameBufferedSpatialRegion(int offset, SpatialRegionType region)
    { this->GetFrame(offset)->SetBufferedRegion(region); }

  /** Graft the data and information from one VideoStream to this one. This
   * just copies the meta information using TemporalProcessObject's Graft then
   * sets the internal RingBuffer pointer to point to the same buffer used by
   * the other VideoStream. */
  virtual void Graft(const DataObject* data);

protected:

  VideoStream() {};
  virtual ~VideoStream() {};
  virtual void PrintSelf(std::ostream & os, Indent indent) const
    { Superclass::Print(os, indent); }

private:

  VideoStream(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented

};  // end class VideoStream

} // end namespace itk

#if ITK_TEMPLATE_TXX
#include "itkVideoStream.txx"
#endif

#endif
