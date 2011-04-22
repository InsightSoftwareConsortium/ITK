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
#ifndef __itkVideoStream_txx
#define __itkVideoStream_txx

namespace itk
{

//-PUBLIC METHODS--------------------------------------------------------------

//
// SetFrameBuffer
//
template<class TFrameType>
void
VideoStream<TFrameType>::SetFrameBuffer(
  typename VideoStream<TFrameType>::BufferType* buffer)
{
  // We reinterpret the buffer to match TemporalDataObject's buffer type. We
  // assume that any tampering with the internal buffer will use our BufferType
  // so this will be safe.
  TemporalDataObject::BufferType* castBuffer =
    reinterpret_cast<TemporalDataObject::BufferType*>(buffer);

  if (m_DataObjectBuffer != castBuffer)
    {
    m_DataObjectBuffer = castBuffer;
    this->Modified();
    }
}

//
// InitializeEmptyFrames
//
template<class TFrameType>
void
VideoStream<TFrameType>::InitializeEmptyFrames()
{
  // Make sure the frame buffer is large enough for the number of frames needed
  // by the buffered temporal region
  unsigned long numFrames = m_BufferedTemporalRegion.GetFrameDuration();
  if (m_DataObjectBuffer->GetNumberOfBuffers() < numFrames)
    {
    m_DataObjectBuffer->SetNumberOfBuffers(numFrames);
    }

  // Go through the number of required frames and make sure none are empty. We
  // check forward in the buffer, starting at offset 1, because the frames will
  // be filled by calling AppendFrame which moves the head forward, then fills
  // the buffer.
  for (unsigned long i = 1; i <= numFrames; ++i)
    {
    if (!m_DataObjectBuffer->BufferIsFull(i))
      {
      m_DataObjectBuffer->SetBufferContents(i, FrameType::New());
      }
    }
}

//
// AppendFrame
//
template<class TFrameType>
void
VideoStream<TFrameType>::AppendFrame(TFrameType* frame)
{
  m_DataObjectBuffer->MoveHeadForward();
  m_DataObjectBuffer->SetBufferContents(0, frame);

  if (m_BufferedTemporalRegion.GetFrameDuration() < m_DataObjectBuffer->GetNumberOfBuffers())
    {
    m_BufferedTemporalRegion.SetFrameDuration(
      m_BufferedTemporalRegion.GetFrameDuration() + 1);
    }
  else
    {
    m_BufferedTemporalRegion.SetFrameStart(
      m_BufferedTemporalRegion.GetFrameStart() + 1);
    }
}

//
// GetFrame
//
template<class TFrameType>
TFrameType*
VideoStream<TFrameType>::GetFrame(int offset)
{
  // reinterpret our buffer to contain images
  BufferType* frameBuffer = reinterpret_cast<BufferType*>(m_DataObjectBuffer.GetPointer());

  // Fetch the frame
  return frameBuffer->GetBufferContents(offset);
}

//
// Graft
//
template<class TFrameType>
void
VideoStream<TFrameType>::Graft(const DataObject* data)
{
  // Call TemporalDataObject's Graft implementation
  Superclass::Graft(data);

  if (data)
    {
    // Attempt to cast to a VideoStream
    const Self* videoData = dynamic_cast< const Self* >(data);
    if (!videoData)
      {
      itkExceptionMacro( << "itk::VideoStream::Graft() cannot cast "
                         << typeid( data ).name() << " to "
                         << typeid( const Self* ).name() );
      }

    // Copy the frame buffer
    this->SetFrameBuffer(const_cast< BufferType* >(videoData->GetFrameBuffer()));
    }
}

//
// SetAllLargestPossibleSpatialRegions
//
template<class TFrameType>
void
VideoStream<TFrameType>::
SetAllLargestPossibleSpatialRegions(typename TFrameType::RegionType region)
{
  unsigned long numFrames = m_BufferedTemporalRegion.GetFrameDuration();
  if (m_DataObjectBuffer->GetNumberOfBuffers() < numFrames)
    {
    itkExceptionMacro("itk::VideoStream::SetAllLargestPossibleSpatialRegions "
                      "not enough frame buffers available. Call InitializeEmptyFrames "
                      "to prepare the frame buffer correctly.");
    }

  // Go through the number of required frames, making sure none are empty and
  // setting the region. We start at 1 and move forwards because frames will be
  // added using AppendFrame which first moves the Head forward, then adds the
  // frame
  for (unsigned long i = 1; i <= numFrames; ++i)
    {
    if (!m_DataObjectBuffer->BufferIsFull(i))
      {
      itkExceptionMacro("itk::VideoStream::SetAllLargestPossibleSpatialRegions "
                        "empty frame buffer found at offset " << i << ". Call "
                        "InitializeEmptyFrames to prepare the frame buffer correctly.");
      }
    this->SetFrameLargestPossibleSpatialRegion(i, region);
    }
}

//
// SetAllRequestedSpatialRegions
//
template<class TFrameType>
void
VideoStream<TFrameType>::
SetAllRequestedSpatialRegions(typename TFrameType::RegionType region)
{
  unsigned long numFrames = m_BufferedTemporalRegion.GetFrameDuration();
  if (m_DataObjectBuffer->GetNumberOfBuffers() < numFrames)
    {
    itkExceptionMacro("itk::VideoStream::SetAllLargestPossibleSpatialRegions "
                      "not enough frame buffers available. Call InitializeEmptyFrames "
                      "to prepare the frame buffer correctly.");
    }

  // Go through the number of required frames, making sure none are empty and
  // setting the region. We start at 1 and move forwards because frames will be
  // added using AppendFrame which first moves the Head forward, then adds the
  // frame
  for (unsigned long i = 1; i <= numFrames; ++i)
    {
    if (!m_DataObjectBuffer->BufferIsFull(i))
      {
      itkExceptionMacro("itk::VideoStream::SetAllLargestPossibleSpatialRegions "
                        "empty frame buffer found at offset " << i << ". Call "
                        "InitializeEmptyFrames to prepare the frame buffer correctly.");
      }
    this->SetFrameRequestedSpatialRegion(i, region);
    }
}

//
// SetAllBufferedSpatialRegions
//
template<class TFrameType>
void
VideoStream<TFrameType>::
SetAllBufferedSpatialRegions(typename TFrameType::RegionType region)
{
  unsigned long numFrames = m_BufferedTemporalRegion.GetFrameDuration();
  if (m_DataObjectBuffer->GetNumberOfBuffers() < numFrames)
    {
    itkExceptionMacro("itk::VideoStream::SetAllLargestPossibleSpatialRegions "
                      "not enough frame buffers available. Call InitializeEmptyFrames "
                      "to prepare the frame buffer correctly.");
    }

  // Go through the number of required frames, making sure none are empty and
  // setting the region. We start at 1 and move forwards because frames will be
  // added using AppendFrame which first moves the Head forward, then adds the
  // frame
  for (unsigned long i = 1; i <= numFrames; ++i)
    {
    if (!m_DataObjectBuffer->BufferIsFull(i))
      {
      itkExceptionMacro("itk::VideoStream::SetAllLargestPossibleSpatialRegions "
                        "empty frame buffer found at offset " << i << ". Call "
                        "InitializeEmptyFrames to prepare the frame buffer correctly.");
      }
    this->SetFrameBufferedSpatialRegion(i, region);
    }
}

//
// Allocate
//
template<class TFrameType>
void
VideoStream<TFrameType>::Allocate()
{
  unsigned long numFrames = m_BufferedTemporalRegion.GetFrameDuration();
  if (m_DataObjectBuffer->GetNumberOfBuffers() < numFrames)
    {
    itkExceptionMacro("itk::VideoStream::SetAllLargestPossibleSpatialRegions "
                      "not enough frame buffers available. Call InitializeEmptyFrames "
                      "to prepare the frame buffer correctly.");
    }

  // Go through the number of required frames, making sure none are empty and
  // allocating them. We start at 1 and move forwards because frames will be
  // added using AppendFrame which first moves the Head forward, then adds the
  // frame
  for (unsigned long i = 1; i <= numFrames; ++i)
    {
    if (!m_DataObjectBuffer->BufferIsFull(i))
      {
      itkExceptionMacro("itk::VideoStream::SetAllLargestPossibleSpatialRegions "
                        "empty frame buffer found at offset " << i << ". Call "
                        "InitializeEmptyFrames to prepare the frame buffer correctly.");
      }
    FrameType* frame = dynamic_cast<FrameType*>(
                        m_DataObjectBuffer->GetBufferContents(i).GetPointer());
    if (!frame)
      {
      itkExceptionMacro("itk::VideoStream::SetAllLargestPossibleSpatialRegions "
                        "could not cast frame " << i << " to the correct type.");
      }
    frame->Allocate();
    }
}

} // end namespace itk

#endif
