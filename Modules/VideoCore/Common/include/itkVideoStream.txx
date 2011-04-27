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
// SetFrameLargestPossibleSpatialRegion
//
template<class TFrameType>
void
VideoStream<TFrameType>::SetFrameLargestPossibleSpatialRegion(
  unsigned long frameNumber, typename TFrameType::RegionType region)
{
  m_LargestPossibleSpatialRegionCache[frameNumber] = region;

  // If the frame is currently buffered, set the actual frame's region
  unsigned long bufStart = m_BufferedTemporalRegion.GetFrameStart();
  unsigned long bufDur = m_BufferedTemporalRegion.GetFrameDuration();
  if (frameNumber >= bufStart && frameNumber < bufStart + bufDur)
    {
    FrameType* frame = this->GetFrame(frameNumber);
    frame->SetLargestPossibleRegion(region);
    }
}

//
// SetFrameRequestedSpatialRegion
//
template<class TFrameType>
void
VideoStream<TFrameType>::SetFrameRequestedSpatialRegion(
  unsigned long frameNumber, typename TFrameType::RegionType region)
{
  m_RequestedSpatialRegionCache[frameNumber] = region;

  // If the frame is currently buffered, set the actual frame's region
  unsigned long bufStart = m_BufferedTemporalRegion.GetFrameStart();
  unsigned long bufDur = m_BufferedTemporalRegion.GetFrameDuration();
  if (frameNumber >= bufStart && frameNumber < bufStart + bufDur)
    {
    FrameType* frame = this->GetFrame(frameNumber);
    frame->SetRequestedRegion(region);
    }
}

//
// SetFrameBufferedSpatialRegion
//
template<class TFrameType>
void
VideoStream<TFrameType>::SetFrameBufferedSpatialRegion(
  unsigned long frameNumber, typename TFrameType::RegionType region)
{
  m_BufferedSpatialRegionCache[frameNumber] = region;

  // If the frame is currently buffered, set the actual frame's region
  unsigned long bufStart = m_BufferedTemporalRegion.GetFrameStart();
  unsigned long bufDur = m_BufferedTemporalRegion.GetFrameDuration();
  if (frameNumber >= bufStart && frameNumber < bufStart + bufDur)
    {
    FrameType* frame = this->GetFrame(frameNumber);
    frame->SetBufferedRegion(region);
    }
}

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
  // If we don't have any frames requested, just return
  unsigned long numFrames = m_RequestedTemporalRegion.GetFrameDuration();
  if (numFrames == 0)
    {
    return;
    }

  // If we don't have enough buffer space to handle the number of requested
  // frames, we need to resize the ring buffer. Just resizing can cause data to
  // be in the wrong place. For example if the head of the buffer is at index 0
  // and the buffer has 3 slots, setting frame number 3 will actually place the
  // data into slot 0.  If we then resize the buffer to have 4 slots, the data
  // for frame 3 will live in slot 0 even though a request for frame 3 will
  // return the data from the newly created slot 3. To circumvent this problem,
  // we move the buffered data to the proper indices in the ring buffer after
  // resizing.
  if (m_DataObjectBuffer->GetNumberOfBuffers() < numFrames)
    {
    // Save the indices of all frames in the currently buffered region
    unsigned long bufferedStart = m_BufferedTemporalRegion.GetFrameStart();
    unsigned long bufferedDuration = m_BufferedTemporalRegion.GetFrameDuration();
    std::map<unsigned long, DataObject*> frameNumPtrMap;
    for (unsigned long i = bufferedStart; i < bufferedStart + bufferedDuration; ++i)
      {
      frameNumPtrMap[i] = m_DataObjectBuffer->GetBufferContents(i);
      //m_DataObjectBuffer->SetBufferContents(i, NULL);
      }

    // Resize the ring buffer
    m_DataObjectBuffer->SetNumberOfBuffers(numFrames);

    // Move previously buffered data to the locations where their frame numbers now map
    for (unsigned long i = bufferedStart; i < bufferedStart + bufferedDuration; ++i)
      {
      m_DataObjectBuffer->SetBufferContents(i, frameNumPtrMap[i]);
      }
    }

  // Go through the number of required frames and make sure none are empty
  unsigned long startFrame = m_RequestedTemporalRegion.GetFrameStart();
  for (unsigned long i = startFrame; i < startFrame + numFrames; ++i)
    {
    if (!m_DataObjectBuffer->BufferIsFull(i))
      {
      m_DataObjectBuffer->SetBufferContents(i, FrameType::New());
      }

    // Check to see if any cached regions exist and if they do, assign them
    if (m_LargestPossibleSpatialRegionCache.find(i) !=
        m_LargestPossibleSpatialRegionCache.end())
      {
      this->GetFrame(i)->SetLargestPossibleRegion(m_LargestPossibleSpatialRegionCache[i]);
      }
    if (m_RequestedSpatialRegionCache.find(i) !=
        m_RequestedSpatialRegionCache.end())
      {
      this->GetFrame(i)->SetRequestedRegion(m_RequestedSpatialRegionCache[i]);
      }
    if (m_BufferedSpatialRegionCache.find(i) !=
        m_BufferedSpatialRegionCache.end())
      {
      this->GetFrame(i)->SetBufferedRegion(m_BufferedSpatialRegionCache[i]);
      }
    }
}

//
// SetFrame
//
template<class TFrameType>
void
VideoStream<TFrameType>::SetFrame(unsigned long frameNumber, TFrameType* frame)
{
  m_DataObjectBuffer->SetBufferContents(frameNumber, frame);

  // Cache the regions
  m_LargestPossibleSpatialRegionCache[frameNumber] = frame->GetLargestPossibleRegion();
  m_RequestedSpatialRegionCache[frameNumber] = frame->GetRequestedRegion();
  m_BufferedSpatialRegionCache[frameNumber] = frame->GetBufferedRegion();
}

//
// GetFrame
//
template<class TFrameType>
TFrameType*
VideoStream<TFrameType>::GetFrame(unsigned long frameNumber)
{
  // reinterpret our buffer to contain images
  BufferType* frameBuffer = reinterpret_cast<BufferType*>(m_DataObjectBuffer.GetPointer());

  // Fetch the frame
  FrameType* frame =  frameBuffer->GetBufferContents(frameNumber);

  return frame;
}

//
// const GetFrame const
//
template<class TFrameType>
const TFrameType*
VideoStream<TFrameType>::GetFrame(unsigned long frameNumber) const
{
  // reinterpret our buffer to contain images
  BufferType* frameBuffer = reinterpret_cast<BufferType*>(m_DataObjectBuffer.GetPointer());

  // Fetch the frame
  FrameType* frame =  frameBuffer->GetBufferContents(frameNumber);

  return frame;
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

    // Copy the spatial region caches
    this->SetLargestPossibleSpatialRegionCache(
      videoData->GetLargestPossibleSpatialRegionCache());
    this->SetRequestedSpatialRegionCache(
      videoData->GetRequestedSpatialRegionCache());
    this->SetBufferedSpatialRegionCache(
      videoData->GetBufferedSpatialRegionCache());

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
  unsigned long numFrames = m_LargestPossibleTemporalRegion.GetFrameDuration();
  unsigned long startFrame = m_LargestPossibleTemporalRegion.GetFrameStart();

  // If the largest region is infinite, use the largest of the requested or
  // buffered region
  if (numFrames == ITK_INFINITE_FRAME_DURATION)
      {
      unsigned long bufEnd = m_BufferedTemporalRegion.GetFrameStart() +
                              m_BufferedTemporalRegion.GetFrameDuration();
      unsigned long reqEnd = m_RequestedTemporalRegion.GetFrameStart() +
                              m_RequestedTemporalRegion.GetFrameDuration();
      (bufEnd > reqEnd) ? (numFrames = bufEnd) : (numFrames = reqEnd);
      }

  // Go through the number of required frames, making sure none are empty and
  // setting the region
  for (unsigned long i = startFrame; i < startFrame + numFrames; ++i)
    {
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
  unsigned long numFrames = m_LargestPossibleTemporalRegion.GetFrameDuration();
  unsigned long startFrame = m_LargestPossibleTemporalRegion.GetFrameStart();

  // If the largest region is infinite, use the largest of the requested or
  // buffered region
  if (numFrames == ITK_INFINITE_FRAME_DURATION)
      {
      unsigned long bufEnd = m_BufferedTemporalRegion.GetFrameStart() +
                              m_BufferedTemporalRegion.GetFrameDuration();
      unsigned long reqEnd = m_RequestedTemporalRegion.GetFrameStart() +
                              m_RequestedTemporalRegion.GetFrameDuration();
      (bufEnd > reqEnd) ? (numFrames = bufEnd) : (numFrames = reqEnd);
      }

  // Go through the number of required frames, making sure none are empty and
  // setting the region
  for (unsigned long i = startFrame; i < startFrame + numFrames; ++i)
    {
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
  unsigned long numFrames = m_LargestPossibleTemporalRegion.GetFrameDuration();
  unsigned long startFrame = m_LargestPossibleTemporalRegion.GetFrameStart();

  // If the largest region is infinite, use the largest of the requested or
  // buffered region
  if (numFrames == ITK_INFINITE_FRAME_DURATION)
      {
      unsigned long bufEnd = m_BufferedTemporalRegion.GetFrameStart() +
                              m_BufferedTemporalRegion.GetFrameDuration();
      unsigned long reqEnd = m_RequestedTemporalRegion.GetFrameStart() +
                              m_RequestedTemporalRegion.GetFrameDuration();
      (bufEnd > reqEnd) ? (numFrames = bufEnd) : (numFrames = reqEnd);
      }

  // Go through the number of required frames, making sure none are empty and
  // setting the region
  for (unsigned long i = startFrame; i < startFrame + numFrames; ++i)
    {
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
