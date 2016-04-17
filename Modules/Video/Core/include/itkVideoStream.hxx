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
#ifndef itkVideoStream_hxx
#define itkVideoStream_hxx

#include "itkVideoStream.h"

namespace itk
{

template<typename TFrameType>
void
VideoStream<TFrameType>
::SetFrameLargestPossibleSpatialRegion(SizeValueType frameNumber, typename TFrameType::RegionType region)
{
  m_LargestPossibleSpatialRegionCache[frameNumber] = region;

  // If the frame is currently buffered, set the actual frame's region
  SizeValueType bufStart = m_BufferedTemporalRegion.GetFrameStart();
  SizeValueType bufDur = m_BufferedTemporalRegion.GetFrameDuration();
  if (frameNumber >= bufStart && frameNumber < bufStart + bufDur)
    {
    FrameType* frame = this->GetFrame(frameNumber);
    frame->SetLargestPossibleRegion(region);
    }
}


template<typename TFrameType>
const typename TFrameType::RegionType &
VideoStream<TFrameType>
::GetFrameLargestPossibleSpatialRegion(SizeValueType frameNumber) const
{
  // It seems that std::map's [] operator isn't const correct, so we need to
  // access this member from an non-const version of ourselves
  return const_cast<Self*>(this)->m_LargestPossibleSpatialRegionCache[frameNumber];
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetFrameRequestedSpatialRegion(SizeValueType frameNumber, typename TFrameType::RegionType region)
{
  m_RequestedSpatialRegionCache[frameNumber] = region;

  // If the frame is currently buffered, set the actual frame's region
  SizeValueType bufStart = m_BufferedTemporalRegion.GetFrameStart();
  SizeValueType bufDur = m_BufferedTemporalRegion.GetFrameDuration();
  if (frameNumber >= bufStart && frameNumber < bufStart + bufDur)
    {
    FrameType* frame = this->GetFrame(frameNumber);
    frame->SetRequestedRegion(region);
    }
}


template<typename TFrameType>
const typename TFrameType::RegionType &
VideoStream<TFrameType>
::GetFrameRequestedSpatialRegion(SizeValueType frameNumber) const
{
  // It seems that std::map's [] operator isn't const correct, so we need to
  // access this member from an non-const version of ourselves
  return const_cast<Self*>(this)->m_RequestedSpatialRegionCache[frameNumber];
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetFrameBufferedSpatialRegion(SizeValueType frameNumber, typename TFrameType::RegionType region)
{
  m_BufferedSpatialRegionCache[frameNumber] = region;

  // If the frame is currently buffered, set the actual frame's region
  SizeValueType bufStart = m_BufferedTemporalRegion.GetFrameStart();
  SizeValueType bufDur = m_BufferedTemporalRegion.GetFrameDuration();
  if (frameNumber >= bufStart && frameNumber < bufStart + bufDur)
    {
    FrameType* frame = this->GetFrame(frameNumber);
    frame->SetBufferedRegion(region);
    }
}


template<typename TFrameType>
const typename TFrameType::RegionType &
VideoStream<TFrameType>
::GetFrameBufferedSpatialRegion(SizeValueType frameNumber) const
{
  // It seems that std::map's [] operator isn't const correct, so we need to
  // access this member from an non-const version of ourselves
  return const_cast<Self*>(this)->m_BufferedSpatialRegionCache[frameNumber];
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetFrameSpacing(SizeValueType frameNumber, typename TFrameType::SpacingType spacing)
{
  // Make sure spacing is non-zero
  for (unsigned int i = 0; i < FrameType::ImageDimension; ++i)
    {
    if (spacing[i] == 0.0)
      {
      itkExceptionMacro("Zero spacing is not allowed for any dimension: Spacing is " << spacing);
      }
    }

  m_SpacingCache[frameNumber] = spacing;

  // If the frame is currently buffered, set the actual frame's spacing
  SizeValueType bufStart = m_BufferedTemporalRegion.GetFrameStart();
  SizeValueType bufDur = m_BufferedTemporalRegion.GetFrameDuration();
  if (frameNumber >= bufStart && frameNumber < bufStart + bufDur)
    {
    FrameType* frame = this->GetFrame(frameNumber);
    frame->SetSpacing(spacing);
    }
}


template<typename TFrameType>
const typename TFrameType::SpacingType &
VideoStream<TFrameType>
::GetFrameSpacing(SizeValueType frameNumber) const
{
  // It seems that std::map's [] operator isn't const correct, so we need to
  // access this member from an non-const version of ourselves
  return const_cast<Self*>(this)->m_SpacingCache[frameNumber];
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetFrameOrigin(SizeValueType frameNumber, typename TFrameType::PointType origin)
{
  m_OriginCache[frameNumber] = origin;

  // If the frame is currently buffered, set the actual frame's spacing
  SizeValueType bufStart = m_BufferedTemporalRegion.GetFrameStart();
  SizeValueType bufDur = m_BufferedTemporalRegion.GetFrameDuration();
  if (frameNumber >= bufStart && frameNumber < bufStart + bufDur)
    {
    FrameType* frame = this->GetFrame(frameNumber);
    frame->SetOrigin(origin);
    }
}


template<typename TFrameType>
const typename TFrameType::PointType &
VideoStream<TFrameType>
::GetFrameOrigin(SizeValueType frameNumber) const
{
  // It seems that std::map's [] operator isn't const correct, so we need to
  // access this member from an non-const version of ourselves
  return const_cast<Self*>(this)->m_OriginCache[frameNumber];
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetFrameDirection(SizeValueType frameNumber, typename TFrameType::DirectionType direction)
{
  // Determinant is non-zero
  if (itk::Math::abs(vnl_determinant(direction.GetVnlMatrix())) <= itk::Math::eps)
    {
    itkExceptionMacro("Bad direction, determinant is 0. Direction is " << direction);
    }

  m_DirectionCache[frameNumber] = direction;

  // If the frame is currently buffered, set the actual frame's spacing
  SizeValueType bufStart = m_BufferedTemporalRegion.GetFrameStart();
  SizeValueType bufDur = m_BufferedTemporalRegion.GetFrameDuration();
  if (frameNumber >= bufStart && frameNumber < bufStart + bufDur)
    {
    FrameType* frame = this->GetFrame(frameNumber);
    frame->SetDirection(direction);
    }
}


template<typename TFrameType>
const typename TFrameType::DirectionType &
VideoStream<TFrameType>
::GetFrameDirection(SizeValueType frameNumber) const
{
  // It seems that std::map's [] operator isn't const correct, so we need to
  // access this member from an non-const version of ourselves
  return const_cast<Self*>(this)->m_DirectionCache[frameNumber];
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetFrameBuffer(typename VideoStream<TFrameType>::BufferType* buffer)
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


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetMinimumBufferSize(SizeValueType minimumNumberOfFrames)
{
  // If we don't have enough buffer space to handle the number of requested
  // frames, we need to resize the ring buffer. Just resizing can cause data to
  // be in the wrong place. For example if the head of the buffer is at index 0
  // and the buffer has 3 slots, setting frame number 3 will actually place the
  // data into slot 0.  If we then resize the buffer to have 4 slots, the data
  // for frame 3 will live in slot 0 even though a request for frame 3 will
  // return the data from the newly created slot 3. To circumvent this problem,
  // we move the buffered data to the proper indices in the ring buffer after
  // resizing.
  if (m_DataObjectBuffer->GetNumberOfBuffers() < minimumNumberOfFrames)
    {
    // Save the indices of all frames in the currently buffered region
    const SizeValueType bufferedStart = m_BufferedTemporalRegion.GetFrameStart();
    const SizeValueType bufferedDuration = m_BufferedTemporalRegion.GetFrameDuration();
    std::vector< DataObject * > frames( bufferedDuration - bufferedStart, ITK_NULLPTR );
    for (SizeValueType i = bufferedStart; i < bufferedStart + bufferedDuration; ++i)
      {
      frames[i - bufferedStart] = m_DataObjectBuffer->GetBufferContents(i);
      }

    // Resize the ring buffer
    m_DataObjectBuffer->SetNumberOfBuffers(minimumNumberOfFrames);

    // Move previously buffered data to the locations where their frame numbers now map
    for (SizeValueType i = bufferedStart; i < bufferedStart + bufferedDuration; ++i)
      {
      m_DataObjectBuffer->SetBufferContents(i, frames[i - bufferedStart]);
      }
    }
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::InitializeEmptyFrames()
{
  // If we don't have any frames requested, just return
  SizeValueType numFrames = m_RequestedTemporalRegion.GetFrameDuration();
  if (numFrames == 0)
    {
    return;
    }


  // Safely expand the ring buffer if necessary
  this->SetMinimumBufferSize(numFrames);

  // Go through the number of required frames and make sure none are empty
  SizeValueType startFrame = m_RequestedTemporalRegion.GetFrameStart();
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    if (!m_DataObjectBuffer->BufferIsFull(i))
      {
      FramePointer newFrame = FrameType::New();
      FrameType* newFrameRawPointer = newFrame.GetPointer();
      typename BufferType::ElementPointer element =
        dynamic_cast<typename BufferType::ElementType*>(newFrameRawPointer);
      m_DataObjectBuffer->SetBufferContents(i, element);
      }

    // Check to see if any cached meta data exists and if it does, assign it
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
    if (m_SpacingCache.find(i) != m_SpacingCache.end())
      {
      this->GetFrame(i)->SetSpacing(m_SpacingCache[i]);
      }
    if (m_OriginCache.find(i) != m_OriginCache.end())
      {
      this->GetFrame(i)->SetOrigin(m_OriginCache[i]);
      }
    if (m_DirectionCache.find(i) != m_DirectionCache.end())
      {
      this->GetFrame(i)->SetDirection(m_DirectionCache[i]);
      }
    }
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetFrame(SizeValueType frameNumber, FramePointer frame)
{
  typename BufferType::ElementType* dataObjectRawPointer =
    dynamic_cast<typename BufferType::ElementType*>(frame.GetPointer());
  typename BufferType::ElementPointer dataObject = dataObjectRawPointer;
  m_DataObjectBuffer->SetBufferContents(frameNumber,dataObject);

  // Cache the meta data
  m_LargestPossibleSpatialRegionCache[frameNumber] = frame->GetLargestPossibleRegion();
  m_RequestedSpatialRegionCache[frameNumber] = frame->GetRequestedRegion();
  m_BufferedSpatialRegionCache[frameNumber] = frame->GetBufferedRegion();
  m_SpacingCache[frameNumber] = frame->GetSpacing();
  m_OriginCache[frameNumber] = frame->GetOrigin();
  m_DirectionCache[frameNumber] = frame->GetDirection();
}


template<typename TFrameType>
typename VideoStream<TFrameType>::FramePointer
VideoStream<TFrameType>
::GetFrame(SizeValueType frameNumber)
{

  // Fetch the frame
  typename BufferType::ElementPointer element =
    m_DataObjectBuffer->GetBufferContents(frameNumber);
  FramePointer frame = dynamic_cast<FrameType*>(element.GetPointer());
  return frame;
}


template<typename TFrameType>
typename VideoStream<TFrameType>::FrameConstPointer
VideoStream<TFrameType>
::GetFrame(SizeValueType frameNumber) const
{
  typename BufferType::ElementPointer element =
    m_DataObjectBuffer->GetBufferContents(frameNumber);
  FrameConstPointer frame = dynamic_cast<FrameType*>(element.GetPointer());
  return frame;
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::Graft(const DataObject* data)
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

    // Copy the meta data caches
    this->SetLargestPossibleSpatialRegionCache(
      videoData->GetLargestPossibleSpatialRegionCache());
    this->SetRequestedSpatialRegionCache(
      videoData->GetRequestedSpatialRegionCache());
    this->SetBufferedSpatialRegionCache(
      videoData->GetBufferedSpatialRegionCache());
    this->SetSpacingCache(videoData->GetSpacingCache());
    this->SetOriginCache(videoData->GetOriginCache());
    this->SetDirectionCache(videoData->GetDirectionCache());

    // Copy the frame buffer
    this->SetFrameBuffer(const_cast< BufferType* >(videoData->GetFrameBuffer()));
    }
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetAllLargestPossibleSpatialRegions(typename TFrameType::RegionType region)
{
  SizeValueType numFrames = m_LargestPossibleTemporalRegion.GetFrameDuration();
  SizeValueType startFrame = m_LargestPossibleTemporalRegion.GetFrameStart();

  // If the largest region is infinite, use the largest of the requested or
  // buffered region
  if (numFrames == ITK_INFINITE_FRAME_DURATION)
      {
      SizeValueType bufEnd = m_BufferedTemporalRegion.GetFrameStart() +
                              m_BufferedTemporalRegion.GetFrameDuration();
      SizeValueType reqEnd = m_RequestedTemporalRegion.GetFrameStart() +
                              m_RequestedTemporalRegion.GetFrameDuration();
      (bufEnd > reqEnd) ? (numFrames = bufEnd) : (numFrames = reqEnd);
      }

  // Go through the number of required frames, making sure none are empty and
  // setting the region
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    this->SetFrameLargestPossibleSpatialRegion(i, region);
    }
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetAllRequestedSpatialRegions(typename TFrameType::RegionType region)
{
  SizeValueType numFrames = m_LargestPossibleTemporalRegion.GetFrameDuration();
  SizeValueType startFrame = m_LargestPossibleTemporalRegion.GetFrameStart();

  // If the largest region is infinite, use the largest of the requested or
  // buffered region
  if (numFrames == ITK_INFINITE_FRAME_DURATION)
      {
      SizeValueType bufEnd = m_BufferedTemporalRegion.GetFrameStart() +
                              m_BufferedTemporalRegion.GetFrameDuration();
      SizeValueType reqEnd = m_RequestedTemporalRegion.GetFrameStart() +
                              m_RequestedTemporalRegion.GetFrameDuration();
      (bufEnd > reqEnd) ? (numFrames = bufEnd) : (numFrames = reqEnd);
      }

  // Go through the number of required frames, making sure none are empty and
  // setting the region
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    this->SetFrameRequestedSpatialRegion(i, region);
    }
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetAllBufferedSpatialRegions(typename TFrameType::RegionType region)
{
  SizeValueType numFrames = m_LargestPossibleTemporalRegion.GetFrameDuration();
  SizeValueType startFrame = m_LargestPossibleTemporalRegion.GetFrameStart();

  // If the largest region is infinite, use the largest of the requested or
  // buffered region
  if (numFrames == ITK_INFINITE_FRAME_DURATION)
      {
      SizeValueType bufEnd = m_BufferedTemporalRegion.GetFrameStart() +
                              m_BufferedTemporalRegion.GetFrameDuration();
      SizeValueType reqEnd = m_RequestedTemporalRegion.GetFrameStart() +
                              m_RequestedTemporalRegion.GetFrameDuration();
      (bufEnd > reqEnd) ? (numFrames = bufEnd) : (numFrames = reqEnd);
      }

  // Go through the number of required frames, making sure none are empty and
  // setting the region
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    this->SetFrameBufferedSpatialRegion(i, region);
    }
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetAllFramesSpacing(typename TFrameType::SpacingType spacing)
{
  SizeValueType numFrames = m_LargestPossibleTemporalRegion.GetFrameDuration();
  SizeValueType startFrame = m_LargestPossibleTemporalRegion.GetFrameStart();

  // If the largest region is infinite, use the largest of the requested or
  // buffered region
  if (numFrames == ITK_INFINITE_FRAME_DURATION)
      {
      SizeValueType bufEnd = m_BufferedTemporalRegion.GetFrameStart() +
                              m_BufferedTemporalRegion.GetFrameDuration();
      SizeValueType reqEnd = m_RequestedTemporalRegion.GetFrameStart() +
                              m_RequestedTemporalRegion.GetFrameDuration();
      (bufEnd > reqEnd) ? (numFrames = bufEnd) : (numFrames = reqEnd);
      }

  // Go through the number of required frames, making sure none are empty and
  // setting the region
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    this->SetFrameSpacing(i, spacing);
    }
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetAllFramesOrigin(typename TFrameType::PointType origin)
{
  SizeValueType numFrames = m_LargestPossibleTemporalRegion.GetFrameDuration();
  SizeValueType startFrame = m_LargestPossibleTemporalRegion.GetFrameStart();

  // If the largest region is infinite, use the largest of the requested or
  // buffered region
  if (numFrames == ITK_INFINITE_FRAME_DURATION)
      {
      SizeValueType bufEnd = m_BufferedTemporalRegion.GetFrameStart() +
                              m_BufferedTemporalRegion.GetFrameDuration();
      SizeValueType reqEnd = m_RequestedTemporalRegion.GetFrameStart() +
                              m_RequestedTemporalRegion.GetFrameDuration();
      (bufEnd > reqEnd) ? (numFrames = bufEnd) : (numFrames = reqEnd);
      }

  // Go through the number of required frames, making sure none are empty and
  // setting the region
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    this->SetFrameOrigin(i, origin);
    }
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::SetAllFramesDirection(typename TFrameType::DirectionType direction)
{
  SizeValueType numFrames = m_LargestPossibleTemporalRegion.GetFrameDuration();
  SizeValueType startFrame = m_LargestPossibleTemporalRegion.GetFrameStart();

  // If the largest region is infinite, use the largest of the requested or
  // buffered region
  if (numFrames == ITK_INFINITE_FRAME_DURATION)
      {
      SizeValueType bufEnd = m_BufferedTemporalRegion.GetFrameStart() +
                              m_BufferedTemporalRegion.GetFrameDuration();
      SizeValueType reqEnd = m_RequestedTemporalRegion.GetFrameStart() +
                              m_RequestedTemporalRegion.GetFrameDuration();
      (bufEnd > reqEnd) ? (numFrames = bufEnd) : (numFrames = reqEnd);
      }

  // Go through the number of required frames, making sure none are empty and
  // setting the region
  for (SizeValueType i = startFrame; i < startFrame + numFrames; ++i)
    {
    this->SetFrameDirection(i, direction);
    }
}


template<typename TFrameType>
void
VideoStream<TFrameType>
::Allocate()
{
  SizeValueType numFrames = m_BufferedTemporalRegion.GetFrameDuration();
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
  for (SizeValueType i = 1; i <= numFrames; ++i)
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
