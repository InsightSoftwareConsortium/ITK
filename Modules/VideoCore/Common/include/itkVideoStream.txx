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

} // end namespace itk

#endif
