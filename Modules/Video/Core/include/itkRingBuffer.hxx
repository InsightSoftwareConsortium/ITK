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
#ifndef itkRingBuffer_hxx
#define itkRingBuffer_hxx

#include "itkMath.h"
#include "itkPrintHelper.h"

namespace itk
{

template <typename TElement>
RingBuffer<TElement>::RingBuffer()
  : m_PointerVector()
{
  // Default to 3 buffers
  this->SetNumberOfBuffers(3);
}

template <typename TElement>
void
RingBuffer<TElement>::PrintSelf(std::ostream & os, Indent indent) const
{
  using namespace print_helper;

  Superclass::PrintSelf(os, indent);

  os << indent << "PointerVector: " << m_PointerVector << std::endl;
}

template <typename TElement>
void
RingBuffer<TElement>::MoveHead(OffsetValueType offset)
{
  // Compute the new Head index
  this->m_HeadIndex = this->GetOffsetBufferIndex(offset);

  // Mark as modified
  this->Modified();
}

template <typename TElement>
void
RingBuffer<TElement>::MoveHeadForward()
{
  this->MoveHead(1);
}

template <typename TElement>
void
RingBuffer<TElement>::MoveHeadBackward()
{
  this->MoveHead(-1);
}

template <typename TElement>
bool
RingBuffer<TElement>::BufferIsFull(OffsetValueType offset)
{
  auto bufferIndex = static_cast<size_t>(this->GetOffsetBufferIndex(offset));

  return !(this->m_PointerVector[bufferIndex].IsNull());
}

template <typename TElement>
typename TElement::Pointer
RingBuffer<TElement>::GetBufferContents(OffsetValueType offset)
{
  // Get the right buffer
  auto bufferIndex = static_cast<size_t>(this->GetOffsetBufferIndex(offset));

  // Return the resulting image
  return this->m_PointerVector[bufferIndex];
}

template <typename TElement>
void
RingBuffer<TElement>::SetBufferContents(OffsetValueType offset, ElementPointer element)
{
  // Get the right buffer
  auto bufferIndex = static_cast<size_t>(this->GetOffsetBufferIndex(offset));

  // Set the pointer
  this->m_PointerVector[bufferIndex] = element;

  // Mark as modified
  this->Modified();
}

template <typename TElement>
auto
RingBuffer<TElement>::GetNumberOfBuffers() -> SizeValueType
{
  return static_cast<typename RingBuffer<TElement>::SizeValueType>(this->m_PointerVector.size());
}

template <typename TElement>
void
RingBuffer<TElement>::SetNumberOfBuffers(SizeValueType n)
{
  const size_t currentSize = this->m_PointerVector.size();

  // If larger than current size, insert difference after tail
  if (n > currentSize)
  {
    for (size_t i = 0; i < n - currentSize; ++i)
    {
      const ElementPointer newPointer = nullptr;
      this->m_PointerVector.insert(this->m_PointerVector.begin() + this->m_HeadIndex, newPointer);

      // Increment head index if this wasn't the first one added
      if (this->m_PointerVector.size() > 1)
      {
        this->m_HeadIndex++;
      }
    }
  }

  // If smaller than current size, remove difference starting at tail
  else if (n < currentSize)
  {
    for (size_t i = 0; i < currentSize - n; ++i)
    {
      const auto tailIndex = static_cast<SizeValueType>(this->GetOffsetBufferIndex(1));
      this->m_PointerVector.erase(this->m_PointerVector.begin() + tailIndex);

      // Decrement head index if necessary
      if (this->m_HeadIndex > tailIndex)
      {
        this->m_HeadIndex--;
      }
    }
  }

  this->Modified();
}

template <typename TElement>
auto
RingBuffer<TElement>::GetOffsetBufferIndex(OffsetValueType offset) -> OffsetValueType
{
  const OffsetValueType moddedOffset = itk::Math::abs(offset) % this->GetNumberOfBuffers();
  auto                  signedHeadIndex = static_cast<OffsetValueType>(m_HeadIndex);
  if (offset >= 0)
  {
    return (signedHeadIndex + moddedOffset) % this->GetNumberOfBuffers();
  }

  return (signedHeadIndex + (this->GetNumberOfBuffers() - moddedOffset)) % this->GetNumberOfBuffers();
}

} // end namespace itk

#endif
