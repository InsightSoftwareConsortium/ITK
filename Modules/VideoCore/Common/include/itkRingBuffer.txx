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
#ifndef __itkRingBuffer_txx
#define __itkRingBuffer_txx

#include "itkRingBuffer.h"

namespace itk
{

//-CONSTRUCTOR DESTRUCTOR PRINT------------------------------------------------

//
// Constructor
//
template< class TElementType >
RingBuffer< TElementType >
::RingBuffer()
  : m_HeadIndex(0),
    m_PointerVector()
{
  // Default to 3 buffers
  this->SetNumberOfBuffers(3);
}


//
// PrintSelf
//
template< class TElementType >
void
RingBuffer< TElementType >
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "RingBuffer:" << std::endl;
  os << indent << "NumberOfBuffers: " << this->m_PointerVector.size() << std::endl;
}


//-PUBLIC METHODS--------------------------------------------------------------


//
// MoveHead
//
template< class TElementType >
void
RingBuffer< TElementType >
::MoveHead( int offset )
{
  // Compute the new Head index
  this->m_HeadIndex = this->GetOffsetBufferIndex(offset);

  // Mark as modified
  this->Modified();
}


//
// MoveHeadForward
//
template< class TElementType >
void
RingBuffer< TElementType >
::MoveHeadForward()
{
  this->MoveHead(1);
}


//
// MoveHeadBackward
//
template< class TElementType >
void
RingBuffer< TElementType >
::MoveHeadBackward()
{
  this->MoveHead(-1);
}


//
// BufferIsFull
//
template< class TElementType >
bool
RingBuffer< TElementType >
::BufferIsFull(int offset)
{
  unsigned int bufferIndex = this->GetOffsetBufferIndex(offset);
  return !(this->m_PointerVector[bufferIndex].IsNull());
}

//
// GetBufferContents
//
template< class TElementType >
typename TElementType::Pointer
RingBuffer< TElementType >
::GetBufferContents( int offset )
{
  // Get the right buffer
  unsigned int bufferIndex = this->GetOffsetBufferIndex(offset);

  // Return the resulting image
  return this->m_PointerVector[bufferIndex];
}


//
// SetBufferContents
//
template< class TElementType >
void
RingBuffer< TElementType >
::SetBufferContents( int offset, RingBuffer< TElementType >::ElementType* element )
{
  // Get the right buffer
  unsigned int bufferIndex = this->GetOffsetBufferIndex(offset);

  // Set the pointer
  this->m_PointerVector[bufferIndex] = element;
}


//
// GetNumberOfBuffers
//
template< class TElementType >
typename RingBuffer< TElementType >::SizeValueType
RingBuffer< TElementType >
::GetNumberOfBuffers()
{
  return this->m_PointerVector.size();
}


//
// SetNumberOfBuffers
//
template< class TElementType >
void
RingBuffer< TElementType >
::SetNumberOfBuffers(SizeValueType n)
{
  unsigned int currentSize = this->m_PointerVector.size();

  // If larger than current size, insert difference after tail
  if (n > currentSize)
    {
    for (unsigned int i = 0; i < n - currentSize; ++i)
      {
      typename ElementType::Pointer newPointer = NULL;
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
    for (unsigned int i = 0; i < currentSize - n; ++i)
      {
      unsigned int tailIndex = this->GetOffsetBufferIndex(1);
      this->m_PointerVector.erase(this->m_PointerVector.begin() + tailIndex);

      // Decrement head index if necessary
      if (this->m_HeadIndex > tailIndex)
        {
        this->m_HeadIndex--;
        }
      }
    }
}


//-PROTECTED METHODS-----------------------------------------------------------

//
// GetOffsetBufferIndex
//
template< class TElementType >
unsigned int
RingBuffer< TElementType >
::GetOffsetBufferIndex(int offset)
{
  int moddedOffset = std::abs(offset) % this->GetNumberOfBuffers();
  if (offset >= 0)
    {
    return ((int)this->m_HeadIndex + moddedOffset) % this->GetNumberOfBuffers();
    }
  else
    {
    return ((int)this->m_HeadIndex + (this->GetNumberOfBuffers() - moddedOffset))
              % this->GetNumberOfBuffers();
    }
}



} // end namespace itk

#endif
