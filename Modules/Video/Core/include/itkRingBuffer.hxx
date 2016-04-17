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
#ifndef itkRingBuffer_hxx
#define itkRingBuffer_hxx

#include "itkRingBuffer.h"
#include "itkMath.h"

namespace itk
{

//-CONSTRUCTOR DESTRUCTOR PRINT------------------------------------------------

//
// Constructor
//
template< typename TElement >
RingBuffer< TElement >
::RingBuffer()
  : m_HeadIndex(0),
    m_PointerVector()
{
  // Default to 3 buffers
  this->SetNumberOfBuffers(3);
}

template< typename TElement >
RingBuffer< TElement >
::~RingBuffer()
{
}


//
// PrintSelf
//
template< typename TElement >
void
RingBuffer< TElement >
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "RingBuffer:" << std::endl;
  os << indent << "NumberOfBuffers: " << this->m_PointerVector.size()
     << std::endl;
}


//-PUBLIC METHODS--------------------------------------------------------------


//
// MoveHead
//
template< typename TElement >
void
RingBuffer< TElement >
::MoveHead( OffsetValueType offset )
{
  // Compute the new Head index
  this->m_HeadIndex = this->GetOffsetBufferIndex(offset);

  // Mark as modified
  this->Modified();
}


//
// MoveHeadForward
//
template< typename TElement >
void
RingBuffer< TElement >
::MoveHeadForward()
{
  this->MoveHead(1);
}


//
// MoveHeadBackward
//
template< typename TElement >
void
RingBuffer< TElement >
::MoveHeadBackward()
{
  this->MoveHead(-1);
}


//
// BufferIsFull
//
template< typename TElement >
bool
RingBuffer< TElement >
::BufferIsFull(OffsetValueType offset)
{
  size_t bufferIndex =
    static_cast<size_t>( this->GetOffsetBufferIndex(offset) );

  return !(this->m_PointerVector[bufferIndex].IsNull());
}

//
// GetBufferContents
//
template< typename TElement >
typename TElement::Pointer
RingBuffer< TElement >
::GetBufferContents( OffsetValueType offset )
{
  // Get the right buffer
  size_t bufferIndex =
    static_cast<size_t>( this->GetOffsetBufferIndex(offset) );

  // Return the resulting image
  return this->m_PointerVector[bufferIndex];
}


//
// SetBufferContents
//
template< typename TElement >
void
RingBuffer< TElement >
::SetBufferContents( OffsetValueType offset,
                     ElementPointer element )
{
  // Get the right buffer
  size_t bufferIndex =
    static_cast<size_t>( this->GetOffsetBufferIndex(offset) );

  // Set the pointer
  this->m_PointerVector[bufferIndex] = element;

  // Mark as modified
  this->Modified();
}


//
// GetNumberOfBuffers
//
template< typename TElement >
typename RingBuffer< TElement >::SizeValueType
RingBuffer< TElement >
::GetNumberOfBuffers()
{
  return static_cast<typename RingBuffer< TElement >::SizeValueType>(this->m_PointerVector.size());
}


//
// SetNumberOfBuffers
//
template< typename TElement >
void
RingBuffer< TElement >
::SetNumberOfBuffers(SizeValueType n)
{
  size_t currentSize = this->m_PointerVector.size();

  // If larger than current size, insert difference after tail
  if (n > currentSize)
    {
    for (size_t i = 0; i < n - currentSize; ++i)
      {
      ElementPointer newPointer = ITK_NULLPTR;
      this->m_PointerVector.insert( this->m_PointerVector.begin() +
                                    this->m_HeadIndex, newPointer );

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
      unsigned int tailIndex = this->GetOffsetBufferIndex(1);
      this->m_PointerVector.erase( this->m_PointerVector.begin() + tailIndex );

      // Decrement head index if necessary
      if (this->m_HeadIndex > tailIndex)
        {
        this->m_HeadIndex--;
        }
      }
    }

  this->Modified();
}


//-PROTECTED METHODS-----------------------------------------------------------

//
// GetOffsetBufferIndex
//
template< typename TElement >
typename RingBuffer< TElement >::OffsetValueType
RingBuffer< TElement >
::GetOffsetBufferIndex(OffsetValueType offset)
{
  OffsetValueType moddedOffset = itk::Math::abs(offset) % this->GetNumberOfBuffers();
  OffsetValueType signedHeadIndex = static_cast<OffsetValueType>(m_HeadIndex);
  if (offset >= 0)
    {
    return ( signedHeadIndex + moddedOffset) % this->GetNumberOfBuffers();
    }
  else
    {
    return ( signedHeadIndex + (this->GetNumberOfBuffers() - moddedOffset))
              % this->GetNumberOfBuffers();
    }
}

} // end namespace itk

#endif
