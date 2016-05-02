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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkImportImageContainer_hxx
#define itkImportImageContainer_hxx

#include "itkImportImageContainer.h"

namespace itk
{
template< typename TElementIdentifier, typename TElement >
ImportImageContainer< TElementIdentifier, TElement >
::ImportImageContainer()
{
  m_ImportPointer = ITK_NULLPTR;
  m_ContainerManageMemory = true;
  m_Capacity = 0;
  m_Size = 0;
}

template< typename TElementIdentifier, typename TElement >
ImportImageContainer< TElementIdentifier, TElement >
::~ImportImageContainer()
{
  DeallocateManagedMemory();
}

/**
 * Tell the container to allocate enough memory to allow at least
 * as many elements as the size given to be stored.
 */
template< typename TElementIdentifier, typename TElement >
void
ImportImageContainer< TElementIdentifier, TElement >
::Reserve(ElementIdentifier size, const bool UseDefaultConstructor )
{
  // Reserve has a Resize semantics. We keep it that way for
  // backwards compatibility .
  // See https://www.itk.org/Bug/view.php?id=2893 for details
  if ( m_ImportPointer )
    {
    if ( size > m_Capacity )
      {
      TElement *temp = this->AllocateElements(size, UseDefaultConstructor);
      // only copy the portion of the data used in the old buffer
      std::copy(m_ImportPointer,
                m_ImportPointer+m_Size,
                temp);

      DeallocateManagedMemory();

      m_ImportPointer = temp;
      m_ContainerManageMemory = true;
      m_Capacity = size;
      m_Size = size;
      this->Modified();
      }
    else
      {
      m_Size = size;
      this->Modified();
      }
    }
  else
    {
    m_ImportPointer = this->AllocateElements(size, UseDefaultConstructor);
    m_Capacity = size;
    m_Size = size;
    m_ContainerManageMemory = true;
    this->Modified();
    }
}

/**
 * Tell the container to try to minimize its memory usage for storage of
 * the current number of elements.
 */
template< typename TElementIdentifier, typename TElement >
void
ImportImageContainer< TElementIdentifier, TElement >
::Squeeze(void)
{
  if ( m_ImportPointer )
    {
    if ( m_Size < m_Capacity )
      {
      const TElementIdentifier size = m_Size;
      TElement *               temp = this->AllocateElements(size, false);
      std::copy(m_ImportPointer,
                m_ImportPointer+m_Size,
                temp);

      DeallocateManagedMemory();

      m_ImportPointer = temp;
      m_ContainerManageMemory = true;
      m_Capacity = size;
      m_Size = size;

      this->Modified();
      }
    }
}

/**
 * Tell the container to try to minimize its memory usage for storage of
 * the current number of elements.
 */
template< typename TElementIdentifier, typename TElement >
void
ImportImageContainer< TElementIdentifier, TElement >
::Initialize(void)
{
  if ( m_ImportPointer )
    {
    DeallocateManagedMemory();

    m_ContainerManageMemory = true;

    this->Modified();
    }
}

/**
 * Set the pointer from which the image data is imported.  "num" is
 * the number of pixels in the block of memory. If
 * "LetContainerManageMemory" is false, then the application retains
 * the responsibility of freeing the memory for this image data.  If
 * "LetContainerManageMemory" is true, then this class will free the
 * memory when this object is destroyed.
 */
template< typename TElementIdentifier, typename TElement >
void
ImportImageContainer< TElementIdentifier, TElement >
::SetImportPointer(TElement *ptr, TElementIdentifier num,
                   bool LetContainerManageMemory)
{
  DeallocateManagedMemory();
  m_ImportPointer = ptr;
  m_ContainerManageMemory = LetContainerManageMemory;
  m_Capacity = num;
  m_Size = num;

  this->Modified();
}

template< typename TElementIdentifier, typename TElement >
TElement *ImportImageContainer< TElementIdentifier, TElement >
::AllocateElements(ElementIdentifier size, bool UseDefaultConstructor ) const
{
  // Encapsulate all image memory allocation here to throw an
  // exception when memory allocation fails even when the compiler
  // does not do this by default.
  TElement *data;

  try
    {
    if ( UseDefaultConstructor )
      {
      data = new TElement[size](); //POD types initialized to 0, others use default constructor.
      }
    else
      {
      data = new TElement[size]; //Faster but uninitialized
      }
    }
  catch ( ... )
    {
    data = ITK_NULLPTR;
    }
  if ( !data )
    {
    // We cannot construct an error string here because we may be out
    // of memory.  Do not use the exception macro.
    throw MemoryAllocationError(__FILE__, __LINE__,
                                "Failed to allocate memory for image.",
                                ITK_LOCATION);
    }
  return data;
}

template< typename TElementIdentifier, typename TElement >
void ImportImageContainer< TElementIdentifier, TElement >
::DeallocateManagedMemory()
{
  // Encapsulate all image memory deallocation here
  if ( m_ContainerManageMemory )
    {
    delete[] m_ImportPointer;
    }
  m_ImportPointer = ITK_NULLPTR;
  m_Capacity = 0;
  m_Size = 0;
}

template< typename TElementIdentifier, typename TElement >
void
ImportImageContainer< TElementIdentifier, TElement >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Pointer: " << static_cast< void * >( m_ImportPointer ) << std::endl;
  os << indent << "Container manages memory: "
     << ( m_ContainerManageMemory ? "true" : "false" ) << std::endl;
  os << indent << "Size: " << m_Size << std::endl;
  os << indent << "Capacity: " << m_Capacity << std::endl;
}
} // end namespace itk

#endif
