/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObjectStore.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkObjectStore_txx
#define __itkObjectStore_txx

#include "itkObjectStore.h"

namespace itk {

template <class TObjectType>
ObjectStore<TObjectType>
::ObjectStore()
{
  m_Size  = 0;
  m_LinearGrowthSize = 1024;
  m_GrowthStrategy = EXPONENTIAL_GROWTH;
}

template <class TObjectType>
ObjectStore<TObjectType>
::~ObjectStore()
{
  this->Clear();
}

template <class TObjectType>
void
ObjectStore<TObjectType>
::Reserve(::size_t n)
{
  // No need to grow? Do nothing.
  if ( n <= m_Size ) return;

  // Need to grow.  Allocate a new block of memory and copy that block's
  // pointers into the m_FreeList.  Updates m_Size appropriately.
  MemoryBlock new_block( n - m_Size );
  m_Store.push_back( new_block );

  m_FreeList.reserve(n);
  for (ObjectType *ptr = new_block.Begin;
       ptr < new_block.Begin + new_block.Size; ptr++)
    {      m_FreeList.push_back(ptr);    }
  m_Size += ( n - m_Size );
}

template <class TObjectType>
typename ObjectStore<TObjectType>::ObjectType *
ObjectStore<TObjectType>
::Borrow()
{
  ObjectType *p;
  if (m_FreeList.empty() ) // must allocate more memory
    {
      this->Reserve( m_Size + this->GetGrowthSize() );
    }
  p = m_FreeList.back();
  m_FreeList.pop_back();
  return p;

  //To do: This method will need to decrement counters in the memory blocks so
  //that blocks know when they can be deleted.
}

template <class TObjectType>
void
ObjectStore<TObjectType>
::Return(ObjectType *p)
{
  if ( m_FreeList.size() != m_Size )
    {  m_FreeList.push_back(p); }
  // else object has been used incorrectly

  // To do:
  // Eventually this method will have to increment counters in the memory
  // blocks so that blocks know when they can be deleted.  This will also allow
  // for checking to see if p actually belongs to this store.
  //
  // Problems could easily result if a pointer is Returned more than
  // once. Only time-wise efficient way to deal with this would be a flag in
  // the allocated memory block for each object, sort of how malloc deals with
  // it?
}



template <class TObjectType>
::size_t
ObjectStore<TObjectType>
::GetGrowthSize()
{
  switch (m_GrowthStrategy)
    {
    case LINEAR_GROWTH:
      return m_LinearGrowthSize;
      break;
    case EXPONENTIAL_GROWTH:
      if (m_Size == 0) return m_LinearGrowthSize;
      else return m_Size;
      break;
    default:
      return m_LinearGrowthSize;
    }
}

template <class TObjectType>
void
ObjectStore<TObjectType>
::Squeeze()
{
  // Not implemented yet.

}

template <class TObjectType>
void
ObjectStore<TObjectType>
::Clear()
{
  // Clear the free list.
  m_FreeList.clear();
  
  // Empty the MemoryBlock list and deallocate all memory blocks.
  while ( ! m_Store.empty() )
    {
      m_Store.back().Delete();
      m_Store.pop_back();
    }
  m_Size = 0;
}

template <class TObjectType>
void
ObjectStore<TObjectType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "m_GrowthStrategy: " << m_GrowthStrategy << std::endl;
  os << indent << "m_Size: " << m_Size << std::endl;
  os << indent << "m_LinearGrowthSize: " << m_LinearGrowthSize << std::endl;
  os << indent << "Free list size: " << m_FreeList.size() << std::endl;
  os << indent << "Free list capacity: " << m_FreeList.capacity() << std::endl;
  os << indent << "Number of blocks in store: " << m_Store.size() << std::endl;
}




} // end namespace itk

#endif
