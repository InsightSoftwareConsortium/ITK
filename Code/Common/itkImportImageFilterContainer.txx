/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImportImageFilterContainer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

namespace itk
{

template <typename TElementIdentifier, typename TElement>
ImportImageFilterContainer<TElementIdentifier , TElement>
::ImportImageFilterContainer()
{
  m_ImportPointer = 0;
  m_ContainerManageMemory = true;
  m_Capacity = 0;
  m_Size = 0;
}

  
template <typename TElementIdentifier, typename TElement>
ImportImageFilterContainer< TElementIdentifier , TElement >
::~ImportImageFilterContainer()
{
  if (m_ImportPointer && m_ContainerManageMemory)
    {
    delete [] m_ImportPointer;
    }
}


/**
 * Tell the container to allocate enough memory to allow at least
 * as many elements as the size given to be stored.  
 */
template <typename TElementIdentifier, typename TElement>
void
ImportImageFilterContainer< TElementIdentifier , TElement >
::Reserve(ElementIdentifier size)
{
  if (m_ImportPointer)
    {
    if (size > m_Capacity)
      {
      TElement *temp = new TElement[size];
      // only copy the portion of the data used in the old buffer
      memcpy(temp, m_ImportPointer, m_Size*sizeof(TElement));
      if (m_ContainerManageMemory)
        {
        delete [] m_ImportPointer;
        }
      m_ImportPointer = temp;
      m_ContainerManageMemory = true;
      m_Capacity = size;
      m_Size = size;
      this->Modified();
      }
    }
  else
    {
    m_ImportPointer = new TElement[size];
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
template <typename TElementIdentifier, typename TElement>
void
ImportImageFilterContainer< TElementIdentifier , TElement >
::Squeeze(void)
{
  if (m_ImportPointer)
    {
    if (m_Size < m_Capacity)
      {
      TElement *temp = new TElement[m_Size];
      memcpy(temp, m_ImportPointer, m_Size*sizeof(TElement));
      if (m_ContainerManageMemory)
        {
        delete [] m_ImportPointer;
        }
      m_ImportPointer = temp;
      m_ContainerManageMemory = true;
      m_Capacity = m_Size;

      this->Modified();
      }
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
template <typename TElementIdentifier, typename TElement>
void
ImportImageFilterContainer< TElementIdentifier , TElement >
::SetImportPointer(TElement *ptr, TElementIdentifier num,
                   bool LetContainerManageMemory)
{
  if (m_ImportPointer && m_ContainerManageMemory)
    {
    delete [] m_ImportPointer;
    }
  m_ImportPointer = ptr;
  m_ContainerManageMemory = LetContainerManageMemory;
  m_Capacity = num;
  m_Size = num;

  this->Modified();
}


template <typename TElementIdentifier, typename TElement>
void
ImportImageFilterContainer< TElementIdentifier , TElement >
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Pointer: " << m_ImportPointer << std::endl;
  os << indent << "Container manages memory: "
     << (m_ContainerManageMemory ? "true" : "false") << std::endl;
  os << indent << "Size: " << m_Size << std::endl;
  os << indent << "Capacity: " << m_Capacity << std::endl;
}

} // end namespace itk
