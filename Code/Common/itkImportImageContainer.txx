/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImportImageContainer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkImportImageContainer_txx
#define _itkImportImageContainer_txx

namespace itk
{

template <typename TElementIdentifier, typename TElement>
ImportImageContainer<TElementIdentifier , TElement>
::ImportImageContainer()
{
  m_ImportPointer = 0;
  m_ContainerManageMemory = true;
  m_Capacity = 0;
  m_Size = 0;
}

  
template <typename TElementIdentifier, typename TElement>
ImportImageContainer< TElementIdentifier , TElement >
::~ImportImageContainer()
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
ImportImageContainer< TElementIdentifier , TElement >
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
ImportImageContainer< TElementIdentifier , TElement >
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
ImportImageContainer< TElementIdentifier , TElement >
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
ImportImageContainer< TElementIdentifier , TElement >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Pointer: " << m_ImportPointer << std::endl;
  os << indent << "Container manages memory: "
     << (m_ContainerManageMemory ? "true" : "false") << std::endl;
  os << indent << "Size: " << m_Size << std::endl;
  os << indent << "Capacity: " << m_Capacity << std::endl;
}

} // end namespace itk

#endif
