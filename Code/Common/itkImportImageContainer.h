/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImportImageContainer.h
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
#ifndef __itkImportImageContainer_h
#define __itkImportImageContainer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include <utility>

namespace itk
{

/** \class ImportImageContainer
 * Defines an itk::Image front-end to a standard C-array. This container
 * conforms to the ImageContainerInterface. This is a full-fleged Object,
 * so there is modification time, debug, and reference count information.
 *
 * Template parameters for ImportImageContainer:
 *
 * TElementIdentifier =
 *     An INTEGRAL type for use in indexing the imported buffer.
 *
 * TElement =
 *    The element type stored in the container.
 */
  
template <typename TElementIdentifier, typename TElement>
class ImportImageContainer:  public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImportImageContainer     Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** \typedef
   * Save the template parameters.
   */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Get the pointer from which the image data is imported.
   */
  TElement *GetImportPointer() {return m_ImportPointer;};

  /**
   * Set the pointer from which the image data is imported.  "num" is
   * the number of pixels in the block of memory. If
   * "LetContainerManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this image data.  If
   * "LetContainerManageMemory" is true, then this class will free the
   * memory when this object is destroyed.
   */
  void SetImportPointer(TElement *ptr, TElementIdentifier num,
                        bool LetContainerManageMemory = false);

  /**
   * Index operator. This version can be an lvalue.
   */
  TElement & operator[](const ElementIdentifier id)
    { return m_ImportPointer[id]; };

  /**
   * Index operator. This version can only be an rvalue
   */
  const TElement & operator[](const ElementIdentifier id) const
    { return m_ImportPointer[id]; };
    

  /**
   * Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class.
   */
  TElement *GetBufferPointer()
    { return m_ImportPointer; };
  
  /**
   * Get the number of elements currently stored in the container.
   */
  unsigned long Size(void) const
    { return (unsigned long) m_Size; };

  /**
   * Tell the container to allocate enough memory to allow at least
   * as many elements as the size given to be stored.  If new memory
   * needs to be allocated, the contents of the old buffer are copied
   * to the new area.  The old buffer is deleted if the original pointer
   * was passed in using "LetContainerManageMemory"=true. The new buffer's
   * memory management will be handled by the container from that point on.
   *
   * \sa SetImportPointer()
   */
  void Reserve(ElementIdentifier num);
  
  /**
   * Tell the container to try to minimize its memory usage for
   * storage of the current number of elements.  If new memory is
   * allocated, the contents of old buffer are copied to the new area.
   * The previous buffer is deleted if the original pointer was in
   * using "LetContainerManageMemory"=true.  The new buffer's memory
   * management will be handled by the container from that point on.
   */
  void Squeeze(void);
  
  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(ImportImageContainer, Object);

protected:
  ImportImageContainer();
  virtual ~ImportImageContainer();
  ImportImageContainer(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  TElement            *m_ImportPointer;
  TElementIdentifier   m_Size;
  TElementIdentifier   m_Capacity;
  bool                 m_ContainerManageMemory;

};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImportImageContainer.txx"
#endif

#endif
