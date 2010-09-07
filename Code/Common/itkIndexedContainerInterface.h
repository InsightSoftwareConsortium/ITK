/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIndexedContainerInterface.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIndexedContainerInterface_h
#define __itkIndexedContainerInterface_h

#include "itkObject.h"

namespace itk
{
/** \class IndexedContainerInterface
 * This should only be used for reference when writing containers
 * conforming to this interface.  ITK uses generic programming to
 * allow container type substitution, so polymorphism is not needed to
 * use containers through this interface.  This means that a container
 * conforming to this interface need not be derived from it, and that
 * their methods should not be virtual.  However, the container must
 * derive from Object in order to support the reference counting,
 * modification time, and debug information required by this
 * interface.
 *
 * Note that many comments refer to a "default element" or "default element
 * value".  This value is equal to the default constructor of the
 * Element type.  Also note that all non-const methods assume that the
 * container was modified, and update the modification time.
 *
 * Template parameters for IndexedContainerInterface:
 *
 * TElementIdentifier =
 *    A type that shall be used to index the container.
 *    It must have a < operator defined for ordering.
 *
 * TElement =
 *    The element type stored in the container.
 *
 * \ingroup DataRepresentation
 */

template< typename TElementIdentifier, typename TElement >
class IndexedContainerInterface:public Object
{
public:
  /** Standard class typedefs. */
  typedef IndexedContainerInterface  Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard part of every itk Object. */
  itkTypeMacro(IndexedContainerInterface, Object);

  /** Save the template parameters. */
  typedef TElementIdentifier ElementIdentifier;
  typedef TElement           Element;

  /** Get a reference to an existing element.
   * It is NOT guaranteed that the element will or will not be created if it
   * doesn't exist.  This behavior is implementation-specific.
   *
   * It is assumed that the value of the element is modified through the
   * reference. */
  Element & ElementAt(ElementIdentifier);

  /** Get a reference to an existing element.
   * It is guaranteed that the element will be inserted with a default
   * value if it does not exist.
   *
   * It is assumed that the value of the element is modified through the
   * reference. */
  Element & CreateElementAt(ElementIdentifier);

  /** Get a copy of an element without range checking. */
  Element GetElement(ElementIdentifier) const;

  /** Set the value of an element.
   * It is NOT guaranteed whether a spot for the element will be created
   * automatically.  This is implementation-defined. */
  void SetElement(ElementIdentifier, Element);

  /** Set the value of an element.
   * It is guaranteed that a spot for the element will be created if it
   * doesn't exist. */
  void InsertElement(ElementIdentifier, Element);

  /** Test if there is an entry in the container corresponding to the given
   * index. */
  bool IndexExists(ElementIdentifier) const;

  /** Combine the GetElement and IndexExists into one method.
   * If false is returned, then no element with the given identifier was found.
   * If true is returned, then the identifier was found.  In this case,
   * if the element pointer given as input is not null, the element is filled
   * in with the value of the element found. */
  bool GetElementIfIndexExists(ElementIdentifier, Element *) const;

  /** Create an entry in the container corresponding to the given index.
   * The entry will be initialized with the default element.
   * If an entry already exists, its value will be overwritten with the
   * default element. */
  void CreateIndex(ElementIdentifier);

  /** Delete the entry in the container corresponding to the given identifier.
   *
   * It is NOT guaranteed that IndexExists(id) will return false if called
   * right after DeleteIndex(id).  This behavior is implementation-defined.
   * If the identifier's location is left behind, though, it will have the
   * value of the default element. */
  void DeleteIndex(ElementIdentifier);

  /** \class Iterator
   * \brief Support iteration operations through a container.
   * Dereferencing the iterator must provide an object with the following
   * methods:
   *   ElementIdentifier Index(void) const;
   *   Element&          Value(void); */
  class Iterator {};

  /** \class ConstIterator
   * \brief Support const iteration operations through a container.
   * Dereferencing the iterator must provide an object with the following
   * methods:
   *   ElementIdentifier Index(void) const;
   *   const Element&    Value(void) const; */
  class ConstIterator {};

  /** Get a begin iterator for the container. */
  Iterator Begin();

  /** Get an end iterator for the container. */
  Iterator End();

  /** Get a begin const iterator for the container. */
  ConstIterator Begin() const;

  /** Get an end const iterator for the container. */
  ConstIterator End() const;

  /** Get the number of elements currently stored in the container. */
  unsigned long Size(void) const;

  /** Tell the container to allocate enough memory to allow at least
   * as many elements as the size given to be stored.  This is NOT
   * guaranteed to actually allocate any memory, but is useful if the
   * implementation of the container allocates contiguous storage. */
  void Reserve(ElementIdentifier);

  /** Tell the container to try to minimize its memory usage for storage of
   * the current number of elements.  This is NOT guaranteed to decrease
   * memory usage. */
  void Squeeze(void);

  /** Tell the container to release any memory it may have allocated and
   * return itself to its initial state. */
  void Initialize(void);
};
} // end namespace itk

#endif
