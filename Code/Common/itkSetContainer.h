/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSetContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkSetContainer_h
#define __itkSetContainer_h

#include "itkContainerInterfaces.h"
#include <set>

/**
 * itkSetContainer
 *
 * Define a front-end to the STL "set" container that conforms to the
 * itkUniqueAssociativeContainer interface.
 */
template <typename TElement>
class itkSetContainer:
  public itkUniqueAssociativeContainer< TElement , TElement >,
  public std::set<TElement>
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkSetContainer        Self;
  typedef itkSmartPointer<Self>  Pointer;

  /**
   * Save the template parameters.
   */
  typedef TElement  Element;
  
private:
  /**
   * Quick access to the STL set type that was inherited.
   */
  typedef std::set<Element> Set;
  
protected:
  /**
   * Provide pass-through constructors corresponding to all the STL
   * set constructors.  These are for internal use only since this is also
   * an itkObject which must be constructed through the "New()" routine.
   */
  itkSetContainer():
    Set() {}

  itkSetContainer(const key_compare& comp):
    Set(comp) {}

  itkSetContainer(const Self& r):
    Set(r) {}
  
  template <typename InputIterator>
  itkSetContainer(InputIterator first, InputIterator last):
    Set(first, last) {}

  template <typename InputIterator>
  itkSetContainer(InputIterator first, InputIterator last,
		  const key_compare& comp):
    Set(first, last, comp) {}
  
public:
  /**
   * Declare the public interface routines.
   */
  static Pointer New(void);
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSetContainer.cxx"
#endif

#endif
