/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSTLContainerAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSTLContainerAdaptor_h
#define __itkSTLContainerAdaptor_h

/** \class STLContainerAdaptor
* An adapter object that casts a itk::XxxContainer into std::xxx
* and enables access to the underlying data structure. When the STLContainerAdaptor
* is destroyed, it automatically calls XxxContainer::Modified().
* Here's a usage example of STLContainerAdaptor
*     itk::STLContainerAdaptor<itk::VectorContainer<unsigned long, ElementType>> vecAdaptor(aContainer);
*     std::vector<ElementType> & vec = vecAdaptor.GetSTLContainerRef();
*     // do things with vec ...
*     // upon return from function, vecAdaptor is destroyed and aContainer is Modified()
*/
namespace itk {

template<typename TContainer>
class STLContainerAdaptor
{
public:

  typedef TContainer                      AdapteeType;

  typedef typename AdapteeType::Element            ElementType;
  typedef typename AdapteeType::STLContainerType   TargetType;
  
private:

  AdapteeType & m_AdapteeRef;
  
  /** hide the copy constructor to allow only direct construction of the adapter */
  STLContainerAdaptor(const STLContainerAdaptor & r);
  
  /* hide and avoid operator= */
  const STLContainerAdaptor & operator=(const STLContainerAdaptor & r);
  
  
public:
  STLContainerAdaptor(AdapteeType & adaptee)
    : m_AdapteeRef(adaptee)
  {}
  
  STLContainerAdaptor(AdapteeType * adaptee)
    : m_AdapteeRef(*adaptee)
  {}
  
  ~STLContainerAdaptor()
  {
    m_AdapteeRef.Modified();
  }
  
  TargetType & GetSTLContainerRef()
  {
    return m_AdapteeRef.CastToSTLContainer(); 
  }
  
};


} // end namespace itk

#endif
