/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSTLConstContainerAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSTLConstContainerAdaptor_h
#define __itkSTLConstContainerAdaptor_h

/** \class STLConstContainerAdaptor
 * An adapter object that casts a [const itk::XxxContainer] into [const std::xxx]
 * and enables access to the underlying data structure.
 * The class is provided for interface consistency with STLContainerAdaptor
 * plus the [const] modifier. Since everything is const, there is no need to call
 * AdapteeType::Modified() in the destructor.
 * Here's a usage example of STLContainerAdaptor
 *     itk::STLConstContainerAdaptor<itk::VectorContainer<unsigned long, ElementType>> vecAdaptor(aContainer);
 *     const std::vector<ElementType> & vec = vecAdaptor.GetSTLContainerRef();
 *     // do things with vec ...
 */

namespace itk {


template<typename TContainer>
class STLConstContainerAdaptor
{
public:

  typedef const TContainer                      AdapteeType;

  typedef const typename AdapteeType::Element            ElementType;
  typedef const typename AdapteeType::STLContainerType   TargetType;
  

private:

  AdapteeType & m_AdapteeRef;
  
  /** hide the copy constructor to allow only direct construction of the adapter */
  STLConstContainerAdaptor(const STLConstContainerAdaptor & r);
  
  /* hide and avoid operator= */
  const STLConstContainerAdaptor & operator=(const STLConstContainerAdaptor & r);
  
  
public:

  STLConstContainerAdaptor(AdapteeType & adaptee)
    : m_AdapteeRef(adaptee)
  {}
  
  STLConstContainerAdaptor(AdapteeType * adaptee)
    : m_AdapteeRef(*adaptee)
  {}
  
  TargetType & GetSTLConstContainerRef()
  {
    return m_AdapteeRef.CastToSTLConstContainer(); 
  }
  
};


}   // end namespace itk

#endif
