/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelObjectAccessors.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelObjectAccessors_h
#define __itkLabelObjectAccessors_h


namespace itk
{


namespace Functor 
{

template< class TLabelObject >
class ITK_EXPORT LabelLabelObjectAccessor
{
public:
  typedef TLabelObject                        LabelObjectType;
  typedef typename LabelObjectType::LabelType AttributeValueType;

  inline const AttributeValueType operator()( const LabelObjectType * labelObject )
    {
    return labelObject->GetLabel();
    }
};

template< class TLabelObject >
class ITK_EXPORT NumberOfLinesLabelObjectAccessor
{
public:
  typedef TLabelObject     LabelObjectType;
  typedef int              AttributeValueType;

  inline const AttributeValueType operator()( const LabelObjectType * labelObject )
    {
    return labelObject->GetNumberOfLines();
    }
};

template< class TLabelObject, class TAttributeAccessor >
class LabelObjectComparator
{
public:
  typedef TLabelObject       LabelObjectType;
  typedef TAttributeAccessor AttributeAccessorType;
  bool operator()( const LabelObjectType * a, const LabelObjectType * b )
    {
    return m_Accessor( a ) > m_Accessor( b );
    }
private:
  AttributeAccessorType m_Accessor;
};

template< class TLabelObject, class TAttributeAccessor >
class LabelObjectReverseComparator
{
public:
  typedef TLabelObject       LabelObjectType;
  typedef TAttributeAccessor AttributeAccessorType;
  bool operator()( const LabelObjectType * a, const LabelObjectType * b )
    {
    return m_Accessor( a ) < m_Accessor( b );
    }
private:
  AttributeAccessorType m_Accessor;
};

}


} // end namespace itk

#endif
