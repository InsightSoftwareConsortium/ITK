/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAttributeSelectionLabelMapFilter.h,v $
  Language:  C++
  Date:      $Date: 2006/03/28 19:59:05 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAttributeSelectionLabelMapFilter_h
#define __itkAttributeSelectionLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"
#include <set>


namespace itk {
/** \class AttributeSelectionLabelMapFilter
 * \brief remove the objects according to the value of their attribute
 *
 * AttributeSelectionLabelMapFilter removes the objects in a label collection image
 * with an attribute value inside or outside a set of attribute values passed by
 * the user.
 * The attribute is provide by an attribute accessor given in template parameter.
 * Contrary to the other filters made to remove some object of a LabelMap, no
 * ordering relation for the attribute is needed in that filter.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa AttributeLabelObject
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TImage, class TAttributeAccessor=
    typename Functor::AttributeLabelObjectAccessor< typename TImage::LabelObjectType > >
class ITK_EXPORT AttributeSelectionLabelMapFilter :
    public InPlaceLabelMapFilter<TImage>
{
public:
  /** Standard class typedefs. */
  typedef AttributeSelectionLabelMapFilter Self;
  typedef InPlaceLabelMapFilter<TImage>    Superclass;
  typedef SmartPointer<Self>               Pointer;
  typedef SmartPointer<const Self>         ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;

  typedef TAttributeAccessor                                 AttributeAccessorType;
  typedef typename AttributeAccessorType::AttributeValueType AttributeValueType;

  typedef typename std::set<AttributeValueType> AttributeSetType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AttributeSelectionLabelMapFilter,
               InPlaceLabelMapFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
/*  itkConceptMacro(InputEqualityComparableCheck,
    (Concept::EqualityComparable<InputImagePixelType>));
  itkConceptMacro(IntConvertibleToInputCheck,
    (Concept::Convertible<int, InputImagePixelType>));
  itkConceptMacro(InputOStreamWritableCheck,
    (Concept::OStreamWritable<InputImagePixelType>));*/
  /** End concept checking */
#endif

  /**
   * Set/Get the threshold used to keep or remove the objects.
   */
  const AttributeSetType & GetAttributeSet() const
    {
    return m_AttributeSet;
    }
  void SetAttributeSet( const AttributeSetType & set )
    {
    m_AttributeSet = set;
    this->Modified();
    }

  /**
   * Set/Get whether the objects with the specified attribute values should be kept
   * or excluded.
   */
  itkGetConstMacro( Exclude, bool );
  itkSetMacro( Exclude, bool );
  itkBooleanMacro( Exclude );

  /** Clear the attribute set, and add the attribute passed in parameter */
  void SetAttribute( const AttributeValueType & attr )
    {
    this->ClearAttributeSet();
    this->AddAttribute( attr );
    }

  void ClearAttributeSet()
    {
    if( ! m_AttributeSet.empty() )
      {
      m_AttributeSet.clear();
      this->Modified();
      }
    }

  void AddAttribute(  const AttributeValueType & attr )
    {
    unsigned long size = m_AttributeSet.size();
    m_AttributeSet.insert( attr );
    if( size != m_AttributeSet.size() )
      {
      this->Modified();
      }
    }

protected:
  AttributeSelectionLabelMapFilter();
  ~AttributeSelectionLabelMapFilter() {};

  void GenerateData();

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  AttributeSelectionLabelMapFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  AttributeSetType m_AttributeSet;
  bool             m_Exclude;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAttributeSelectionLabelMapFilter.txx"
#endif

#endif
