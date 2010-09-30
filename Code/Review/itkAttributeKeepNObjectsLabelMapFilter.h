/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAttributeKeepNObjectsLabelMapFilter.h,v $
  Language:  C++
  Date:      $Date: 2006/03/28 19:59:05 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAttributeKeepNObjectsLabelMapFilter_h
#define __itkAttributeKeepNObjectsLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"

namespace itk {
/** \class AttributeKeepNObjectsLabelMapFilter
 * \brief keep N objects according to their attribute value
 *
 * AttributeKeepNObjectsLabelMapFilter keeps the N objects in a label collection image
 * with the highest (or lowest) attribute value. The attribute is provided by an
 * attribute accessor given in template parameter.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TImage, class TAttributeAccessor=
    typename Functor::AttributeLabelObjectAccessor< typename TImage::LabelObjectType > >
class ITK_EXPORT AttributeKeepNObjectsLabelMapFilter :
    public InPlaceLabelMapFilter<TImage>
{
public:
  /** Standard class typedefs. */
  typedef AttributeKeepNObjectsLabelMapFilter Self;
  typedef InPlaceLabelMapFilter<TImage>       Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;

  typedef TAttributeAccessor AttributeAccessorType;
  typedef typename AttributeAccessorType::AttributeValueType AttributeValueType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AttributeKeepNObjectsLabelMapFilter,
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
   * Set/Get the ordering of the objects. By default, the ones with the
   * highest value are kept. Turming ReverseOrdering to true make this filter
   * keep the objects with the smallest values
   */
  itkSetMacro( ReverseOrdering, bool );
  itkGetConstReferenceMacro( ReverseOrdering, bool );
  itkBooleanMacro( ReverseOrdering );

  /**
   * Set/Get the number of objects to keep
   */
  itkSetMacro( NumberOfObjects, unsigned long );
  itkGetConstReferenceMacro( NumberOfObjects, unsigned long );

protected:
  AttributeKeepNObjectsLabelMapFilter();
  ~AttributeKeepNObjectsLabelMapFilter() {};

  void GenerateData();

  void PrintSelf(std::ostream& os, Indent indent) const;

  class ReverseComparator
    {
    public:
    bool operator()( const typename LabelObjectType::Pointer & a, const typename LabelObjectType::Pointer & b )
      {
      return accessor( a ) < accessor( b );
      }
     AttributeAccessorType accessor;
    };

  class Comparator
    {
  public:
    bool operator()( const typename LabelObjectType::Pointer & a, const typename LabelObjectType::Pointer & b )
      {
      return accessor( a ) > accessor( b );
      }
    AttributeAccessorType accessor;
    };

private:
  AttributeKeepNObjectsLabelMapFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  bool          m_ReverseOrdering;
  unsigned long m_NumberOfObjects;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAttributeKeepNObjectsLabelMapFilter.txx"
#endif

#endif
