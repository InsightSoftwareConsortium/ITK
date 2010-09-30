/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAttributeOpeningLabelMapFilter.h,v $
  Language:  C++
  Date:      $Date: 2006/03/28 19:59:05 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAttributeOpeningLabelMapFilter_h
#define __itkAttributeOpeningLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"

namespace itk {
/** \class AttributeOpeningLabelMapFilter
 * \brief remove the objects according to the value of their attribute
 *
 * AttributeOpeningLabelMapFilter removes the objects in a label collection image
 * with an attribute value smaller or greater than a threshold called Lambda.
 * The attribute is provide by an attribute accessor given in template parameter.
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
class ITK_EXPORT AttributeOpeningLabelMapFilter :
    public InPlaceLabelMapFilter<TImage>
{
public:
  /** Standard class typedefs. */
  typedef AttributeOpeningLabelMapFilter Self;
  typedef InPlaceLabelMapFilter<TImage>  Superclass;
  typedef SmartPointer<Self>             Pointer;
  typedef SmartPointer<const Self>       ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;

  typedef TAttributeAccessor                                 AttributeAccessorType;
  typedef typename AttributeAccessorType::AttributeValueType AttributeValueType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AttributeOpeningLabelMapFilter,
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
  itkGetConstMacro(Lambda, AttributeValueType);
  itkSetMacro(Lambda, AttributeValueType);

  /**
   * Set/Get the ordering of the objects. By default, the objects with
   * an attribute value smaller than Lamba are removed. Turning ReverseOrdering
   * to true make this filter remove the object with an attribute value greater
   * than Lambda instead.
   */
  itkGetConstMacro( ReverseOrdering, bool );
  itkSetMacro( ReverseOrdering, bool );
  itkBooleanMacro( ReverseOrdering );

protected:
  AttributeOpeningLabelMapFilter();
  ~AttributeOpeningLabelMapFilter() {};

  void GenerateData();

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  AttributeOpeningLabelMapFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  AttributeValueType m_Lambda;
  bool               m_ReverseOrdering;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAttributeOpeningLabelMapFilter.txx"
#endif

#endif
