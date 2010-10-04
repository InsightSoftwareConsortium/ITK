/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAttributePositionLabelMapFilter.h,v $
  Language:  C++
  Date:      $Date: 2006/03/28 19:59:05 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAttributePositionLabelMapFilter_h
#define __itkAttributePositionLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include "itkAttributeLabelObject.h"

namespace itk {
/** \class AttributePositionLabelMapFilter
 * \brief Mark a single pixel in the label object which correspond to a position given by an attribute
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa AttributeLabelObject
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TImage, class TAttributeAccessor=
    typename Functor::AttributeLabelObjectAccessor< typename TImage::LabelObjectType >, bool VPhysicalPosition=true >
class ITK_EXPORT AttributePositionLabelMapFilter :
    public InPlaceLabelMapFilter<TImage>
{
public:
  /** Standard class typedefs. */
  typedef AttributePositionLabelMapFilter Self;
  typedef InPlaceLabelMapFilter<TImage>   Superclass;
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;

  typedef TAttributeAccessor                                 AttributeAccessorType;
  typedef typename AttributeAccessorType::AttributeValueType AttributeValueType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AttributePositionLabelMapFilter,
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

protected:
  AttributePositionLabelMapFilter();
  ~AttributePositionLabelMapFilter() {};

  virtual void ThreadedProcessLabelObject( LabelObjectType * labelObject );

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  AttributePositionLabelMapFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAttributePositionLabelMapFilter.txx"
#endif

#endif
