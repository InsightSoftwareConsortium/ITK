/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkLabelUniqueLabelMapFilter.h,v $
  Language:  C++
  Date:      $Date: 2006/03/28 19:59:05 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelUniqueLabelMapFilter_h
#define __itkLabelUniqueLabelMapFilter_h

#include "itkAttributeUniqueLabelMapFilter.h"
#include "itkLabelObject.h"
#include "itkLabelObjectAccessors.h"
#include <set>


namespace itk {
/** \class LabelUniqueLabelMapFilter
 * \brief Make sure that the objects are not overlapping
 *
 * AttributeUniqueLabelMapFilter search the overlapping zones in the overlapping
 * objects and keeps only a single object on all the pixels of the image.
 * The object to keep is selected according to their label.
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
template<class TImage>
class ITK_EXPORT LabelUniqueLabelMapFilter :
    public AttributeUniqueLabelMapFilter<TImage, typename Functor::LabelLabelObjectAccessor< typename TImage::LabelObjectType > >
{
public:
  /** Standard class typedefs. */
  typedef LabelUniqueLabelMapFilter Self;
  typedef AttributeUniqueLabelMapFilter<TImage, typename Functor::LabelLabelObjectAccessor< typename TImage::LabelObjectType > >
                                       Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;

  typedef typename Superclass::AttributeAccessorType AttributeAccessorType;
  typedef typename Superclass::AttributeValueType    AttributeValueType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelUniqueLabelMapFilter,
               AttributeUniqueLabelMapFilter);

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
  LabelUniqueLabelMapFilter() {};
  ~LabelUniqueLabelMapFilter() {};

private:
  LabelUniqueLabelMapFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

}; // end of class

} // end namespace itk

#endif
