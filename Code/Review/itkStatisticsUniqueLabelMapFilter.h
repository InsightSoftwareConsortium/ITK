/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsUniqueLabelMapFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStatisticsUniqueLabelMapFilter_h
#define __itkStatisticsUniqueLabelMapFilter_h

#include "itkShapeUniqueLabelMapFilter.h"
#include "itkStatisticsLabelObject.h"

namespace itk
{
/** \class StatisticsUniqueLabelMapFilter
 * \brief Remove some pixels in the label object according to the value of their statistics attribute to ensure that a pixel is not in to objects
 *
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa ShapeLabelObject, BinaryShapeOpeningImageFilter, LabelStatisticsOpeningImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TImage >
class ITK_EXPORT StatisticsUniqueLabelMapFilter:
  public ShapeUniqueLabelMapFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef StatisticsUniqueLabelMapFilter      Self;
  typedef ShapeUniqueLabelMapFilter< TImage > Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;

  typedef typename LabelObjectType::AttributeType AttributeType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(StatisticsUniqueLabelMapFilter,
               ShapeUniqueLabelMapFilter);

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
  StatisticsUniqueLabelMapFilter();
  ~StatisticsUniqueLabelMapFilter() {}

  void GenerateData();

private:
  StatisticsUniqueLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented
};                                              // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatisticsUniqueLabelMapFilter.txx"
#endif

#endif
