/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsRelabelLabelMapFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStatisticsRelabelLabelMapFilter_h
#define __itkStatisticsRelabelLabelMapFilter_h

#include "itkShapeRelabelLabelMapFilter.h"
#include "itkStatisticsLabelObject.h"
#include "itkStatisticsLabelObjectAccessors.h"

namespace itk
{
/** \class StatisticsRelabelLabelMapFilter
 * \brief relabel objects according to their shape attributes
 *
 * StatisticsRelabelLabelMapFilter relabel a label collection image according to the statistics attributes of
 * the objects. The label produced are always consecutives.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 *
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa StatisticsLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TImage >
class ITK_EXPORT StatisticsRelabelLabelMapFilter:
  public ShapeRelabelLabelMapFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef StatisticsRelabelLabelMapFilter      Self;
  typedef ShapeRelabelLabelMapFilter< TImage > Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

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
  itkTypeMacro(StatisticsRelabelLabelMapFilter,
               ShapeRelabelLabelMapFilter);

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
  StatisticsRelabelLabelMapFilter();
  ~StatisticsRelabelLabelMapFilter() {}

  void GenerateData();

private:
  StatisticsRelabelLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                  //purposely not implemented
};                                               // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatisticsRelabelLabelMapFilter.txx"
#endif

#endif
