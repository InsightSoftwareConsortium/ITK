/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShiftScaleLabelMapFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShiftScaleLabelMapFilter_h
#define __itkShiftScaleLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"

namespace itk
{
/** \class ShiftScaleLabelMapFilter
 * \brief Shifts and scales a label map filter, giving the option to change the background value.
 *
 * This filter takes as input a label map and shift, scale and background values
 * to produce, as output, a rescaled and shifted label map with, when applicable, a
 * new background.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TImage >
class ITK_EXPORT ShiftScaleLabelMapFilter:
  public InPlaceLabelMapFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ShiftScaleLabelMapFilter        Self;
  typedef InPlaceLabelMapFilter< TImage > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                                        ImageType;
  typedef typename ImageType::Pointer                   ImagePointer;
  typedef typename ImageType::ConstPointer              ImageConstPointer;
  typedef typename ImageType::PixelType                 PixelType;
  typedef typename ImageType::IndexType                 IndexType;
  typedef typename ImageType::LabelObjectType           LabelObjectType;
  typedef typename Superclass::LabelObjectContainerType LabelObjectContainerType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ShiftScaleLabelMapFilter, InPlaceLabelMapFilter);

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

  itkSetMacro(Shift, double);
  itkGetConstReferenceMacro(Shift, double);

  itkSetMacro(Scale, double);
  itkGetConstReferenceMacro(Scale, double);

  itkSetMacro(ChangeBackgroundValue, bool);
  itkGetConstMacro(ChangeBackgroundValue, bool);
  itkBooleanMacro(ChangeBackgroundValue);
protected:
  ShiftScaleLabelMapFilter();
  ~ShiftScaleLabelMapFilter() {}

  void GenerateData();

  void PrintSelf(std::ostream & os, Indent indent) const;

  double m_Shift;
  double m_Scale;

  bool m_ChangeBackgroundValue;
private:
  ShiftScaleLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};                                        // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShiftScaleLabelMapFilter.txx"
#endif

#endif
