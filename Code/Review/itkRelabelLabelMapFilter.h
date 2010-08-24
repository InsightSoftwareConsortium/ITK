/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRelabelLabelMapFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRelabelLabelMapFilter_h
#define __itkRelabelLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"

namespace itk
{
/** \class RelabelLabelMapFilter
 * \brief This filter relabels the LabelObjects; the new labels are arranged
 * consecutively with consideration for the background value.
 *
 * This filter takes the LabelObjects from the input and reassigns them to the
 * output by calling the PushLabelObject method, which by default, attempts to
 * reorganize the labels consecutively. The user can assign an arbitrary value
 * to the background; the filter will assign the labels consecutively by
 * skipping the background value.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ShapeLabelObject, RelabelComponentImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template< class TImage >
class ITK_EXPORT RelabelLabelMapFilter:
  public InPlaceLabelMapFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef RelabelLabelMapFilter           Self;
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
  itkTypeMacro(RelabelLabelMapFilter, InPlaceLabelMapFilter);

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
  RelabelLabelMapFilter() {}
  ~RelabelLabelMapFilter() {}

  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateData();

private:
  RelabelLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);        //purposely not implemented
};                                     // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRelabelLabelMapFilter.txx"
#endif

#endif
