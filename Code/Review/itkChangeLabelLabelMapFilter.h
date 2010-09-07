/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChangeLabelLabelMapFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkChangeLabelLabelMapFilter_h
#define __itkChangeLabelLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"
#include <map>

namespace itk
{
/** \class ChangeLabelLabelMapFilter
 * \brief  Replace the label Ids of selected LabelObjects with new label Ids.
 *
 * This filter takes as input a label map and a list of pairs of Label Ids, to
 * produce as output a new label map where the label Ids have been replaced
 * according to the pairs in the list.
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
class ITK_EXPORT ChangeLabelLabelMapFilter:
  public InPlaceLabelMapFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ChangeLabelLabelMapFilter       Self;
  typedef InPlaceLabelMapFilter< TImage > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                              ImageType;
  typedef typename ImageType::Pointer         ImagePointer;
  typedef typename ImageType::ConstPointer    ImageConstPointer;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::IndexType       IndexType;
  typedef typename ImageType::LabelObjectType LabelObjectType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ChangeLabelLabelMapFilter, InPlaceLabelMapFilter);

  typedef typename std::map< PixelType, PixelType > ChangeMapType;
  typedef typename ChangeMapType::const_iterator    ChangeMapIterator;

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
   */
  void SetChangeMap(const ChangeMapType & changeMap);

  const ChangeMapType & GetChangeMap() const;

  void SetChange(const PixelType & oldLabel, const PixelType & newLabel);

  void ClearChangeMap();

protected:
  ChangeLabelLabelMapFilter();
  ~ChangeLabelLabelMapFilter() {}

  void GenerateData();

  void PrintSelf(std::ostream & os, Indent indent) const;

  ChangeMapType m_MapOfLabelToBeReplaced;
private:
  ChangeLabelLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);            //purposely not implemented
};                                         // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkChangeLabelLabelMapFilter.txx"
#endif

#endif
