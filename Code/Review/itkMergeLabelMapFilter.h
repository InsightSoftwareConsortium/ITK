/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMergeLabelMapFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMergeLabelMapFilter_h
#define __itkMergeLabelMapFilter_h

#include "itkInPlaceLabelMapFilter.h"

namespace itk
{
/** \class MergeLabelMapFilter
 * \brief Merges two Label Maps using different methods to create the product.
 *
 * This filter takes two input Label Map and takes an additional integer to determine the method that will
 * be used to merge the two Label Maps. The integers are read as follows:
 *   KEEP = 0,
 *   AGGREGATE = 1,
 *   PACK = 2,
 *   STRICT = 3
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
class ITK_EXPORT MergeLabelMapFilter:
  public InPlaceLabelMapFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef MergeLabelMapFilter             Self;
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
  typedef typename LabelObjectType::Pointer   LabelObjectPointer;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MergeLabelMapFilter, InPlaceLabelMapFilter);

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

#ifdef STRICT
#undef STRICT
#endif
  typedef enum {
    KEEP = 0,
    AGGREGATE = 1,
    PACK = 2,
    STRICT = 3
    } MethodChoice;

  itkSetMacro(Method, MethodChoice);
  itkGetConstReferenceMacro(Method, MethodChoice);
protected:
  MergeLabelMapFilter();
  ~MergeLabelMapFilter() {}

  void GenerateData();

  void PrintSelf(std::ostream & os, Indent indent) const;

  typedef typename ImageType::LabelObjectContainerType LabelObjectContainerType;
  typedef typename LabelObjectType::LineContainerType  LineContainerType;
  typedef typename LineContainerType::const_iterator   LineContainerIterator;

  MethodChoice m_Method;
private:
  MergeLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  void MergeWithKeep();

  void MergeWithAggregate();

  void MergeWithPack();

  void MergeWithStrict();
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMergeLabelMapFilter.txx"
#endif

#endif
