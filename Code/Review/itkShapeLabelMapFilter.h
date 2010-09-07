/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeLabelMapFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapeLabelMapFilter_h
#define __itkShapeLabelMapFilter_h
#include "itkLabelPerimeterEstimationCalculator.h"

#include "itkInPlaceLabelMapFilter.h"

namespace itk
{
/** \class ShapeLabelMapFilter
 * \brief The valuator class for the ShapeLabelObject
 *
 * ShapeLabelMapFilter can be used to set the attributes values of the
 * ShapeLabelObject in a LabelMap.
 *
 * ShapeLabelMapFilter takes an optional parameter, used only to
 * optimize the computation time and the memory usage when the
 * perimeter or the feret diameter is used: the exact copy of the
 * input LabelMap is stored in an Image.
 * It can be set with SetLabelImage(). It is cleared at the end of the
 * computation, so must be reset before running Update() again. It is
 * not part of the pipeline management design, to let the subclasses
 * of ShapeLabelMapFilter use the pipeline design to specify truly
 * required inputs.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */

template< class TImage, class TLabelImage =
            Image< ITK_TYPENAME TImage::PixelType, ::itk::GetImageDimension< TImage >::ImageDimension > >
class ITK_EXPORT ShapeLabelMapFilter:
  public
  InPlaceLabelMapFilter< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ShapeLabelMapFilter             Self;
  typedef InPlaceLabelMapFilter< TImage > Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Some convenient typedefs. */
  typedef TImage                               ImageType;
  typedef typename ImageType::Pointer          ImagePointer;
  typedef typename ImageType::ConstPointer     ImageConstPointer;
  typedef typename ImageType::PixelType        PixelType;
  typedef typename ImageType::IndexType        IndexType;
  typedef typename ImageType::SizeType         SizeType;
  typedef typename ImageType::LabelObjectType  LabelObjectType;
  typedef typename LabelObjectType::MatrixType MatrixType;
  typedef typename LabelObjectType::VectorType VectorType;

  typedef TLabelImage                           LabelImageType;
  typedef typename LabelImageType::Pointer      LabelImagePointer;
  typedef typename LabelImageType::ConstPointer LabelImageConstPointer;
  typedef typename LabelImageType::PixelType    LabelPixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  typedef LabelPerimeterEstimationCalculator< LabelImageType > PerimeterCalculatorType;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ShapeLabelMapFilter, InPlaceLabelMapFilter);

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
   * Set/Get whether the maximum Feret diameter should be computed or not.
   * Default value is false because of the high computation time required.
   */
  itkSetMacro(ComputeFeretDiameter, bool);
  itkGetConstReferenceMacro(ComputeFeretDiameter, bool);
  itkBooleanMacro(ComputeFeretDiameter);

  /**
   * Set/Get whether the perimeter should be computed or not.
   * Default value is false because of the high computation time required.
   */
  itkSetMacro(ComputePerimeter, bool);
  itkGetConstReferenceMacro(ComputePerimeter, bool);
  itkBooleanMacro(ComputePerimeter);

  /** Set the label image */
  void SetLabelImage(const TLabelImage *input)
  {
    m_LabelImage = input;
  }

protected:
  ShapeLabelMapFilter();
  ~ShapeLabelMapFilter() {}

  virtual void ThreadedProcessLabelObject(LabelObjectType *labelObject);

  virtual void BeforeThreadedGenerateData();

  virtual void AfterThreadedGenerateData();

  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  ShapeLabelMapFilter(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  bool                   m_ComputeFeretDiameter;
  bool                   m_ComputePerimeter;
  LabelImageConstPointer m_LabelImage;

  typename PerimeterCalculatorType::Pointer m_PerimeterCalculator;

  void ComputeFeretDiameter(LabelObjectType *labelObject);

  /** utilities */
  static long Factorial(const long n);

  static long DoubleFactorial(const long n);

  static double GammaN2p1(const long n);

  static double HyperSphereVolume(const double radius);

  static double HyperSpherePerimeter(const double radius);

  static double HyperSphereRadiusFromVolume(const double volume);
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapeLabelMapFilter.txx"
#endif

#endif
