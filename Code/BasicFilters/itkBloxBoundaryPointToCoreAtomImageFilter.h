/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryPointToCoreAtomImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxBoundaryPointToCoreAtomImageFilter_h
#define __itkBloxBoundaryPointToCoreAtomImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkBloxBoundaryPointPixel.h"
#include "itkBloxBoundaryPointImage.h"
#include "itkBloxCoreAtomItem.h"
#include "itkBloxCoreAtomPixel.h"
#include "itkBloxCoreAtomImage.h"

namespace itk
{

/** \class BloxBoundaryPointToCoreAtomImageFilter
 * \brief Converts a gradient image to an BloxImage of BloxBoundaryPoints
 *
 * Thresholds the magnitude of a gradient image to produce
 * a BloxBoundaryPointImage
 * 
 * \ingroup ImageEnhancement
 */
template<unsigned int dim>
class ITK_EXPORT BloxBoundaryPointToCoreAtomImageFilter :
    public ImageToImageFilter< BloxBoundaryPointImage<dim>,
                               BloxCoreAtomImage<dim> >
{
public:
  /** Standard class typedefs. */
  typedef BloxBoundaryPointToCoreAtomImageFilter Self;
  typedef ImageToImageFilter<BloxBoundaryPointImage<dim>,
                             BloxCoreAtomImage<dim> > Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( BloxBoundaryPointToCoreAtomImageFilter, ImageToImageFilter );

  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, dim);

  /** typedef for images */
  typedef BloxBoundaryPointImage<dim>             TInputImage;
  typedef BloxBoundaryPointImage<dim>             InputImageType;
  typedef BloxCoreAtomImage<dim>                  TOutputImage;
  typedef BloxCoreAtomImage<dim>                  OutputImageType;
  typedef typename OutputImageType::Pointer       OutputImagePointer;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;

  /** Image size typedef */
  typedef Size<dim> SizeType;

  /** Image index typedef */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image pixel value typedef */
  typedef typename TOutputImage::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** The type of Point used to convert between physical and blox space */
  typedef Point<double, dim> PositionType;

  /** The vector between two points */
  typedef typename PositionType::VectorType VectorType;

  /** How we represent gradients. */
  typedef CovariantVector<double, dim> GradientType;

  /** Walk the input image, find core atoms, store them.  */
  void FindCoreAtoms();

  /** Find core atoms given a specific boundary point. */
  void FindCoreAtomsAtBoundaryPoint(BloxBoundaryPointItem<dim>* pItem);

  /** Gets and sets for member variables. */
  itkSetMacro(DistanceMin, double);
  itkSetMacro(DistanceMax, double);
  itkSetMacro(Epsilon, double);
  itkSetMacro(Polarity, bool);

  /** Overload of base class function to generate input region */
  void GenerateInputRequestedRegion();

protected:
  BloxBoundaryPointToCoreAtomImageFilter();
  virtual ~BloxBoundaryPointToCoreAtomImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Method for forming the BloxBoundaryPointImage. */
  void GenerateData();

private:
  BloxBoundaryPointToCoreAtomImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Pointers to input and output images */
  InputImageConstPointer  m_InputPtr;
  OutputImagePointer      m_OutputPtr;
  
  /** Parameters used to establish conic shell iterator regions.
   * See the documentation for itkConicShellInteriorExteriorSpatialFunction
   * for how these affect the iterator. */
  double m_DistanceMin;
  double m_DistanceMax;
  double m_Epsilon;
  bool m_Polarity;

  /**Parameters for progress update*/
  float m_InverseNumberOfBoundaryPoints;
  unsigned long m_CurrentBoundaryPoint;
  unsigned long m_BoundaryPointsPerUpdate;
  unsigned long m_BoundaryPointsBeforeUpdate;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryPointToCoreAtomImageFilter.txx"
#endif

#endif
