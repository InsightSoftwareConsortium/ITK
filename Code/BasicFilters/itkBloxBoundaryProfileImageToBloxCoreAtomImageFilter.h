/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter_h
#define __itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "itkBloxBoundaryProfileItem.h"
#include "itkBloxBoundaryProfilePixel.h"
#include "itkBloxBoundaryProfileImage.h"
#include "itkBloxCoreAtomItem.h"
#include "itkBloxCoreAtomPixel.h"
#include "itkBloxCoreAtomImage.h"

namespace itk
{

/**
 * Converts an blox boundary profile image to an image of core atoms.
 */
template< typename TSourceImage, unsigned int dim >
class ITK_EXPORT BloxBoundaryProfileImageToBloxCoreAtomImageFilter :
public ImageToImageFilter< BloxBoundaryProfileImage<dim>,
                           BloxCoreAtomImage<dim> >
{
public:
  /** Standard class typedefs. */
  typedef BloxBoundaryProfileImageToBloxCoreAtomImageFilter Self;
  typedef ImageToImageFilter<BloxBoundaryProfileImage<dim>,
                             BloxCoreAtomImage<dim> > Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter, ImageToImageFilter );

  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TSourceImage::ImageDimension);

  typedef BloxCoreAtomImage<dim>                  TOutputImage;
  typedef BloxCoreAtomImage<dim>                  OutputImageType;
  typedef typename OutputImageType::Pointer       OutputImagePointer;

  /** Typedef for boundary point image */
  typedef          BloxBoundaryProfileImage<dim>          BoundaryProfileImageType;
  typedef typename BoundaryProfileImageType::Pointer      BoundaryProfileImagePointer;
  typedef typename BoundaryProfileImageType::RegionType   BoundaryProfileImageRegionType; 
  typedef typename BoundaryProfileImageType::PixelType    BoundaryProfileImagePixelType; 
  typedef typename BoundaryProfileImageType::ConstPointer BoundaryProfileImageConstPointer;

  /** Typedef for blurred source image */
  typedef          TSourceImage                         SourceImageType;
  typedef typename SourceImageType::Pointer             SourceImagePointer;
  typedef typename SourceImageType::RegionType          SourceImageRegionType; 
  typedef typename SourceImageType::PixelType           SourceImagePixelType; 
  typedef typename SourceImageType::ConstPointer        SourceImageConstPointer;
  typedef typename SourceImageType::IndexType           SourceImageIndexType;

  /** Image size typedef */
  typedef Size<itkGetStaticConstMacro(NDimensions)> SizeType;

  /** Image index typedef */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image pixel value typedef */
  typedef typename TOutputImage::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** The type of Point used to convert between physical and blox space */
  typedef Point<double, itkGetStaticConstMacro(NDimensions)> PositionType;

  /** The vector between two points */
  typedef typename PositionType::VectorType VectorType;

  /** How we represent gradients. */
  typedef CovariantVector<double, itkGetStaticConstMacro(NDimensions)> GradientType;

  /** Walk the input image, find core atoms, store them.  */
  void FindCoreAtoms();

  /** Find core atoms given a specific boundary point. */
  void FindCoreAtomsAtBoundaryPoint(BloxBoundaryProfileItem<dim>* pItem);

  /** Gets and sets for member variables. */
  itkSetMacro(DistanceMin, double);
  itkSetMacro(DistanceMax, double);
  itkSetMacro(Epsilon, double);
  itkSetMacro(Polarity, bool);

  /** Overload of base class function to generate input region */
  void GenerateInputRequestedRegion();

  /** Set the two input images **/
  void SetInput1(const SourceImageType * image1);
  void SetInput2(const BoundaryProfileImageType * image2);


protected:
  BloxBoundaryProfileImageToBloxCoreAtomImageFilter();
  virtual ~BloxBoundaryProfileImageToBloxCoreAtomImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Method for forming the BloxBoundaryPointImage. */
  void GenerateData();

private:
  BloxBoundaryProfileImageToBloxCoreAtomImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Pointers to input and output images */
  BoundaryProfileImageConstPointer  m_BoundaryProfileImagePtr;
  SourceImageConstPointer m_SourceImagePtr;
  OutputImagePointer      m_OutputPtr;
  
  /** Parameters used to establish conic shell iterator regions.
   * See the documentation for itkConicShellInteriorExteriorSpatialFunction
   * for how these affect the iterator. */
  double m_DistanceMin;
  double m_DistanceMax;
  double m_Epsilon;
  bool m_Polarity;
  bool m_IntensityFlag;
  unsigned int m_IntensityThreshold;
  bool m_CreateCoreAtom;
  int m_CoreAtomsCreated;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter.txx"
#endif

#endif
