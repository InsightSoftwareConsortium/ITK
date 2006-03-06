/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
 * Converts a blox boundary profile image to an image of core atoms.
 */
template< typename TInputImage, typename TOutputImage, typename TSourceImage >
class ITK_EXPORT BloxBoundaryProfileImageToBloxCoreAtomImageFilter :
public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BloxBoundaryProfileImageToBloxCoreAtomImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter, ImageToImageFilter );

  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TSourceImage::ImageDimension);

  /** Typedefs */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType; 
  typedef typename OutputImageType::PixelType OutputImagePixelType; 
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::IndexType OutputImageIndexType;

  typedef TSourceImage SourceImageType;
  typedef typename SourceImageType::Pointer SourceImagePointer;
  typedef typename SourceImageType::RegionType SourceImageRegionType; 
  typedef typename SourceImageType::PixelType SourceImagePixelType; 
  typedef typename SourceImageType::ConstPointer SourceImageConstPointer;
  typedef typename SourceImageType::IndexType SourceImageIndexType;

  /** Image size typedef */
  typedef Size<itkGetStaticConstMacro(NDimensions)> SizeType;

  /** The type of Point used to convert between physical and blox space */
  typedef Point<double, itkGetStaticConstMacro(NDimensions)> PositionType;

  /** The vector between two points */
  typedef typename PositionType::VectorType VectorType;

  /** How we represent gradients. */
  typedef CovariantVector<double, itkGetStaticConstMacro(NDimensions)> GradientType;

  /** Walk the input image, find core atoms, store them.  */
  void FindCoreAtoms();

  /** Find core atoms given a specific boundary point. */
  typedef BloxBoundaryProfileItem<itkGetStaticConstMacro(NDimensions)> BloxProfileItemType;
  void FindCoreAtomsAtBoundaryPoint(BloxProfileItemType* pItem);

  /** Gets and sets for member variables. */
  itkSetMacro(DistanceMin, double);
  itkSetMacro(DistanceMax, double);
  itkSetMacro(Epsilon, double);
  itkSetMacro(Polarity, bool);

  /** Overload of base class function to generate input region */
  void GenerateInputRequestedRegion();

  /** Set the two input images **/
  void SetInput1(const SourceImageType * image1);
  void SetInput2(const InputImageType * image2);

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
  InputImageConstPointer  m_BoundaryProfileImagePtr;
  OutputImagePointer      m_OutputPtr;
  SourceImageConstPointer m_SourceImagePtr;
  
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
