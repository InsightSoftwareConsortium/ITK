/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkOrienterImageFilter.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOrienterImageFilter_h
#define __itkOrienterImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkIOCommon.h"
#include "itkPermuteAxesImageFilter.h"
#include "itkFlipImageFilter.h"

namespace itk {

/** \class OrienterImageFilter
 * \brief Permute axes and then flip images as needed to obtain
 *  agreement in coordinateOrientation codes.
 *
 * OrienterImageFilter is designed to use the
 *
 * This filter uses the HMaximaImageFilter.
 *
 * Geodesic morphology and the H-Convex algorithm is described in
 * Chapter 6 of Pierre Soille's book "Morphological Image Analysis:
 * Principles and Applications", Second Edition, Springer, 2003.
 *
 * \sa GrayscaleGeodesicDilateImageFilter, HMinimaImageFilter
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT OrienterImageFilter :
    public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef OrienterImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage>
  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::Pointer         InputImagePointer;
  typedef typename InputImageType::ConstPointer    InputImageConstPointer;
  typedef typename InputImageType::RegionType      InputImageRegionType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  typedef typename itk::IOCommon::ValidCoordinateOrientationFlags
  CoordinateOrientationCode;
  /** Axes permuter type. */
  typedef typename itk::PermuteAxesImageFilter< TInputImage > PermuterType;
  typedef typename PermuterType::PermuteOrderArrayType PermuteOrderArrayType;

  /** Axes flipper type. */
  typedef typename itk::FlipImageFilter< TInputImage > FlipperType;
  typedef typename FlipperType::FlipAxesArrayType  FlipAxesArrayType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(OrienterImageFilter, ImageToImageFilter);

  /** Set/Get the orienttion codes to define the coordinate transform. */
  itkGetMacro(GivenCoordinateOrientation, CoordinateOrientationCode);
  void SetGivenCoordinateOrientation(CoordinateOrientationCode newCode);
  itkGetMacro(DesiredCoordinateOrientation, CoordinateOrientationCode);
  void SetDesiredCoordinateOrientation(CoordinateOrientationCode newCode);

  /** Get axes permute order. */
  itkGetConstReferenceMacro( PermuteOrder, PermuteOrderArrayType );

  /** Get flip axes. */
  itkGetConstReferenceMacro( FlipAxes, FlipAxesArrayType );

  /** OrienterImageFilter produces an image which is a different
   * dimensionality than its input image, in general.
   *As such, OrienterImageFilter needs to provide an
   * implementation for GenerateOutputInformation() in order to inform
   * the pipeline execution model.  The original documentation of this
   * method is below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation();

protected:
  OrienterImageFilter();
  ~OrienterImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** OrienterImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ;

  /** OrienterImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));

  /*** Member functions used by GenerateData: */
  void DeterminePermutationsAndFlips(const itk::IOCommon::ValidCoordinateOrientationFlags fixed_orient, const itk::IOCommon::ValidCoordinateOrientationFlags moving_orient);
  bool NeedToPermute();
  bool NeedToFlip();


  /** Single-threaded version of GenerateData.  This filter delegates
   * to PermuteAxesImageFilter and FlipImageFilter. */
  void GenerateData();


private:
  OrienterImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  CoordinateOrientationCode     m_GivenCoordinateOrientation;
  CoordinateOrientationCode     m_DesiredCoordinateOrientation;

  PermuteOrderArrayType              m_PermuteOrder;
  FlipAxesArrayType                  m_FlipAxes;
} ; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOrienterImageFilter.txx"
#endif

#endif


