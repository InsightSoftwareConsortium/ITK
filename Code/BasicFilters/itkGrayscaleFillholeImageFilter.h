/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleFillholeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFillholeImageFilter_h
#define __itkFillholeImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk {

/** \class GrayscaleFillholeImageFilter
 * \brief Remove local minima not connected to the boundary of the image.
 *
 * GrayscaleFillholeImageFilter fills holes in a grayscale image.
 * Holes are local minima in the grayscale topography that are not
 * connected to boundaries of the image. Gray level values adjacent to
 * a hole are extrapolated across the hole.
 *
 * This filter is used to smooth over local minima without affecting
 * the values of local maxima.  If you take the difference between the
 * output of this filter and the original image (and perhaps threshold
 * the difference above a small value), you'll obtain a map of the
 * local minima.
 *
 * This filter uses the GrayscaleGeodesicErodeImageFilter.  It
 * provides its own input as the "mask" input to the geodesic
 * erosion.  The "marker" image for the geodesic erosion is
 * constructed such that boundary pixels match the boundary pixels of
 * the input image and the interior pixels are set to the maximum
 * pixel value in the input image.
 *
 * Geodesic morphology and the Fillhole algorithm is described in
 * Chapter 6 of Pierre Soille's book "Morphological Image Analysis:
 * Principles and Applications", Second Edition, Springer, 2003.
 *
 * \sa GrayscaleGeodesicErodeImageFilter
 * \sa MorphologyImageFilter, GrayscaleErodeImageFilter, GrayscaleFunctionErodeImageFilter, BinaryErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT GrayscaleFillholeImageFilter : 
  public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef GrayscaleFillholeImageFilter Self;
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
  
  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(GrayscaleFillholeImageFilter, 
               ImageToImageFilter);
  
  /** Get the number of iterations used to produce the current
   * output. */
  itkGetMacro(NumberOfIterationsUsed, unsigned long);

protected:
  GrayscaleFillholeImageFilter();
  ~GrayscaleFillholeImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** GrayscaleFillholeImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ;

  /** GrayscaleFillholeImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));
  
  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void GenerateData();
  

private:
  GrayscaleFillholeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned long m_NumberOfIterationsUsed;
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGrayscaleFillholeImageFilter.txx"
#endif

#endif


