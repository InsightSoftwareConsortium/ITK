/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHMaximaImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHMaximaImageFilter_h
#define __itkHMaximaImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk {

/** \class HMaximaImageFilter
 * \brief Suppress local maxima whose height above the baseline is less than h.
 *
 * HMaximaImageFilter suppresses local maxima that are less than h
 * intensity units above the (local) background. This has the effect
 * of smoothing over the "high" parts of the noise in the image
 * without smoothing over large changes in intensity (region
 * boundaries). See the HMinimaImageFilter to suppress the local
 * minima whose depth is less than h intensity units below the (local)
 * background.
 *
 * If the output of HMaximaImageFilter is subtracted from the original
 * image, the signicant "peaks" in the image can be identified.  This
 * is what the HConvexImageFilter provides.
 *
 * This filter uses the GrayscaleGeodesicDilateImageFilter.  It
 * provides its own input as the "mask" input to the geodesic
 * dilation.  The "marker" image for the geodesic dilation is
 * the input image minus the height parameter h.
 *
 * Geodesic morphology and the H-Maxima algorithm is described in
 * Chapter 6 of Pierre Soille's book "Morphological Image Analysis:
 * Principles and Applications", Second Edition, Springer, 2003.
 *
 * \sa GrayscaleGeodesicDilateImageFilter, HMinimaImageFilter, HConvexImageFilter
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT HMaximaImageFilter : 
  public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef HMaximaImageFilter Self;
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
  itkTypeMacro(HMaximaImageFilter, 
               ImageToImageFilter);

  /** Set/Get the height that a local maximum must be above the local
   * background (local contrast) in order to survive the
   * processing. Local maxima below this value are replaced with an
   * estimate of the local background. */
  itkSetMacro(Height, InputImagePixelType);
  itkGetMacro(Height, InputImagePixelType);
  
  /** Get the number of iterations used to produce the current
   * output. */
  itkGetMacro(NumberOfIterationsUsed, unsigned long);

protected:
  HMaximaImageFilter();
  ~HMaximaImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** HMaximaImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ;

  /** HMaximaImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));
  
  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void GenerateData();
  

private:
  HMaximaImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  InputImagePixelType m_Height;
  unsigned long m_NumberOfIterationsUsed;
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHMaximaImageFilter.txx"
#endif

#endif


