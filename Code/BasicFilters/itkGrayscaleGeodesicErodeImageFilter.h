/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleGeodesicErodeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGrayscaleGeodesicErodeImageFilter_h
#define __itkGrayscaleGeodesicErodeImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk {

/** \class GrayscaleGeodesicErodeImageFilter
 * \brief geodesic gray scale erosion of an image
 *
 * Geodesic erosion operates on a "marker" image and a "mask"
 * image. The marker image is eroded using an elementary structuring
 * element (neighborhood of radius one using only the face connected
 * neighbors). The resulting image is then compared with the mask
 * image. The output image is the pixelwise maximum of the eroded
 * marker image and the mask image.
 *
 * Geodesic erosion is run either one iteration or until
 * convergence. In the convergence case, the filter is equivalent to
 * "reconstruction by erosion". This filter is implemented to handle
 * both scenarios.  The one iteration case is multi-threaded.  The
 * convergence case is delegated to another instance of the same
 * filter (but configured to run a single iteration).
 *
 * The marker image must be greater than or equal to the mask image
 * (on a pixel by pixel basis).
 *
 * Geodesic morphology is described in Chapter 6 of Pierre Soille's
 * book "Morphological Image Analysis: Principles and Applications",
 * Second Edition, Springer, 2003.
 *
 * \sa MorphologyImageFilter, GrayscaleErodeImageFilter, GrayscaleFunctionErodeImageFilter, BinaryErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT GrayscaleGeodesicErodeImageFilter : 
  public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef GrayscaleGeodesicErodeImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage>
    Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage MarkerImageType;
  typedef TInputImage MaskImageType;
  typedef TOutputImage OutputImageType;
  typedef typename MarkerImageType::Pointer        MarkerImagePointer;
  typedef typename MarkerImageType::ConstPointer   MarkerImageConstPointer;
  typedef typename MarkerImageType::RegionType     MarkerImageRegionType;
  typedef typename MarkerImageType::PixelType      MarkerImagePixelType;
  typedef typename MaskImageType::Pointer          MaskImagePointer;
  typedef typename MaskImageType::ConstPointer     MaskImageConstPointer;
  typedef typename MaskImageType::RegionType       MaskImageRegionType;
  typedef typename MaskImageType::PixelType        MaskImagePixelType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::ConstPointer   OutputImageConstPointer;
  typedef typename OutputImageType::RegionType     OutputImageRegionType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  
  /** ImageDimension constants */
  itkStaticConstMacro(MarkerImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(MaskImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(GrayscaleGeodesicErodeImageFilter, 
               ImageToImageFilter);
  
  /** Set/Get the marker image. The marker image must be pixelwise
   * greater than or equal to the mask image. The marker image the
   * image that is eroded by this filter. */
  void SetMarkerImage(const MarkerImageType *);
  const MarkerImageType* GetMarkerImage();

  /** Set/Get the mask image. The mask image is used to "mask" the
   * eroded marker image. The mask operation is a pixelwise
   * maximum. */
  void SetMaskImage(const MaskImageType *);
  const MaskImageType* GetMaskImage();

  /** Set/Get whether the filter should run one iteration or until
   * convergence. When run to convergence, this filter is equivalent
   * to "reconstruction by erosion". Default is off. */
  itkSetMacro(RunOneIteration, bool);
  itkGetMacro(RunOneIteration, bool);
  itkBooleanMacro(RunOneIteration);

  /** Get the number of iterations used to produce the current
   * output. */
  itkGetMacro(NumberOfIterationsUsed, unsigned long);

protected:
  GrayscaleGeodesicErodeImageFilter();
  ~GrayscaleGeodesicErodeImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** GrayscaleGeodesicErodeImageFilter needs to request enough of the
   * marker image to account for the elementary structuring element.
   * The mask image does not need to be padded. Depending on whether
   * the filter is configured to run a single iteration or until
   * convergence, this method may request all of the marker and mask
   * image be provided. */
  void GenerateInputRequestedRegion();

  /** This filter will enlarge the output requested region to produce
   * all of the output if the filter is configured to run to
   * convergence.
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));

  /** Single-threaded version of GenerateData.  This version is used
   * when the filter is configured to run to convergence. This method
   * may delegate to the multithreaded version if the filter is
   * configured to run a single iteration.  Otherwise, it will
   * delegate to a separate instance to run each iteration until the
   * filter converges. */
  void GenerateData();
  
  /** Multi-thread version GenerateData. This version is used when the
   * filter is configured to run a single iteration. When the filter
   * is configured to run to convergence, the GenerateData() method is
   * called. */
  void ThreadedGenerateData (const OutputImageRegionType& 
                             outputRegionForThread,
                             int threadId) ;

private:
  GrayscaleGeodesicErodeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  bool m_RunOneIteration;
  unsigned long m_NumberOfIterationsUsed;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGrayscaleGeodesicErodeImageFilter.txx"
#endif

#endif


