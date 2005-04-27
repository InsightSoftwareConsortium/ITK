/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPadImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPadImageFilter_h
#define __itkPadImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class PadImageFilter
 * \brief Increase the image size by padding. Superclass for filters that fill
 * in extra pixels.
 *
 * PadImageFilter changes the image boundary of an image by padding each 
 * dimension with subclass defined algorithms.  The number of pixels to pad
 * for the upper and lower bounds of each dimension must be specified.
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 * 
 * \ingroup GeometricTransforms
 * \sa WrapPadImageFilter, MirrorPadImageFilter, ConstantPadImageFilter
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT PadImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PadImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Typedef to describe the output and input image region types. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;
  typedef typename TInputImage::RegionType InputImageRegionType;

  /** Typedef to describe the type of pixel. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;
  typedef typename TInputImage::PixelType InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename TOutputImage::IndexType OutputImageIndexType;
  typedef typename TInputImage::IndexType InputImageIndexType;
  typedef typename TOutputImage::SizeType OutputImageSizeType;
  typedef typename TInputImage::SizeType InputImageSizeType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PadImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension );

  /** Set/Get the output image padding.  Default is no padding 
   *  (same as input). */
  itkSetVectorMacro(PadLowerBound, const unsigned long, ImageDimension);
  itkSetVectorMacro(PadUpperBound, const unsigned long, ImageDimension);
  itkGetVectorMacro(PadLowerBound, const unsigned long, ImageDimension);
  itkGetVectorMacro(PadUpperBound, const unsigned long, ImageDimension);
                 
  /** PadImageFilter produces an image which is a different resolution
   * than its input image.  As such, PadImageFilter needs to
   * provide an implementation for GenerateOutputInformation() in order
   * to inform the pipeline execution model.  The original
   * documentation of this method is below.
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void GenerateOutputInformation();

  /** PadImageFilter needs a smaller input requested region than
   * output requested region.  As such, PadImageFilter needs to
   * provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion()  */
  virtual void GenerateInputRequestedRegion();

protected:
  PadImageFilter();
  ~PadImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  PadImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned long m_PadLowerBound[ImageDimension];
  unsigned long m_PadUpperBound[ImageDimension];
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPadImageFilter.txx"
#endif
  
#endif
