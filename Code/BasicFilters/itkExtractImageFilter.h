/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkExtractImageFilter_h
#define __itkExtractImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSmartPointer.h"

namespace itk
{

/** \class ExtractImageFilter
 * \brief Decrease the image size by cropping the image to the selected 
 * region bounds.
 *
 * ExtractImageFilter changes the image boundary of an image by removing  
 * pixels outside the target region.  The target region must be specified.
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 * 
 * \ingroup GeometricTransforms
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ExtractImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ExtractImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExtractImageFilter, ImageToImageFilter);

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

  /** ImageDimension enumeration */
  enum { InputImageDimension = TInputImage::ImageDimension };
  enum { OutputImageDimension = TOutputImage::ImageDimension };

  /** Set/Get the output image region. */
  itkSetMacro(ExtractionRegion, OutputImageRegionType);
  itkGetMacro(ExtractionRegion, OutputImageRegionType);
                 
  /** ExtractImageFilter produces an image which is a different resolution
   * than its input image.  As such, ExtractImageFilter needs to
   * provide an implementation for GenerateOutputInformation() in order
   * to inform the pipeline execution model.  The original
   * documentation of this method is below.
   * \sa ProcessObject::GenerateOutputInformaton()  */
  virtual void GenerateOutputInformation();

protected:
  ExtractImageFilter();
  ~ExtractImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** ExtractImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  ExtractImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  OutputImageRegionType m_ExtractionRegion;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExtractImageFilter.txx"
#endif
  
#endif
