/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroCrossingImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkZeroCrossingImageFilter_h
#define __itkZeroCrossingImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
namespace itk
{  
/** \class ZeroCrossingImageFilter
 *
 *  This filter finds the closest pixel to the zero-crossings (sign changes) in
 *  a signed itk::Image.  Pixels closest to zero-crossings are labeled with
 *  a foreground value.  All other pixels are marked with a background value.
 *  The algorithm works by detecting differences in sign among neighbors using
 *  city-block style connectivity (4-neighbors in 2d, 6-neighbors in 3d, etc.).
 *  
 *  \par Inputs and Outputs
 *  The input to this filter is an itk::Image of arbitrary dimension.  The
 *  algorithm assumes a signed data type (zero-crossings are not defined for
 *  unsigned data types), and requires that operator>, operator<, operator==,
 *  and operator!= are defined.  
 *
 *  \par
 *  The output of the filter is a binary, labeled image of user-specified type.
 *  By default, zero-crossing pixels are labeled with a default ``foreground''
 *  value of itk::NumericTraits<OutputDataType>::One, where OutputDataType is
 *  the data type of the output image.  All other pixels are labeled with a
 *  default ``background'' value of itk::NumericTraits<OutputDataType>::Zero.
 *
 *  \par Parameters
 *  There are two parameters for this filter.  ForegroundValue is the value
 *  that marks zero-crossing pixels.  The BackgroundValue is the value given to 
 *  all other pixels.
 *       
 *  \sa Image
 *  \sa Neighborhood
 *  \sa NeighborhoodOperator
 *  \sa NeighborhoodIterator     
 *  \ingroup ImageFeatureExtraction */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT ZeroCrossingImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard "Self" & Superclass typedef. */
  typedef ZeroCrossingImageFilter    Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  
  /** Image typedef support   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  
  /** SmartPointer typedef support  */ 
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Define pixel types  */
  typedef typename TInputImage::PixelType   InputImagePixelType;
  typedef typename TOutputImage::PixelType  OutputImagePixelType;
  
  /** Method for creation through the object factory.  */
  itkNewMacro(Self);  
  
  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ZeroCrossingImageFilter, ImageToImageFilter);
  
  /** ImageDimension enumeration   */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension );
  
  /** ZeroCrossingImageFilter needs a larger input requested
   * region than the output requested region (larger by the kernel
   * size to do comparisons between the central pixel and ite neighbors).
   * Thus ZeroCrossingImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()   */   
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

  /** Set/Get the label value for zero-crossing pixels. */
  itkSetMacro(ForegroundValue, OutputImagePixelType);
  itkGetMacro(ForegroundValue, OutputImagePixelType);
  
  /** Set/Get the label value for non-zero-crossing pixels. */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetMacro(BackgroundValue, OutputImagePixelType);
  
protected:
  ZeroCrossingImageFilter()
  {
    m_ForegroundValue = NumericTraits<OutputImagePixelType>::One;
    m_BackgroundValue = NumericTraits<OutputImagePixelType>::Zero;
  }
  ~ZeroCrossingImageFilter(){}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  ZeroCrossingImageFilter(const Self&) {}
  OutputImagePixelType m_BackgroundValue;
  OutputImagePixelType m_ForegroundValue;
  
  /**
   * ZeroCrossingImageFilter can be implemented as a multithreaded filter.  
   * Therefore,this implementation provides a ThreadedGenerateData() routine which
   * is called for each processing thread. The output image data is allocated
   * automatically by the superclass prior to calling ThreadedGenerateData().
   * ThreadedGenerateData can only write to the portion of the output image
   * specified by the parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()
   */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );
};
  
} //end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroCrossingImageFilter.txx"
#endif
  
#endif

