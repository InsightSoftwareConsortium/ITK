/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShiftScaleImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShiftScaleImageFilter_h
#define __itkShiftScaleImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkArray.h"

namespace itk
{

/** \class ShiftScaleImageFilter
 * \brief Shift and scale the pixels in an image.
 *
 * ShiftScaleImageFilter shifts the input pixel by Shift (default 0.0)
 * and then scales the pixel by Scale (default 1.0). All computattions
 * are performed in the precison of the input pixel's RealType. Before
 * assigning the computed value to the output pixel, the value is clamped
 * at the NonpositiveMin and max of the pixel type.
 * \ingroup IntensityImageFilters
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ShiftScaleImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ShiftScaleImageFilter         Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Typedef to describe the output and input image region types. */
  typedef typename TInputImage::RegionType InputImageRegionType;
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Typedef to describe the pointer to the input/output. */  
  typedef typename TInputImage::Pointer InputImagePointer;
  typedef typename TOutputImage::Pointer OutputImagePointer;

  /** Typedef to describe the type of pixel. */
  typedef typename TInputImage::PixelType InputImagePixelType;
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename TInputImage::IndexType InputImageIndexType;
  typedef typename TInputImage::SizeType InputImageSizeType;
  typedef typename TInputImage::OffsetType InputImageOffsetType;
  typedef typename TOutputImage::IndexType OutputImageIndexType;
  typedef typename TOutputImage::SizeType OutputImageSizeType;
  typedef typename TOutputImage::OffsetType OutputImageOffsetType;

  /** Type to use form computations. */
  typedef typename NumericTraits<OutputImagePixelType>::RealType RealType;
      
  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension ) ;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShiftScaleImageFilter, ImageToImageFilter);

  /** Set/Get the amount to Shift each Pixel. The shift is followed by a Scale. */
  itkSetMacro(Shift,RealType);
  itkGetMacro(Shift,RealType);

  /** Set/Get the amount to Scale each Pixel. The Scale is applied after the Shift. */
  itkSetMacro(Scale,RealType);
  itkGetMacro(Scale,RealType);

  /** Get the number of pixels that underflowed and overflowed. */
  itkGetMacro(UnderflowCount,long);
  itkGetMacro(OverflowCount,long);


protected:
  ShiftScaleImageFilter();
  ~ShiftScaleImageFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData ();
  
  /** Tally accumulated in threads. */
  void AfterThreadedGenerateData ();
  
  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData (const OutputImageRegionType& 
                              outputRegionForThread,
                              int threadId) ;
private:
  ShiftScaleImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  RealType m_Shift;
  RealType m_Scale;

  long m_UnderflowCount;
  long m_OverflowCount;
  Array<long> m_ThreadUnderflow;
  Array<long> m_ThreadOverflow;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShiftScaleImageFilter.txx"
#endif
  
#endif
