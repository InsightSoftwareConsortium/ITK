/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkThresholdImageFilter_h
#define __itkThresholdImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class ThresholdImageFilter
 * \brief Set image values to a user-specified value if they are below, 
 * above, or between simple threshold values.
 *
 * ThresholdImageFilter sets image values to a user-specified "outside"
 * value (by default, "black") if the image values are below, above, or
 * between simple threshold values. The filter can produce two outputs,
 * one the inverse of the other. (GetOutput() returns an image whose
 * pixels satisfy the threshold values and are passed to the output 
 * unchanged (and those that don't are marked with the outside user-value);
 * GetInverseOutput() returns an image in which pixels satisfying the
 * threshold are marked "outside", and the other pixel values are passed
 * through.)
 *
 * The pixels must support the operators >= and <=.
 */
template <class TImage>
class ITK_EXPORT ThresholdImageFilter:public ImageToImageFilter<TImage,TImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ThresholdImageFilter         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TImage,TImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /**
   * Typedef to describe the type of pixel.
   */
  typedef typename TImage::PixelType PixelType;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ThresholdImageFilter, ImageToImageFilter);

  /** 
   * Set the "outside" pixel value. The default value 
   * NumericTraits<PixelType>::Zero;
   */
  itkSetMacro(OutsideValue,PixelType);
  
  /** 
   * Get the "outside" pixel value.
   */
  itkGetMacro(OutsideValue,PixelType);
                 
  /** 
   * Some typedefs to handle the second output.
   */
  typedef TImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType OutputImagePixelType;
  
  /** 
   * Get the image output of this process object. 
   */
  OutputImagePointer GetInverseOutput();

  /** 
   * Set the image output of this process object. 
   */
  void SetInverseOutput(OutputImageType *output);

protected:
  ThresholdImageFilter();
  ~ThresholdImageFilter() {};
  ThresholdImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  /**
   * ThresholdImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() 
   */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  PixelType m_OutsideValue;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdImageFilter.txx"
#endif
  
#endif
