/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTwoOutputExampleImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTwoOutputExampleImageFilter_h
#define __itkTwoOutputExampleImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class TwoOutputExampleImageFilter
 * \brief Example of a filter that produce two outputs. 
 *
 * TwoOutputExampleImageFilter sets image values to a user-specified "outside"
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
 * 
 * \ingroup IntensityImageFilters
 */
template <class TImage>
class ITK_EXPORT TwoOutputExampleImageFilter:public ImageToImageFilter<TImage,TImage>
{
public:
  /** Standard class typedefs. */
  typedef TwoOutputExampleImageFilter         Self;
  typedef ImageToImageFilter<TImage,TImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Typedef to describe the type of pixel. */
  typedef typename TImage::PixelType PixelType;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(TwoOutputExampleImageFilter, ImageToImageFilter);

  /** Set the "outside" pixel value. The default value 
   * NumericTraits<PixelType>::Zero. */
  itkSetMacro(OutsideValue,PixelType);
  
  /** Get the "outside" pixel value. */
  itkGetMacro(OutsideValue,PixelType);
                 
  /** The values greater than or equal to the value are set to OutsideValue. */
  void ThresholdAbove(PixelType &thresh);
  
  /** The values less than or equal to the value are set to OutsideValue. */
  void ThresholdBelow(PixelType &thresh);

  /** The values outside the range are set to OutsideValue. */
  void ThresholdOutside(PixelType &lower, PixelType &upper);

  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;
  
  /** Some typedefs to handle the second output. */
  typedef TImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType OutputImagePixelType;
  
  /** Get the image output of this process object.  */
  OutputImagePointer GetInverseOutput();

  /** Set the image output of this process object.  */
  void SetInverseOutput(OutputImageType *output)
  { this->SetNthOutput(1, output); };

protected:
  TwoOutputExampleImageFilter();
  ~TwoOutputExampleImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** TwoOutputExampleImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  TwoOutputExampleImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  PixelType m_OutsideValue;
  PixelType m_Lower;
  PixelType m_Upper;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTwoOutputExampleImageFilter.txx"
#endif
  
#endif
