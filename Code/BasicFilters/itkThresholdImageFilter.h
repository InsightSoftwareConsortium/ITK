/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 * 
 * \ingroup IntensityImageFilters
 *
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
   * The values greater than or equal to the value are set to OutsideValue
   */
  void ThresholdAbove(PixelType &thresh);
  
  /**
   * The values less than or equal to the value are set to OutsideValue
   */
  void ThresholdBelow(PixelType &thresh);

  /**
   * The values outside the range are set to OutsideValue
   */
  void ThresholdOutside(PixelType &lower, PixelType &upper);

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
  void SetInverseOutput(OutputImageType *output)
    { this->SetNthOutput(1, output); };

protected:
  ThresholdImageFilter();
  ~ThresholdImageFilter() {};
  ThresholdImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

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
  PixelType m_Lower;
  PixelType m_Upper;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdImageFilter.txx"
#endif
  
#endif
