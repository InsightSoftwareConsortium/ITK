/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShrinkImageFilter.h
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
#ifndef __itkShrinkImageFilter_h
#define __itkShrinkImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class ShrinkImageFilter
 * \brief Reduce the size of an image by an integer factor in each
 * dimension.
 *
 * ShrinkImageFilter reduces the size of an image by an integer factor
 * in each dimension. The algorithm implemented is a simple subsample. 
 * The output image size in each dimension is given by:
 *
 * outputSize[j] = max( floor(inputSize[j]/shrinkFactor[j]), 1 ); 
 *
 * Since this filter produces an image which is a different resolution 
 * and with different pixel spacing than its input image, 
 * it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::UpdateOutputInformation().
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ShrinkImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ShrinkImageFilter         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;

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
   * Typedef to describe the output image region type.
   */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ShrinkImageFilter, ImageToImageFilter);

  /**
   * ImageDimension enumeration
   */
  enum { ImageDimension = TInputImage::ImageDimension };

  /**
   * Set the shrink factors. Values are clamped to 
   * a minimum value of 1. Default is 1 for all dimensions.
   */
  void SetShrinkFactors( unsigned int factors[] );
  void SetShrinkFactors( unsigned int factor );
  
  /** 
   * Get the shrink factors.
   */
  const unsigned int * GetShrinkFactors() const
		{ return m_ShrinkFactors; }
                 
  /**
   * ShrinkImageFilter produces an image which is a different resolution and
   * with a different pixel spacing than its input image.  As such,
   * ShrinkImageFilter needs to provide an implementation for
   * UpdateOutputInformation() in order to inform the pipeline execution model.
   * The original documentation of this method is below.
   *
   * \sa ProcessObject::UpdateOutputInformaton()
   */
  virtual void UpdateOutputInformation();

  /**
   * ShrinkImageFilter needs a larger input requested region than the output
   * requested region.  As such, ShrinkImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the pipeline
   * execution model.  
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()
   */
  virtual void GenerateInputRequestedRegion();

 protected:
  ShrinkImageFilter();
  ~ShrinkImageFilter() {};
  ShrinkImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  /**
   * ShrinkImageFilter can be implemented as a multithreaded filter.  Therefore,
   * this implementation provides a ThreadedGenerateData() routine which
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

private:
  unsigned int m_ShrinkFactors[ImageDimension];
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShrinkImageFilter.txx"
#endif
  
#endif
