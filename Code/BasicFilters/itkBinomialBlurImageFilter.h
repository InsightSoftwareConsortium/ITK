/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinomialBlurImageFilter.h
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
#ifndef __itkBinomialBlurImageFilter_h
#define __itkBinomialBlurImageFilter_h

#include "itkImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkSize.h"

namespace itk
{

/**
 * \class BinomialBlurImageFilter
 * \brief Performs a separable blur on each dimension of an image
 *
 * The binomial blur consists of a nearest neighbor average along each
 * image dimension. The net result after n-iterations approaches
 * convultion with a gaussian.
 * 
 * \ingroup ImageEnhancement
 * \ingroup ImageFeatureExtraction 
 *
 * */

template<class TInputImage, class TOutputImage>
class ITK_EXPORT BinomialBlurImageFilter :
   public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
   
  /**
   * Standard "Self" typedef.
   */
  typedef BinomialBlurImageFilter Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Number of dimensions
   */
  enum {NDimensions = TInputImage::ImageDimension};

  /**
   * Image size typedef
   */
  typedef Size<TOutputImage::ImageDimension> SizeType;

  /**
   * Image index typedef
   */
  typedef typename TOutputImage::IndexType IndexType;

  /**
   * Image pixel value typedef
   */
  typedef typename TOutputImage::PixelType PixelType;

  /**
   * Typedef to describe the output image region type.
   */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( BinomialBlurImageFilter, ImageToImageFilter );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Get and set the number of times to repeat the filter.
   */
  itkSetMacro(Repetitions, unsigned int);
  itkGetMacro(Repetitions, unsigned int);

  /**
   * Method for evaluating the implicit function over the image.
   */
  void GenerateData();

  /**
   * This filter needs to request a larger input than its requested output.
   * If this filter runs "Repetitions" iterations, then it needs an input
   * that is 2*Repetitions larger than the output. In other words, this
   * filter needs a border of "Repetitions" pixels.
   */
  void GenerateInputRequestedRegion();

protected:

  BinomialBlurImageFilter();
  virtual ~BinomialBlurImageFilter() {};

  BinomialBlurImageFilter(const Self&) {}
  void operator=(const Self&) {}

private:

  /**
   * How many times should we apply the blur?
   */
  unsigned int m_Repetitions;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinomialBlurImageFilter.txx"
#endif

#endif
