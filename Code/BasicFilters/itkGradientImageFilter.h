/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientImageFilter.h
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
#ifndef __itkGradientImageFilter_h
#define __itkGradientImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkCovariantVector.h"

namespace itk
{
/** \class GradientImageFilter
 * \brief Computes the gradient of an image using directional derivatives.
 *
 * Computes the gradient of an image using directional derivatives.
 * The directional derivative at each pixel location is computed by
 * convolution with a first-order derivative operator.
 *
 * The second template parameter defines the value type used in the
 * derivative operator (defaults to float).  The third template
 * parameter defines the value type used for output image (defaults to
 * float).  The output image is defined as a covariant vector image
 * whose value type is specified as this third template parameter.
 *
 * \todo Take into account the Spacing of the pixels
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * 
 * \ingroup GradientFilters 
 */
template <class TInputImage, class TOperatorValueType=float, class TOutputValueType=float>
class ITK_EXPORT GradientImageFilter :
    public ImageToImageFilter< TInputImage,
                               Image<CovariantVector<TOutputValueType, TInputImage::ImageDimension>,  TInputImage::ImageDimension> >
{
public:
  /** Extract dimension from input image. */
  enum {InputImageDimension = TInputImage::ImageDimension};
  enum {OutputImageDimension = TInputImage::ImageDimension};

  /** Convenient typedefs for simplifying declarations. */
  typedef Image<CovariantVector<TOutputValueType, OutputImageDimension>,  OutputImageDimension> OutputImageType;

  /** Standard class typedefs. */
  typedef GradientImageFilter Self;
  typedef ImageToImageFilter< InputImageType, OutputImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientImageFilter, ImageToImageFilter);
  
  /** Image typedef support. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::PixelType InputPixelType;
  typedef TOperatorValueType OperatorValueType;
  typedef TOutputValueType OutputValueType;
  typedef CovariantVector<OutputValueType, OutputImageDimension> OutputPixelType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  
  /** GradientImageFilter needs a larger input requested region than
   * the output requested region.  As such, GradientImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

protected:
  GradientImageFilter() {}
  virtual ~GradientImageFilter() {}

  /** GradientImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  GradientImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientImageFilter.txx"
#endif

#endif
