/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkResampleImageFilter.h
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


#ifndef __itkResampleImageFilter_h
#define __itkResampleImageFilter_h

#include "itkAffineTransform.h"
#include "itkImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkSize.h"

namespace itk
{

/** \class ResampleImageFilter
 * \brief Resample an image via a coordinate transform
 *
 * ResampleImageFilter resamples an existing image through some coordinate
 * transform, interpolating via some image function.  (In the current
 * version, the transform is always affine and the interpolation is
 * always linear, but this can be expected to change in the future.)
 *
 * Since this filter produces an image which is a different size than
 * its input, it needs to override several of the methods defined
 * in ProcessObject in order to properly manage the pipeline execution model.
 * In particular, this filter overrides
 * ProcessObject::GenerateInputRequestedRegion() and
 * ProcessObject::UpdateOutputInformation().
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ResampleImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ResampleImageFilter         Self;

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
  enum {NDimensions = TOutputImage::ImageDimension};

  /**
   * Transform typedef
   *
   * FIXME: Generalize this to any sort of coordinate transformation.
   * FIXME: Check that input and output images have the same number of 
   * dimensions; this is required by the current implementation of 
   * AffineTransform.
   */
  typedef AffineTransform<double, TInputImage::ImageDimension> TransformType;
  typedef AffineTransform<double, TInputImage::ImageDimension> 
      *TransformPointerType;

  /**
   * Interpolator typedef
   *
   * FIXME: Generalize to any sort of image function.
   */
  typedef LinearInterpolateImageFunction<TInputImage>   InterpolatorType;
  typedef InterpolatorType::Pointer  InterpolatorPointerType;

  /**
   * Image size typedef
   */
  typedef Size<TOutputImage::ImageDimension>  SizeType;

  /**
   * Image index typedef
   */
  typedef typename TOutputImage::IndexType IndexType;

  /**
   * Image point typedef
   */
  typedef Point<double, TOutputImage::ImageDimension>    PointType;

  /**
   * Image pixel value typedef
   */
  typedef typename TOutputImage::PixelType   PixelType;

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
  itkTypeMacro(ResampleImageFilter, ImageToImageFilter);

  /**
   * Set the coordinate transformation
   *
   * Set the coordinate transform to use for resampling.  Note that this
   * must be in index coordinates and is the output-to-input transform,
   * NOT the input-to-output transform that you might naively expect.
   */
  void SetTransform(TransformPointerType transform) {
    m_Transform = transform;
  }

  /**
   * Set the interpolator function
   */
  void SetInterpolator(InterpolatorPointerType interpolator) {
    m_Interpolator = interpolator;
  }

  /**
   * Set the size of the output image
   */
  void SetSize(SizeType &size) {
    m_Size = size;
  }
      
  /**
   * ResampleImageFilter produces an image which is a different size than
   * its input.  As such, it needs to provide an implementation for
   * UpdateOutputInformation() in order to inform the pipeline execution model.
   * The original documentation of this method is below.
   *
   * \sa ProcessObject::UpdateOutputInformaton()
   */
  virtual void UpdateOutputInformation();

  /**
   * ResampleImageFilter needs a different input requested region than
   * the output requested region.  As such, ResampleImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

protected:

  ResampleImageFilter();
  ~ResampleImageFilter() {};
  ResampleImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  /**
   * ResampleImageFilter can be implemented as a multithreaded filter.  Therefore,
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

  SizeType m_Size;                      // Size of the output image
  TransformPointerType m_Transform;     // Coordinate transform to use
  InterpolatorPointerType m_Interpolator;
                                        // Image function for interpolation
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkResampleImageFilter.txx"
#endif
  
#endif
