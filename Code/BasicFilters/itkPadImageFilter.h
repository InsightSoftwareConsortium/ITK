/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPadImageFilter.h
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
#ifndef __itkPadImageFilter_h
#define __itkPadImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class PadImageFilter
 * \brief Increase the image size by padding. Superclass for filters that fill
 * in extra pixels.
 *
 * PadImageFilter changes the image extent of an image.  If the image extent
 * is larger than the input image extent, the extra pixels are filled in by 
 * an algorithm determined by the subclass.  The image extent of the output 
 * must be specified.
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 * 
 * \ingroup GeometricTransforms
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT PadImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef PadImageFilter         Self;

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
   * Typedef to describe the output and input image region types.
   */
  typedef typename TOutputImage::RegionType OutputImageRegionType;
  typedef typename TInputImage::RegionType InputImageRegionType;

  /**
   * Typedef to describe the output and input image index and size types.
   */
  typedef typename TOutputImage::IndexType OutputImageIndexType;
  typedef typename TInputImage::IndexType InputImageIndexType;
  typedef typename TOutputImage::SizeType OutputImageSizeType;
  typedef typename TInputImage::SizeType InputImageSizeType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(PadImageFilter, ImageToImageFilter);

  /**
   * ImageDimension enumeration
   */
  enum { ImageDimension = TInputImage::ImageDimension };

  /**
   * Set the output image extent.  Default is the input image extent.
   */
  void PadLowerBound( unsigned int dimension, unsigned int bound );
  void PadUpperBound( unsigned int dimension, unsigned int bound );
  void PadLowerBound( unsigned int bounds[] );
  void PadUpperBound( unsigned int bounds[] );
  
  /** 
   * Get the output image extent.
   */
  const unsigned int * GetPadLowerBound() const
		{ return m_PadLowerBound; }
  const unsigned int GetPadLowerBound(unsigned int dimension) const;
  const unsigned int * GetPadUpperBound() const
		{ return m_PadUpperBound; }
  const unsigned int GetPadUpperBound(unsigned int dimension) const;
                 
  /** 
   * PadImageFilter produces an image which is a different resolution
   * than its input image.  As such, PadImageFilter needs to
   * provide an implementation for UpdateOutputInformation() in order
   * to inform the pipeline execution model.  The original
   * documentation of this method is below.
   *
   * \sa ProcessObject::UpdateOutputInformaton() 
   */
  virtual void UpdateOutputInformation();

  /** 
   * PadImageFilter needs a smaller input requested region than
   * output requested region.  As such, PadImageFilter needs to
   * provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion() 
   */
  virtual void GenerateInputRequestedRegion();

 protected:
  PadImageFilter();
  ~PadImageFilter() {};
  PadImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  unsigned int m_PadLowerBound[ImageDimension];
  unsigned int m_PadUpperBound[ImageDimension];
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPadImageFilter.txx"
#endif
  
#endif
