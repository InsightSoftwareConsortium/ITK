/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstantPadImageFilter.h
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
#ifndef __itkConstantPadImageFilter_h
#define __itkConstantPadImageFilter_h

#include "itkPadImageFilter.h"

namespace itk
{

/** \class ConstantPadImageFilter
 * \brief Increase the image size by padding with a constant value.
 *
 * ConstantPadImageFilter changes the output image region.  If the output
 * image region is larger than the input image region, the extra pixels are
 * filled in by a constant value.  The output image region must be specified.
 *
 * This filter is implemented as a multithreaded filter.  It provides a 
 * ThreadedGenerateData() method for its implementation.
 * 
 * \ingroup GeometricTransforms
 * */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ConstantPadImageFilter:
    public PadImageFilter<TInputImage,TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ConstantPadImageFilter         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef PadImageFilter<TInputImage,TOutputImage>  Superclass;

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
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::InputImageRegionType InputImageRegionType;

  /**
   * Typedef to describe the type of pixel.
   */
  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;
  typedef typename Superclass::InputImagePixelType InputImagePixelType;

  /**
   * Typedef to describe the output and input image index and size types.
   */
  typedef typename Superclass::OutputImageIndexType OutputImageIndexType;
  typedef typename Superclass::InputImageIndexType InputImageIndexType;
  typedef typename Superclass::OutputImageSizeType OutputImageSizeType;
  typedef typename Superclass::InputImageSizeType InputImageSizeType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ConstantPadImageFilter, PadImageFilter);

  /**
   * ImageDimension enumeration
   */
  enum { ImageDimension = TInputImage::ImageDimension };

  /**
   * Set the pad value.  Default is Zero.
   */
  void SetConstant( OutputImagePixelType constant )
    {m_Constant = constant; this->Modified();}
  
  /** 
   * Get the output image extent.
   */
  const OutputImagePixelType GetConstant() const
		{ return m_Constant; }
                 
 protected:
   ConstantPadImageFilter();
  ~ConstantPadImageFilter() {};
  ConstantPadImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * PadImageFilter can be implemented as a multithreaded filter.  Therefore,
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
  int GenerateNextRegion(int *regIndices, int *regLimit, 
			 OutputImageIndexType *indices, 
			 OutputImageSizeType *sizes, 
			 OutputImageRegionType& outputRegion);

private:
  OutputImagePixelType m_Constant;
};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstantPadImageFilter.txx"
#endif
  
#endif
