/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryDilateImageFilter.h
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
#ifndef __itkBinaryDilateImageFilter_h
#define __itkBinaryDilateImageFilter_h

#include "itkMorphologyImageFilter.h"

namespace itk {

/**
 * \class BinaryDilateImageFilter
 * \brief binary dilation of an image
 *
 * Dilate an image using binary morphology. Gray scale images can be
 * processed as binary images by selecting a "DilateValue".  Pixel values
 * matching the dilate value are considered the "foreground" and all other
 * pixels are "background". This is useful in processing segmented images
 * where all pixels in segment #1 have value 1 and pixels in segment #2
 * have value 2, etc. A particular "segment number" can be processed.
 * DilateValue defaults to the maximum possible value of the PixelType.
 *
 * Binary dilation will set a pixel as the "DilateValue" if any of the
 * pixels in the image under the structuring element have a value of
 * "DilateValue" and that structuring element value is greater than 0.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * If none of the pixels under the structuring element have
 * DilateValue, the pixel under the center pixel value of the
 * structuring element is unchanged. If the center pixel is "in" the
 * structuring element (value > 0), then leaving the pixel unchanged
 * is the right things to do since that is most appropriate
 * "background" value for the pixel.  If the center pixel is not part
 * of the structuring element (a rare designation), then leaving the
 * pixel unchanged is not correct in the strict morphological
 * definition (operating on an image with multiple background values
 * is not defined in morphology).  Under these conditions, the center
 * pixel should be set to "a" background value.  However, we do not
 * know which background value to set it to.
 * 
 * For the each input image pixel, 
 *   - NeighborhoodIterator gives neighbors of the pixel. 
 *   - Evaluate() member function returns either the original pixel value
 *     or the DilateValue.
 *   - Replace the original value with the specified value
 *
 * \sa MorphologyImageFilter, BinaryFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement
 */

template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT BinaryDilateImageFilter : 
  public MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  /**
   * Standard Self typedef
   */
  typedef BinaryDilateImageFilter Self;

  /**
   * Standard Superclass typedef
   */
  typedef MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
    Superclass;

  /**
   * Standard smart pointer support
   */ 
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Runtime information support
   */
  itkTypeMacro(BinaryDilateImageFilter, MorphologyImageFilter);
  
  /**
   * Standard New method
   */
  itkNewMacro(Self);  

  /**
   * Declaration of Pixel Type
   */
  typedef typename Superclass::PixelType PixelType;

  /**
   * Declaration of ImageKernelIteratorType
   */
  typedef typename Superclass::ImageKernelIteratorType ImageKernelIteratorType;

  /**
   * Kernel (structuring element) iterator
   */
  typedef typename Superclass::KernelIteratorType  KernelIteratorType;
 
  /**
   * Set the value in the image to consider as "foreground". Defaults to
   * maximum value of PixelType.
   */
  itkSetMacro(DilateValue, PixelType);

  /**
   * Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType.
   */
  itkGetMacro(DilateValue, PixelType);

  
protected:
  BinaryDilateImageFilter();
  ~BinaryDilateImageFilter() {};
  BinaryDilateImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value
   *
   * It will return the DilateValue if any of the image pixels in the
   * neighborhood have the DilateValue and that pixel's corresponding
   * element in the structuring element is positive and
   */
  PixelType Evaluate(const SmartNeighborhoodIteratorType &nit,
                     const KernelType &kernel);


private:
  PixelType m_DilateValue;
  
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryDilateImageFilter.txx"
#endif

#endif


