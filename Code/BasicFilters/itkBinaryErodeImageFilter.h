/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryErodeImageFilter.h
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
#ifndef __itkBinaryErodeImageFilter_h
#define __itkBinaryErodeImageFilter_h

#include "itkMorphologyImageFilter.h"

namespace itk {

/**
 * \class BinaryErodeImageFilter
 * \brief binary dilation of an image
 *
 * Erode an image using binary morphology. Gray scale images can be
 * processed as binary images by selecting a "ErodeValue".  Pixel values
 * matching the erode value are considered the "foreground" and all other
 * pixels are "background". This is useful in processing segmented images
 * where all pixels in segment #1 have value 1 and pixels in segment #2
 * have value 2, etc. A particular "segment number" can be processed.
 * ErodeValue defaults to the maximum possible value of the PixelType.
 *
 * Binary erosion will set a pixel as the "ErodeValue" if all of the
 * pixels in the image for "on" structuring element pixels have a
 * value of "ErodeValue".  Otherwise, the center pixel is set to an
 * appropriate "background" value. For lack of something better, the
 * background value used will be the minimum of the pixels that were
 * not the ErodeValue. 
 *
 * The structuring element is assumed to be composed of binary values
 * (zero or one). Only elements of the structuring element having
 * values > 0 ("on" values) are candidates for affecting the center
 * pixel.
 *
 * 
 * For the each input image pixel, 
 *   - NeighborhoodIterator gives neighbors of the pixel. 
 *   - Evaluate() member function returns either the original pixel value
 *     or the ErodeValue.
 *   - Replace the original value with the specified value
 *
 * \sa MorphologyImageFilter, BinaryFunctionErodeImageFilter, BinaryErodeImageFilter
 * \ingroup ImageEnhancement
 */

template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT BinaryErodeImageFilter : 
  public MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  /**
   * Standard Self typedef
   */
  typedef BinaryErodeImageFilter Self;

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
  itkTypeMacro(BinaryErodeImageFilter, MorphologyImageFilter);
  
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
  itkSetMacro(ErodeValue, PixelType);

  /**
   * Get the value in the image considered as "foreground". Defaults to
   * maximum value of PixelType.
   */
  itkGetMacro(ErodeValue, PixelType);

  
protected:
  BinaryErodeImageFilter();
  ~BinaryErodeImageFilter() {};
  BinaryErodeImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value
   *
   * It will return the ErodeValue if any of the image pixels in the
   * neighborhood have the ErodeValue and that pixel's corresponding
   * element in the structuring element is positive and
   */
  PixelType Evaluate(ImageKernelIteratorType imageIt, 
                     ImageKernelIteratorType imageLast, 
                     KernelIteratorType kernelIt,
                     PixelType centerValue);

private:
  PixelType m_ErodeValue;
  
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryErodeImageFilter.txx"
#endif

#endif


