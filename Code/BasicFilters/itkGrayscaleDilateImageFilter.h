/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleDilateImageFilter.h
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
#ifndef __itkGrayscaleDilateImageFilter_h
#define __itkGrayscaleDilateImageFilter_h

#include "itkMorphologyImageFilter.h"

namespace itk {

/**
 * \class GrayscaleDilateImageFilter
 * \brief gray scale dilation of an image
 *
 * Dilate an image using grayscale morphology. Dilation takes the
 * maximum of all the pixels identified by the structuring element.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 * 
 * For the each input image pixel, 
 *   - NeighborhoodIterator gives neighbors of the pixel. 
 *   - Evaluate() member function returns the maximum value among 
 *     the image neighbors where the kernel has elements > 0.
 *   - Replace the original value with the max value
 *
 * \sa MorphologyImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement
 */

template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT GrayscaleDilateImageFilter : 
  public MorphologyImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  /**
   * Standard Self typedef
   */
  typedef GrayscaleDilateImageFilter Self;

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
  itkTypeMacro(GrayscaleDilateImageFilter, 
               MorphologyImageFilter);
  
  /**
   * Standard New method
   */
  itkNewMacro(Self);  

  /**
   * Declaration of Pixel Type
   */
  typedef typename Superclass::PixelType PixelType;

  /**
   * Kernel (structuring element) iterator
   */
  typedef typename Superclass::KernelIteratorType  KernelIteratorType;

protected:

  /**
   * Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value
   *
   * It will return the maximum value of the image pixels whose corresponding
   * element in the structuring element is positive.
   */
  PixelType Evaluate(const SmartNeighborhoodIteratorType &nit,
                     const KernelType &kernel);
  
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGrayscaleDilateImageFilter.txx"
#endif

#endif


