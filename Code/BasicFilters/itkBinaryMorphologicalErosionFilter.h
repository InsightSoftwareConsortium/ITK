/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMorphologicalErosionFilter.h
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
#ifndef __itkBinaryMorphologicalErosionFilter_h
#define __itkBinaryMorphologicalErosionFilter_h

#include "itkBinaryMorphologicalFilterBase.h"

namespace itk {

  /** \class BinaryMorphologicalErosionFilter
   * \brief filter implementation of binary morphological erosion
   *
   * This filter considers the input image is composed of only two types of
   * pixel values: background and forground. each pixel is categorized using
   * the threshold value. 
   * 
   * For the each input image pixel, 
   *   - NeighborhoodIterator gives neighbors of the pixel. 
   *   - Evalue() member function retunrs the minimum value among 
   *     the results of (image neighbors * kernel element) if the min value 
   *     is less than threshold, else return center pixel value 
   *     (original value)  
   *   - Replace the original value with the min value
   *
   * \sa BinaryMorphologicalFilterBase
   * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
   */
template<class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT BinaryMorphologicalErosionFilter : 
  public BinaryMorphologicalFilterBase<TInputImage, TOutputImage, TKernel>
{
public:
  /**
   * Standard Self typedef
   */
  typedef BinaryMorphologicalErosionFilter Self;

  /**
   * Standard Superclass typedef
   */
  typedef BinaryMorphologicalFilterBase<TInputImage, TOutputImage, TKernel>
    Superclass;

  /**
   * Standard smart pointer support
   */ 
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Runtime information support
   */
  itkTypeMacro(BinaryMorphologicalErosionFilter, 
               BinaryMorphologicalFilterBase);
  
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
 

protected:

  /**
   * Evaluate image neighborhood with kernel to find the new value 
   * for the center pixel value
   *
   * It will return the minimum value among the results of 
   * (image neighbors * kernel element) if the min value is less than
   * threshold, else return center pixel value (original value) 
   */
  PixelType Evaluate(ImageKernelIteratorType first, 
                     ImageKernelIteratorType last, 
                     KernelIteratorType first2,
                     PixelType centerValue,
                     PixelType threshold) ;
} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryMorphologicalErosionFilter.txx"
#endif

#endif


