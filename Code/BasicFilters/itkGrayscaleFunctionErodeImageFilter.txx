/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGrayscaleFunctionErodeImageFilter.txx
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
#ifndef __itkGrayscaleFunctionErodeImage_txx
#define __itkGrayscaleFunctionErodeImage_txx

#include "itkGrayscaleFunctionErodeImageFilter.h"

namespace itk {

template<class TInputImage, class TOutputImage, class TKernel>
GrayscaleFunctionErodeImageFilter<TInputImage, TOutputImage, TKernel>::PixelType
GrayscaleFunctionErodeImageFilter<TInputImage, TOutputImage, TKernel>
::Evaluate(const SmartNeighborhoodIteratorType &nit,
           const KernelType &kernel)
{
  PixelType min = NumericTraits<PixelType>::max();
  PixelType temp = min ;
  
  typename SmartNeighborhoodIteratorType::ConstIterator neigh_it;
  KernelIteratorType kernel_it;
  const KernelIteratorType kernelEnd = kernel.End();

  neigh_it = nit.Begin();
  for (kernel_it=kernel.Begin(); kernel_it<kernelEnd; ++kernel_it, ++neigh_it)
    {
    // if structuring element is positive, use the pixel under that element
    // in the image minus the structuring element value
    if (*kernel_it > 0)
      {
      // subtract the structuring element value to the pixel value
      temp = *neigh_it - (PixelType) *kernel_it;

      if (temp < min)
        min = temp ;
      }
    }
  
  return min; 
} 

}// end namespace itk
#endif
