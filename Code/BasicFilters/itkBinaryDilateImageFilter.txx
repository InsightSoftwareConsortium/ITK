/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryDilateImageFilter.txx
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
#ifndef __itkBinaryDilateImageFilter_txx
#define __itkBinaryDilateImageFilter_txx

#include "itkBinaryDilateImageFilter.h"

namespace itk {

template<class TInputImage, class TOutputImage, class TKernel>
BinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>
::BinaryDilateImageFilter()
{
  m_DilateValue = NumericTraits<PixelType>::max();
}

template<class TInputImage, class TOutputImage, class TKernel>
BinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>::PixelType
BinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>
::Evaluate(const SmartNeighborhoodIteratorType &nit,
           const KernelType &kernel)
{
  SmartNeighborhoodIteratorType::ConstIterator neigh_it;
  KernelIteratorType kernel_it;
  const KernelIteratorType kernelEnd = kernel.End();

  neigh_it = nit.Begin();
  for (kernel_it=kernel.Begin(); kernel_it<kernelEnd; ++kernel_it, ++neigh_it)
    {
    // if structuring element is positive, use the pixel under that element
    // in the image
    if (*kernel_it > 0)
      {
      // if the pixel is the DilateValue, then we can exit early
      if (*neigh_it == m_DilateValue)
        {
        return m_DilateValue;
        }
      }
    }

  // if we got here, we never saw a pixel that had the DilateValue in
  // the structuring element, return the centerValue which is the most
  // appopriate "background" value for center pixel
  return nit.GetCenterPixel();
} 

template<class TInputImage, class TOutputImage, class TKernel>
BinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>::RegionType
BinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Dilate value: " << m_DilateValue
     << std::endl;
}

}// end namespace itk
#endif
