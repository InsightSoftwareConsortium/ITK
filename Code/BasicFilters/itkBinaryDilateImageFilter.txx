/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryDilateImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
  m_KernelCenterPixelOn = false;
}


template<class TInputImage, class TOutputImage, class TKernel>
void
BinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>
::BeforeThreadedGenerateData()
{
  // Cache whether the center pixel in the kernel is set
  m_KernelCenterPixelOn = (this->GetKernel().GetCenterValue() > 0);
}


template<class TInputImage, class TOutputImage, class TKernel>
typename BinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>::PixelType
BinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>
::Evaluate(const NeighborhoodIteratorType &nit,
           const KernelIteratorType kernelBegin,
           const KernelIteratorType kernelEnd)
{
  // Early exit test.  Most structuring elements will have the center
  // pixel on. So test the center pixel first to see if it is already
  // the dilate value and if so, exit without iterating over the
  // kernel.
  if (m_KernelCenterPixelOn && (nit.GetCenterPixel() == m_DilateValue))
    {
    return m_DilateValue;
    }
  
  unsigned int i;
  KernelIteratorType kernel_it;

  for (i=0, kernel_it=kernelBegin; kernel_it<kernelEnd; ++kernel_it, ++i)
    {
    // if structuring element is positive, use the pixel under that element
    // in the image
    if (*kernel_it > 0)
      {
      // if the pixel is the DilateValue, then we can exit early
      // note that we use GetPixel() on the NeighborhoodInterator
      // to respect the boundary conditions
      if (nit.GetPixel(i) == m_DilateValue)
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
void
BinaryDilateImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Dilate value: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>(m_DilateValue)
     << std::endl;
}

}// end namespace itk
#endif
