/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryErodeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryErodeImageFilter_txx
#define __itkBinaryErodeImageFilter_txx

#include "itkBinaryErodeImageFilter.h"

namespace itk {

template<class TInputImage, class TOutputImage, class TKernel>
BinaryErodeImageFilter<TInputImage, TOutputImage, TKernel>
::BinaryErodeImageFilter()
{
  m_ErodeValue = NumericTraits<PixelType>::NonpositiveMin();
}

template<class TInputImage, class TOutputImage, class TKernel>
typename BinaryErodeImageFilter<TInputImage, TOutputImage, TKernel>::PixelType
BinaryErodeImageFilter<TInputImage, TOutputImage, TKernel>
::Evaluate(const NeighborhoodIteratorType &nit,
           const KernelType &kernel)
{
  unsigned int i;
  PixelType min = NumericTraits<PixelType>::max();

  bool erode = false;               // do some sort of erosion
  bool completelyBackground = true; // structuring element is completely
                                    // over background pixels
  
  KernelIteratorType kernel_it;
  const KernelIteratorType kernelEnd = kernel.End();

  for (i=0, kernel_it=kernel.Begin(); kernel_it<kernelEnd; ++kernel_it, ++i)
    {
    // if structuring element is positive, use the pixel under that element
    // in the image
    if (*kernel_it > 0)
      {
      // if the image pixel is not the erode value, note we use GetPixel()
      // on the SmartNeighborhoodIterator in order to respect boundary
      // conditions
      if (nit.GetPixel(i) != m_ErodeValue)
        {
        erode = true;
        
        // if the image pixel is less than current min,  note we use GetPixel()
        // on the NeighborhoodIterator in order to respect boundary
        // conditions
        if (min > nit.GetPixel(i))
          {
          min = nit.GetPixel(i);
          }
        }
      else
        {
        // at least one pixel in structuring element is the foreground
        completelyBackground = false;
        }
      }
    }

  // Four cases for the return value:
  // 1) If nothing in structuring element is the ErodeValue (foreground)
  //      then leave pixel unchanged
  // 2) If all of structuring element is the ErodeValue (foreground)
  //      then return ErodeValue
  // 3) If part of the structuring elemene is over background, and the
  //       center pixel of the structuring element is "on", then
  //       return the minimum of all the background values visited
  // 4) If part of the structuring element is over background, and the
  //       center pixel of the structuring element is "off", then
  //       leave pixel unchanged
  if (completelyBackground)
    {
    // case #1
    return nit.GetCenterPixel();
    }
  else
    {
    if (!erode)
      {
      // case #2, don't erode
      return m_ErodeValue;
      }
    else
      {
      if (kernel.GetCenterValue() > 0)
        {
        // case #3, center pixel is "on"
        return min;
        }
      else
        {
        // case #4, center pixel is "off"
        return nit.GetCenterPixel();
        }
      }
    }
} 


template<class TInputImage, class TOutputImage, class TKernel>
void
BinaryErodeImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Erode value: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>(m_ErodeValue)
     << std::endl;
}

}// end namespace itk
#endif
