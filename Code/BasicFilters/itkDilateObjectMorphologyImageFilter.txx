/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDilateObjectMorphologyImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDilateObjectMorphologyImageFilter_txx
#define __itkDilateObjectMorphologyImageFilter_txx

#include "itkDilateObjectMorphologyImageFilter.h"

namespace itk {

template<class TInputImage, class TOutputImage, class TKernel>
DilateObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::DilateObjectMorphologyImageFilter()
{
}


template<class TInputImage, class TOutputImage, class TKernel>
void
DilateObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::Evaluate(OutputNeighborhoodIteratorType &nit, const KernelType &kernel)
{
  unsigned int i;
  KernelIteratorType kernel_it;
  const KernelIteratorType kernelEnd = kernel.End();

  for (i=0, kernel_it=kernel.Begin(); kernel_it<kernelEnd; ++kernel_it, ++i)
    {
    if(*kernel_it>0)
      {
      nit.SetPixel(i, m_ObjectValue);
      }
    }
} 

template<class TInputImage, class TOutputImage, class TKernel>
void
DilateObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::Evaluate(OutputSmartNeighborhoodIteratorType &nit,
           const KernelType &kernel)
{
  unsigned int i;
  KernelIteratorType kernel_it;
  const KernelIteratorType kernelEnd = kernel.End();

  bool valid = true;
  for (i=0, kernel_it=kernel.Begin(); kernel_it<kernelEnd; ++kernel_it, ++i)
    {
    if(*kernel_it>0)
      {
      nit.SetPixel(i, m_ObjectValue, valid);
      }
    }
} 


template<class TInputImage, class TOutputImage, class TKernel>
void
DilateObjectMorphologyImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}// end namespace itk
#endif
