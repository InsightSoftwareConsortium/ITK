/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKernelImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkKernelImageFilter_txx
#define __itkKernelImageFilter_txx

#include "itkKernelImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk {

template <class TInputImage, class TOutputImage, class TKernel>
KernelImageFilter<TInputImage, TOutputImage, TKernel>
::KernelImageFilter()
{
  this->SetRadius( 1 );
}


template <class TInputImage, class TOutputImage, class TKernel>
void
KernelImageFilter<TInputImage, TOutputImage, TKernel>
::SetRadius( const RadiusType & radius )
{
  // Superclass::SetRadius() is called in SetKenel() - no need to call it here
  // Superclass::SetRadius( radius );
  KernelType kernel;
  kernel.SetRadius( radius );
  for( typename KernelType::Iterator kit=kernel.Begin(); kit != kernel.End(); kit++ )
    {
    *kit = 1;
    }
  this->SetKernel( kernel );
}


template <class TInputImage, class TOutputImage, class TKernel>
void
KernelImageFilter<TInputImage, TOutputImage, TKernel>
::SetKernel( const KernelType & kernel )
{
  if( m_Kernel != kernel )
    {
    m_Kernel = kernel;
    this->Modified();
    }
  // set the radius of the super class to be the same than the kernel one
  Superclass::SetRadius( kernel.GetRadius() );
}


template <class TInputImage, class TOutputImage, class TKernel>
void
KernelImageFilter<TInputImage, TOutputImage, TKernel>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Kernel: " << m_Kernel << std::endl;
}

}


#endif
