/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnsharpMaskLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

     =========================================================================*/
#ifndef _itkUnsharpMaskLevelSetImageFilter_txx_
#define _itkUnsharpMaskLevelSetImageFilter_txx_

#include "itkUnsharpMaskLevelSetImageFilter.h"

namespace itk {

template <class TInputImage, class TOutputImage>
UnsharpMaskLevelSetImageFilter <TInputImage, TOutputImage>
::UnsharpMaskLevelSetImageFilter()
{
  RadiusType radius;
  for (unsigned int j=0; j<ImageDimension;j++)
    {
    radius[j] = 1;
    }
  
  m_Function=FunctionType::New();
  this->SetLevelSetFunction(m_Function);
  this->SetNumberOfLayers(this->GetMinimumNumberOfLayers());
  
  this->SetNormalProcessType (0);  // isotropic diffusion
  this->SetNormalProcessUnsharpFlag(true); // unsharp masking on
  this->SetNormalProcessUnsharpWeight (1);
  this->SetMaxNormalIteration(25);
  this->SetMaxRefitIteration(100);
  m_MaxFilterIteration = 99;
  m_Function->Initialize(radius);
}

template <class TInputImage, class TOutputImage>
void
UnsharpMaskLevelSetImageFilter <TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);     
  os << indent << "MaxFilterIteration: " << m_MaxFilterIteration << std::endl;      
}

} // end namespace itk

#endif
