/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIsotropicFourthOrderLevelSetImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

     =========================================================================*/
#ifndef _itkIsotropicFourthOrderLevelSetImageFilter_txx_
#define _itkIsotropicFourthOrderLevelSetImageFilter_txx_

#include "itkIsotropicFourthOrderLevelSetImageFilter.h"

namespace itk {

template <class TInputImage, class TOutputImage>
IsotropicFourthOrderLevelSetImageFilter <TInputImage, TOutputImage>
::IsotropicFourthOrderLevelSetImageFilter()
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
  this->SetMaxNormalIteration(25);
  this->SetMaxRefitIteration(100);
  this->SetMaxFilterIteration(1000);
  m_Function->Initialize(radius);
}

template <class TInputImage, class TOutputImage>
void
IsotropicFourthOrderLevelSetImageFilter <TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);     
  os << indent << "MaxFilterIteration: " << m_MaxFilterIteration << std::endl;      
}

} // end namespace itk

#endif
