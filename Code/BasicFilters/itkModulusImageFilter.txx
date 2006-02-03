/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkModulusImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkModulusImageFilter_txx
#define _itkModulusImageFilter_txx

#include "itkModulusImageFilter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ModulusImageFilter<TInputImage, TOutputImage>
::ModulusImageFilter()
{
  m_Dividend = 5;
}

template <class TInputImage, class TOutputImage>
void
ModulusImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  this->GetFunctor().SetDividend(m_Dividend);
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ModulusImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Dividend: "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_Dividend)
     << std::endl;
}


} // end namespace itk

#endif
