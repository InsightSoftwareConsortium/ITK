/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInvertIntensityImageFilter.txx
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
#ifndef _itkInvertIntensityImageFilter_txx
#define _itkInvertIntensityImageFilter_txx

#include "itkInvertIntensityImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
InvertIntensityImageFilter<TInputImage, TOutputImage>
::InvertIntensityImageFilter()
{
  m_Maximum = NumericTraits<InputPixelType>::max();
}

template <class TInputImage, class TOutputImage>
void
InvertIntensityImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  this->GetFunctor().SetMaximum(m_Maximum);
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
InvertIntensityImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Maximum: "
     << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_Maximum)
     << std::endl;
}


} // end namespace itk

#endif
