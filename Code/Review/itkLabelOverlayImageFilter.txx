/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelOverlayImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#
#ifndef _itkLabelOverlayImageFilter_txx
#define _itkLabelOverlayImageFilter_txx

#include "itkLabelOverlayImageFilter.h"

namespace itk
{

/**
Constructor method
 */
template <class TInputImage, class TLabelImage, class TOutputImage>
LabelOverlayImageFilter<TInputImage, TLabelImage, TOutputImage>
::LabelOverlayImageFilter()
{
  m_Opacity = 1.0;
}

/**
Destructor method
*/
template <class TInputImage, class TLabelImage, class TOutputImage>
void
LabelOverlayImageFilter<TInputImage, TLabelImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  this->GetFunctor().SetOpacity(m_Opacity);
}

/**
 * Standard PrintSelf method 
 */
template <class TInputImage, class TLabelImage, class TOutputImage>
void 
LabelOverlayImageFilter<TInputImage, TLabelImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Opacity: "
     << static_cast<typename NumericTraits<double>::PrintType>(m_Opacity)
     << std::endl;
}


} // end namespace itk

#endif
