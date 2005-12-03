/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGetAverageSliceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGetAverageSliceImageFilter_txx
#define _itkGetAverageSliceImageFilter_txx

#include "itkGetAverageSliceImageFilter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage >
GetAverageSliceImageFilter<TInputImage,TOutputImage >
::GetAverageSliceImageFilter()
{
  m_AveragedOutDimension = this->GetAccumulateDimension();
  this->AverageOn();
}

template <class TInputImage, class TOutputImage >
void
GetAverageSliceImageFilter<TInputImage,TOutputImage>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "AveragedOutDimension: " << m_AveragedOutDimension << std::endl;
}

} // end namespace itk


#endif
