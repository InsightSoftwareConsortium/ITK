/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumMaximumImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMinimumMaximumImageFilter_txx
#define _itkMinimumMaximumImageFilter_txx

#include "itkMinimumMaximumImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkStatisticsImageFilter.h"

namespace itk
{


template <class TInputImage>
void
MinimumMaximumImageFilter<TInputImage>
::GenerateData(void)
{
  StatisticsImageFilter<TInputImage>::Pointer stats = StatisticsImageFilter<TInputImage>::New();
  stats->SetInput (this->GetInput());
  stats->GraftOutput (this->GetOutput());
  stats->Update();

  m_Minimum = stats->GetMinimum();
  m_Maximum = stats->GetMaximum();

  this->GraftOutput(stats->GetOutput());
}

template <class TInputImage>
void
MinimumMaximumImageFilter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << "Minimum: " << m_Minimum << std::endl;
  os << "Maximum: " << m_Maximum << std::endl;
}
} // end namespace itk

#endif
