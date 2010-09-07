/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaskedRankImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMaskedRankImageFilter_txx
#define __itkMaskedRankImageFilter_txx

#include "itkMaskedRankImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"
#include "itkNumericTraits.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageLinearConstIteratorWithIndex.h"

#include <iomanip>
#include <sstream>

namespace itk
{
template< class TInputImage, class TMaskImage, class TOutputImage, class TKernel >
MaskedRankImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel >
::MaskedRankImageFilter()
{
  m_Rank = 0.5;
}

template< class TInputImage, class TMaskImage, class TOutputImage, class TKernel >
typename MaskedRankImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel >::HistogramType *
MaskedRankImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel >
::NewHistogram()
{
  HistogramType *hist;

  if ( UseVectorBasedHistogram() )
    {
    hist = new VHistogram();
    }
  else
    {
    hist = new MHistogram();
    }
  hist->SetRank( this->GetRank() );
  return hist;
}

template< class TInputImage, class TMaskImage, class TOutputImage, class TKernel >
void
MaskedRankImageFilter< TInputImage, TMaskImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Rank: " << static_cast< typename NumericTraits< float >::PrintType >( m_Rank ) << std::endl;
}
} // end namespace itk
#endif
