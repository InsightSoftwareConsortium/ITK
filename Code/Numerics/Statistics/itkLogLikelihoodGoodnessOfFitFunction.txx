/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLogLikelihoodGoodnessOfFitFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLogLikelihoodGoodnessOfFitFunction_txx
#define __itkLogLikelihoodGoodnessOfFitFunction_txx

#include "itkLogLikelihoodGoodnessOfFitFunction.h"

namespace itk{ 
namespace Statistics{

template< class TInputHistogram >
LogLikelihoodGoodnessOfFitFunction< TInputHistogram >
::LogLikelihoodGoodnessOfFitFunction()
{
  this->SetUseExpectedHistogram(true) ;
  m_Initialized = false ;
}

template< class TInputHistogram >
void
LogLikelihoodGoodnessOfFitFunction< TInputHistogram >
::GenerateData()
{
  const TInputHistogram* observedHistogram = this->GetObservedHistogram() ;
  const TInputHistogram* expectedHistogram = this->GetExpectedHistogram() ;

  float p, px, sum = 0.0f ;
  double ratio;
  typename TInputHistogram::ConstIterator e_iter = expectedHistogram->Begin() ;
  typename TInputHistogram::ConstIterator e_last = expectedHistogram->End() ;
  typename TInputHistogram::ConstIterator o_iter = observedHistogram->Begin() ;
  while ( e_iter != e_last )
    {
    p = e_iter.GetFrequency() ;
    px = o_iter.GetFrequency() ;
      
    ratio = px / p ;

    if ( ratio > this->GetEpsilon() && px > 0 )
      {
      sum += px * vcl_log(ratio);
      }
    else
      {
      sum += px * this->GetLogEpsilon() ;
      }
    ++e_iter ;
    ++o_iter ;
    }

  sum *= 2.0;
  this->GetOutput() = sum ;
}

} // end of namespace Statistics 
} // end of namespace itk

#endif

