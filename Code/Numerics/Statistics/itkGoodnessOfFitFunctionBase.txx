/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGoodnessOfFitFunctionBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGoodnessOfFitFunctionBase_txx
#define __itkGoodnessOfFitFunctionBase_txx

#include "itkGoodnessOfFitFunctionBase.h"

namespace itk{ 
namespace Statistics{

template< class TInputHistogram >
GoodnessOfFitFunctionBase< TInputHistogram >
::GoodnessOfFitFunctionBase()
{
  m_Epsilon = 1e-20 ;
  m_LogEpsilon = log(m_Epsilon) ;

  m_UseExpectedHistogram = true ;
  m_ObservedHistogram = 0 ;
  m_ExpectedHistogram = 0 ;

  m_UseExpectedHistogram = false ;
  m_TotalObservedScale = 0 ;
}

template< class TInputHistogram >
void
GoodnessOfFitFunctionBase< TInputHistogram >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent) ;
  
  os << indent << "Observed Histogram " << m_ObservedHistogram << std::endl ;
  os << indent << "Expected Histogram " << m_ExpectedHistogram << std::endl ;
  os << indent << "Output " << m_Output << std::endl ;
  os << indent << "Total Observed Scale   " << m_TotalObservedScale 
     << std::endl ;
  os << indent << "Use Expected Histogram " << m_UseExpectedHistogram 
     << std::endl ;
  os << indent << "Epsilon                " << m_Epsilon << std::endl ;
  os << indent << "Log(Epsilon)           " <<  m_LogEpsilon << std::endl ;
}

template< class TInputHistogram >
void
GoodnessOfFitFunctionBase< TInputHistogram >
::SetObservedHistogram(InputHistogramType* histogram)
{
  m_ObservedHistogram = histogram ;
  this->Modified() ;
}

template< class TInputHistogram >
void
GoodnessOfFitFunctionBase< TInputHistogram >
::SetExpectedHistogram(InputHistogramType* histogram)
{
  m_ExpectedHistogram = histogram ;
  this->Modified() ;
}

} // end of namespace Statistics 
} // end of namespace itk

#endif

