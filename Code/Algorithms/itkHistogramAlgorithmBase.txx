/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHistogramAlgorithmBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHistogramAlgorithmBase_txx
#define __itkHistogramAlgorithmBase_txx

#include "itkHistogramAlgorithmBase.h"

namespace itk
{ 

template< class TInputHistogram >
HistogramAlgorithmBase< TInputHistogram >
::HistogramAlgorithmBase()
{
  m_InputHistogram = 0;
}

template< class TInputHistogram >
void
HistogramAlgorithmBase< TInputHistogram >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Input Histogram: " ;
  if ( m_InputHistogram.IsNotNull() )
    {
    os << m_InputHistogram << std::endl;
    }
  else
    {
    os << "not set." << std::endl ;
    }
}


} // end of namespace itk

#endif

