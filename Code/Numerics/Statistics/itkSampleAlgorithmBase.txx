/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleAlgorithmBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSampleAlgorithmBase_txx
#define __itkSampleAlgorithmBase_txx

#include "itkSampleAlgorithmBase.h"

namespace itk{ 
namespace Statistics{

template< class TSample >
SampleAlgorithmBase< TSample >
::SampleAlgorithmBase()
{
}

template< class TSample >
void
SampleAlgorithmBase< TSample >
::SetSample(SamplePointer sample)
{
  m_Sample = sample ;
} 

template< class TSample >
typename SampleAlgorithmBase< TSample >::SamplePointer
SampleAlgorithmBase< TSample >
::GetSample()
{
  return m_Sample ;
} 

template< class TSample >
inline void
SampleAlgorithmBase< TSample >
::GenerateData() 
{
  // subclasses should override this function.
}

template< class TSample >
void
SampleAlgorithmBase< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Sample: " << m_Sample << std::endl;
}
  } // end of namespace Statistics 
} // end of namespace itk

#endif

