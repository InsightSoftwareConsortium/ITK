/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHypersphereKernelMeanShiftModeSeeker.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHypersphereKernelMeanShiftModeSeeker_txx
#define __itkHypersphereKernelMeanShiftModeSeeker_txx

#include "vnl/vnl_math.h"

namespace itk{ 
namespace Statistics{

template< class TSample >
HypersphereKernelMeanShiftModeSeeker< TSample >
::HypersphereKernelMeanShiftModeSeeker()
{
  m_SearchRadius = 0.0 ;
}

template< class TSample >
HypersphereKernelMeanShiftModeSeeker< TSample >
::~HypersphereKernelMeanShiftModeSeeker()
{
}

template< class TSample >
void
HypersphereKernelMeanShiftModeSeeker< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Search radius: " << m_SearchRadius << std::endl ;
}

template< class TSample >
void
HypersphereKernelMeanShiftModeSeeker< TSample >
::SetSearchRadius(double radius)
{
  if ( m_SearchRadius != radius )
    {
    m_SearchRadius = radius ;
    this->Modified() ;
    }
}

template< class TSample >
inline void
HypersphereKernelMeanShiftModeSeeker< TSample >
::ComputeMode(MeasurementVectorType queryPoint, 
              MeasurementVectorType& newPoint)
{
  TSample* inputSample = this->GetInputSample() ;
  float frequencySum ;

  SearchResultVectorType result ;
  inputSample->Search( queryPoint, m_SearchRadius, result ) ;
  
  frequencySum = 0.0f ;
  m_TempVectorSum.Fill(NumericTraits< NumericTraits< MeasurementType >::AccumulateType >::Zero) ;
  
  typename SearchResultVectorType::iterator iter =  result.begin() ;
  while ( iter != result.end() )
    {
    m_TempVector = inputSample->GetMeasurementVector(*iter) ;
    for ( unsigned int i = 0 ; i < MeasurementVectorSize ; ++i )
      {
      m_TempVectorSum[i] += m_TempVector[i] ;
      }
    frequencySum += inputSample->GetFrequency(*iter) ;
    ++iter ;
    }
  
  for ( unsigned int i = 0 ; i < MeasurementVectorSize ; ++i )
    {
    newPoint[i] =  m_TempVectorSum[i] / frequencySum ;
    }
}


} // end of namespace Statistics 
} // end of namespace itk

#endif

