/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCacheableScalarFunction.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkCacheableScalarFunction.h"

namespace itk {
CacheableScalarFunction
::CacheableScalarFunction()
{
  m_CacheAvailable = false ;
}

void 
CacheableScalarFunction
::CreateCache(double lowerBound, double upperBound, long sampleSize) 
{
  m_NumberOfSamples = sampleSize ;
  m_CacheLowerBound = lowerBound ;
  m_CacheUpperBound = upperBound ;

  long i ;
  MeasureType d ;
  
  m_CacheTable = MeasureArrayType(m_NumberOfSamples) ;
  
  m_TableInc = 
    static_cast<MeasureType>( (m_CacheUpperBound - m_CacheLowerBound) / 
                              double(m_NumberOfSamples - 1) );

  d = static_cast<MeasureType>( m_CacheLowerBound );
  for (i = 0; i < m_NumberOfSamples; i++) 
    {
    m_CacheTable[i] = Evaluate(d);
    d += m_TableInc;
    }

  m_CacheAvailable = true ;
}

} // end of namespace itk
 
