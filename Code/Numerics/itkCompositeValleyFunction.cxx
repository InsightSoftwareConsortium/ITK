/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompositeValleyFunction.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkCompositeValleyFunction.h"

namespace itk {
CompositeValleyFunction
::CompositeValleyFunction( const MeasureArrayType & classMeans, 
                           const MeasureArrayType & classSigmas )
{
  unsigned int length = classMeans.size() ;

  if (length != classSigmas.size())
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("Arrays of Means and Sigmas have not the same length");
    throw ex;
    }

  if( length == 0 )
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("arrays of Means is empty");
    throw ex;
    }
  
  for (unsigned int i = 0 ; i < length ; i++) 
    {
    this->AddNewClass(classMeans[i], classSigmas[i]) ;
    }

  this->Initialize() ;
}

void CompositeValleyFunction
::Initialize() 
{
  long i,low,high;
  
  // build table
  // when using valley-func then the table values run from 
  // lowest_my - 10 * sigma to highest_my + 10 * sigma
  
  low = 0; high = 0;
  
  int noOfClasses = static_cast<int>( m_Targets.size() );

  for (i = 0 ; i < noOfClasses ; i++) 
    {
      if (m_Targets[i].GetMean() > m_Targets[high].GetMean()) 
        high = i;
      if (m_Targets[i].GetMean() < m_Targets[low].GetMean()) 
        low =i;
    }
  
  m_LowerBound = m_Targets[low].GetMean() - 
    9.0 * m_Targets[low].GetSigma() ;
  m_UpperBound = m_Targets[high].GetMean() + 
    9.0 * m_Targets[high].GetSigma() ;

  CreateCache(m_LowerBound, m_UpperBound, 1000000) ;
}

} // end of namespace itk
