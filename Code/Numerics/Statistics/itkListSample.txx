/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSample.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSample_txx
#define __itkListSample_txx

#include "itkMacro.h"
#include "itkIndex.h"
#include "itkPoint.h"
#include "itkSize.h"

#include <vector>

namespace itk{ 
  namespace Statistics{

template< class TMeasurement, unsigned int VMeasurementVectorSize >
ListSample< TMeasurement, VMeasurementVectorSize >
::ListSample()
{
  // all measurement vectors are sored
  SetSortedFlag(false) ;
  // supports GetFrequency method
  SetSupportingFrequencyFlag(false) ;
  // no dupliates
  SetAllowingDuplicatesFlag(true) ;

  m_SortedDimension = MeasurementVectorSize ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize >
void 
ListSample< TMeasurement, VMeasurementVectorSize >
::GenerateSortedDimensionData(unsigned int dimension)
{
  unsigned long size = this->Size() ;
  m_SortedDimensionData.clear() ;
  InstanceIdentifier n ;

  for (n = 0 ; n < size ; n++)
    {
      m_SortedDimensionData.push_back(GetMeasurement(n, dimension)) ;
    }
  
  std::sort(m_SortedDimensionData.begin(), m_SortedDimensionData.end()) ;
  m_SortedDimension = dimension ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize >
double 
ListSample< TMeasurement, VMeasurementVectorSize >
::Quantile(unsigned int dimension, double p, 
           bool forceSortedDimensionDataGeneration)
{
  if (forceSortedDimensionDataGeneration)
    {
      GenerateSortedDimensionData(dimension) ;
    }

  if (m_SortedDimension != dimension)
    {
      GenerateSortedDimensionData(dimension) ;
    }
  
  double t = p * double(GetTotalFrequency(dimension)) ;
  int i = (int) t ;
  return double(i + 1 - t) * 
    double(m_SortedDimensionData[i]) + 
    double(t - i) * 
    double(m_SortedDimensionData[i + 1]) ;
}

template< class TMeasurement, unsigned int VMeasurementVectorSize >
void 
ListSample< TMeasurement, VMeasurementVectorSize >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "SortedDimensionData: " << &m_SortedDimensionData 
     << std::endl;
  os << indent << "SortedDimension: " << m_SortedDimension << std::endl;
}
  } // end of namespace Statistics
} // end of namespace itk 

#endif
