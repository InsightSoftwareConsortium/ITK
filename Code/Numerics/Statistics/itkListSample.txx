/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSample.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
  SizeValueType size = GetSize(dimension) ;
  m_SortedDimensionData.clear() ;
  InstanceIdentifier n ;

  for (n = 0 ; n < size ; n++)
    {
      m_SortedDimensionData.push_back(GetMeasurement(dimension, n)) ;
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
