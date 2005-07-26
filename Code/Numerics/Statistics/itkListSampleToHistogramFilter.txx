/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSampleToHistogramFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSampleToHistogramFilter_txx
#define __itkListSampleToHistogramFilter_txx

#include "itkListSampleToHistogramFilter.h"

namespace itk{
namespace Statistics{

template< class TListSample, class THistogram >
ListSampleToHistogramFilter< TListSample, THistogram >
::ListSampleToHistogramFilter()
{
}

template< class TListSample, class THistogram >
void
ListSampleToHistogramFilter< TListSample, THistogram >
::Run()
{
  typename TListSample::ConstIterator iter = m_List->Begin() ;
  typename TListSample::ConstIterator last = m_List->End() ;
  typename THistogram::IndexType index ;
  typename TListSample::MeasurementVectorType lvector ;
  typename THistogram::MeasurementVectorType hvector ;

  // Sanity check to see if lengths of the vector passed in and the 
  // histogram's MV lengths are the same
  if( m_List->GetMeasurementVectorSize() != THistogram::MeasurementVectorSize )
    {
    itkExceptionMacro(<< "List sample and histogram have different measurement "
        << "vector lengths: " << m_List->GetMeasurementVectorSize() << ", " <<
        THistogram::MeasurementVectorSize);
    }

  unsigned int i ;
  while (iter != last)
    {
    lvector = iter.GetMeasurementVector() ;
    for ( i = 0 ; i < THistogram::MeasurementVectorSize ; i++)
      {
      hvector[i] = 
        (typename THistogram::MeasurementType) lvector[i] ;
      }

    m_Histogram->GetIndex(hvector,index);
    if (!m_Histogram->IsIndexOutOfBounds(index))
      {
      // if the measurement vector is out of bound then
      // the GetIndex method returns index with the sizes of each dimension
      // and doesn't increase the frequency
      //          id = m_Histogram->GetInstanceIdentifier(index) ;
      m_Histogram->IncreaseFrequency(index, 1) ;
      }
    ++iter ;
    }
}

} // end of namespace Statistics 
} // end of namespace itk 

#endif


