/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSampleToHistogramGenerator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSampleToHistogramGenerator_txx
#define __itkListSampleToHistogramGenerator_txx

namespace itk{
namespace Statistics{

template< class TListSample, 
          class THistogramMeasurement, 
          class TFrequencyContainer >
ListSampleToHistogramGenerator< TListSample, 
                                THistogramMeasurement, 
                                TFrequencyContainer>
::ListSampleToHistogramGenerator()
{
  m_Sizes.Fill(0) ;
  m_Histogram = HistogramType::New() ;
  m_MarginalScale = 100 ;
  m_HistogramMin.Fill(0);
  m_HistogramMax.Fill(0);
  m_AutoMinMax = true;
}  


template< class TListSample, 
          class THistogramMeasurement, 
          class TFrequencyContainer >
void
ListSampleToHistogramGenerator< TListSample, 
                                THistogramMeasurement, 
                                TFrequencyContainer>
::GenerateData()
{
  typename TListSample::MeasurementVectorType lower;
  typename TListSample::MeasurementVectorType upper;

  typename HistogramType::MeasurementVectorType h_upper  = m_HistogramMax;
  typename HistogramType::MeasurementVectorType h_lower = m_HistogramMin;

  if(m_AutoMinMax)
    {
    FindSampleBound(m_List, m_List->Begin(),
                    m_List->End(), lower, upper) ;
    
    float margin ;

    for ( unsigned int i = 0 ; i < TListSample::MeasurementVectorSize ; i++ )
      {
      // integer and char type has usually 0 for the epsilon
      if ( NumericTraits< THistogramMeasurement >::epsilon() > 
           NumericTraits< THistogramMeasurement >::Zero )
        {
        margin = 
          ( (THistogramMeasurement)(upper[i] - lower[i]) / 
            (THistogramMeasurement) m_Sizes[i] ) / 
          (THistogramMeasurement) m_MarginalScale ;
        h_upper[i] = (THistogramMeasurement) (upper[i] + 
          margin) ;
        }
      else
        {
        h_upper[i] = ((THistogramMeasurement) upper[i]) + 
          NumericTraits< THistogramMeasurement >::One ;
        }
      h_lower[i] = ( THistogramMeasurement) lower[i] ;
      }
    }

  // initialize the Histogram object using the sizes and
  // the upper and lower bound from the FindSampleBound function
  m_Histogram->Initialize(m_Sizes, h_lower, h_upper) ;

  typename TListSample::ConstIterator iter = m_List->Begin() ;
  typename TListSample::ConstIterator last = m_List->End() ;
  typename HistogramType::IndexType index ;
  typename TListSample::MeasurementVectorType lvector ;
  typename HistogramType::MeasurementVectorType hvector ;
  unsigned int i;
  while (iter != last)
    {
    lvector = iter.GetMeasurementVector() ;
    for ( i = 0 ; i < HistogramType::MeasurementVectorSize ; i++)
      {
      hvector[i] = (THistogramMeasurement) lvector[i] ;
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


