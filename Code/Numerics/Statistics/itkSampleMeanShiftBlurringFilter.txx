/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleMeanShiftBlurringFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSampleMeanShiftBlurringFilter_txx
#define __itkSampleMeanShiftBlurringFilter_txx

namespace itk{ 
namespace Statistics{

template< class TSample >
SampleMeanShiftBlurringFilter< TSample >
::SampleMeanShiftBlurringFilter()
{
  m_Output = OutputType::New() ;
  m_ModeSeeker = 0 ;
}

template< class TSample >
SampleMeanShiftBlurringFilter< TSample >
::~SampleMeanShiftBlurringFilter()
{
}

template< class TSample >
void
SampleMeanShiftBlurringFilter< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Output: " << m_Output << std::endl;
}

template< class TSample >
void
SampleMeanShiftBlurringFilter< TSample >
::SetMeanShiftModeSeeker(MeanShiftModeSeekerType* seeker)
{
  if ( m_ModeSeeker != seeker )
    {
    m_ModeSeeker = seeker ;
    this->Modified() ;
    }
}

template< class TSample >
typename SampleMeanShiftBlurringFilter< TSample >::OutputType*
SampleMeanShiftBlurringFilter< TSample >
::GetOutput()
{
  return m_Output.GetPointer() ;
} 

template< class TSample >
inline void
SampleMeanShiftBlurringFilter< TSample >
::GenerateData() 
{
  typename InputSampleType::Iterator iter = this->GetInputSample()->Begin() ;
  typename InputSampleType::Iterator end = this->GetInputSample()->End() ;

  m_Output->Clear() ;
  MeasurementVectorType finalPoint ;
  while ( iter != end )
    {
    finalPoint = m_ModeSeeker->Evolve( iter.GetMeasurementVector() ) ;
    m_Output->PushBack( finalPoint  ) ;
    ++iter ;
    }
}

} // end of namespace Statistics 
} // end of namespace itk

#endif

