/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleSelectiveMeanShiftBlurringFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSampleSelectiveMeanShiftBlurringFilter_txx
#define __itkSampleSelectiveMeanShiftBlurringFilter_txx

namespace itk{ 
namespace Statistics{

template< class TSample >
SampleSelectiveMeanShiftBlurringFilter< TSample >
::SampleSelectiveMeanShiftBlurringFilter()
{
  m_ComponentSelections.Fill( true ) ;
}

template< class TSample >
SampleSelectiveMeanShiftBlurringFilter< TSample >
::~SampleSelectiveMeanShiftBlurringFilter()
{
}

template< class TSample >
void
SampleSelectiveMeanShiftBlurringFilter< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Component selections: " 
     << m_ComponentSelections << std::endl ;
}

template< class TSample >
inline void
SampleSelectiveMeanShiftBlurringFilter< TSample >
::SetComponentSelections(ComponentSelectionsType selections)
{
  if ( m_ComponentSelections != selections )
    {
    m_ComponentSelections = selections ;
    }
}

template< class TSample >
inline void
SampleSelectiveMeanShiftBlurringFilter< TSample >
::GenerateData() 
{
  MeasurementVectorType queryPoint ;
  MeasurementVectorType modePoint ;
  MeasurementVectorType finalPoint ;

  typename InputSampleType::Iterator iter = this->GetInputSample()->Begin() ;
  typename InputSampleType::Iterator end = this->GetInputSample()->End() ;

  OutputType* output = this->GetOutput() ;
  output->Clear() ;
  MeanShiftModeSeekerType* modeSeeker = this->GetMeanShiftModeSeeker() ;
  while ( iter != end )
    {
    queryPoint = iter.GetMeasurementVector() ;
    modePoint = modeSeeker->Evolve( queryPoint ) ;
    for ( unsigned int i = 0 ; i < MeasurementVectorSize ; ++i )
      {
      if ( m_ComponentSelections[i] )
        {
        finalPoint[i] = modePoint[i] ;
        }
      else
        {
        finalPoint[i] = queryPoint[i] ; 
        }
      }
    output->PushBack( finalPoint  ) ;
    ++iter ;
    }
}

} // end of namespace Statistics 
} // end of namespace itk

#endif

