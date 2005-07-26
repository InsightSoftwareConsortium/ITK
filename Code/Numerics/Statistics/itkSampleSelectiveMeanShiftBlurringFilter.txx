/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleSelectiveMeanShiftBlurringFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  
  os << indent << "Component selections:  [";
  for( unsigned int i=0; i< MeasurementVectorTraits::GetLength( 
                                            m_ComponentSelections ); i++ )
    {
    os << m_ComponentSelections[i] << " " << std::endl;
    }
  os << "]" << std::endl;
}

template< class TSample >
inline void
SampleSelectiveMeanShiftBlurringFilter< TSample >
::SetComponentSelections(ComponentSelectionsType selections)
{
  if( this->GetMeasurementVectorSize() )
    {
    MeasurementVectorTraits::Assert( selections, this->GetMeasurementVectorSize(),
    "Size of measurement vectors in the sample must be the same as the size of the component selections array.");
    }
  
  if ( m_ComponentSelections != selections )
    {
    m_ComponentSelections = selections ;
    this->Modified();
    }
}

template< class TSample >
inline void
SampleSelectiveMeanShiftBlurringFilter< TSample >
::GenerateData() 
{
  // Assert at run time that the given mean has the same length as 
  // measurement vectors in the sample and that the size is non-zero.
  if( MeasurementVectorTraits::GetLength(m_ComponentSelections ))
    {
    MeasurementVectorTraits::SetLength( m_ComponentSelections, 
                              this->GetMeasurementVectorSize() );
    for( unsigned int i=0; i< this->GetMeasurementVectorSize(); i++ )
      {
      m_ComponentSelections[i] = true;
      }
    }
  else if( !(this->GetMeasurementVectorSize() ) || 
      ( m_ComponentSelections.size() != this->GetMeasurementVectorSize() ) )
    {
    itkExceptionMacro( << "Size of measurement vectors in the sample must be "
        << "the same as the size of the component selections array." );
    }
  
  
  MeasurementVectorType queryPoint ;
  MeasurementVectorType modePoint ;
  MeasurementVectorType finalPoint;
  MeasurementVectorTraits::SetLength( finalPoint, this->GetMeasurementVectorSize() );

  typename Superclass::InputSampleType::ConstIterator iter = this->GetInputSample()->Begin() ;
  typename Superclass::InputSampleType::ConstIterator end = this->GetInputSample()->End() ;

  OutputType* output = this->GetOutput() ;
  output->Clear() ;
  MeanShiftModeSeekerType* modeSeeker = this->GetMeanShiftModeSeeker() ;
  while ( iter != end )
    {
    queryPoint = iter.GetMeasurementVector() ;
    modePoint = modeSeeker->Evolve( queryPoint ) ;
    for ( unsigned int i = 0 ; i < this->GetMeasurementVectorSize() ; ++i )
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

