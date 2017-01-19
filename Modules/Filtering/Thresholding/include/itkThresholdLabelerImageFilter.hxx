/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkThresholdLabelerImageFilter_hxx
#define itkThresholdLabelerImageFilter_hxx

#include "itkThresholdLabelerImageFilter.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
ThresholdLabelerImageFilter< TInputImage, TOutputImage >
::ThresholdLabelerImageFilter() :
  m_LabelOffset( NumericTraits< OutputPixelType >::ZeroValue() )
{
  m_Thresholds.clear();
  m_RealThresholds.clear();
}

template< typename TInputImage, typename TOutputImage >
void
ThresholdLabelerImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  unsigned int size = static_cast<unsigned int>( m_Thresholds.size() );

  for ( unsigned int i = 0; i < size - 1; i++ )
    {
    if ( m_Thresholds[i] > m_Thresholds[i + 1] )
      {
      itkExceptionMacro(<< "Thresholds must be sorted.");
      }
    }

  // Set up the functor values
  this->GetFunctor().SetThresholds(m_RealThresholds);
  this->GetFunctor().SetLabelOffset(m_LabelOffset);
}

template< typename TInputImage, typename TOutputImage >
void
ThresholdLabelerImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Thresholds: ";
  SizeValueType thresholdsSize = static_cast<SizeValueType>( m_Thresholds.size() );
  for ( SizeValueType j = 0; j < thresholdsSize; j++ )
    {
    os << m_Thresholds[j] << " ";
    }
  os << std::endl;

  os << indent << "Real Thresholds: ";
  SizeValueType realThresholdsSize = static_cast<SizeValueType>( m_RealThresholds.size() );
  for ( SizeValueType i = 0; i < realThresholdsSize; i++ )
    {
    os << m_RealThresholds[i] << " ";
    }
  os << std::endl;

  os << indent << "LabelOffset: " << m_LabelOffset << std::endl;
}
} // end namespace itk

#endif
