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
#ifndef itkAnisotropicDiffusionImageFilter_hxx
#define itkAnisotropicDiffusionImageFilter_hxx

#include "itkAnisotropicDiffusionImageFilter.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >
::AnisotropicDiffusionImageFilter()
{
  this->SetNumberOfIterations(1);
  m_ConductanceParameter = 1.0;
  m_ConductanceScalingParameter = 1.0;
  m_ConductanceScalingUpdateInterval = 1;
  m_TimeStep = 0.5 / std::pow( 2.0, static_cast< double >( ImageDimension ) );
  m_FixedAverageGradientMagnitude = 1.0;
  m_GradientMagnitudeIsFixed = false;
}

/** Prepare for the iteration process. */
template< typename TInputImage, typename TOutputImage >
void
AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >
::InitializeIteration()
{
  AnisotropicDiffusionFunction< UpdateBufferType > *f =
    dynamic_cast< AnisotropicDiffusionFunction< UpdateBufferType > * >
    ( this->GetDifferenceFunction().GetPointer() );
  if ( !f )
    {
    throw ExceptionObject(__FILE__, __LINE__, "Anisotropic diffusion function is not set.", ITK_LOCATION);
    }

  f->SetConductanceParameter(m_ConductanceParameter);
  f->SetTimeStep(m_TimeStep);

  // Check the timestep for stability
  double minSpacing;
  if ( this->GetUseImageSpacing() )
    {
    minSpacing = this->GetInput()->GetSpacing()[0];
    for ( unsigned int i = 1; i < ImageDimension; i++ )
      {
      if ( this->GetInput()->GetSpacing()[i] < minSpacing )
        {
        minSpacing = this->GetInput()->GetSpacing()[i];
        }
      }
    }
  else
    {
    minSpacing = 1.0;
    }
  if ( m_TimeStep >  ( minSpacing / std::pow(2.0, static_cast< double >( ImageDimension ) + 1) ) )
    {
    //    f->SetTimeStep(1.0 / std::pow(2.0,
    // static_cast<double>(ImageDimension)));
    itkWarningMacro( << "Anisotropic diffusion unstable time step: "
                     << m_TimeStep << std::endl
                     << "Stable time step for this image must be smaller than "
                     << minSpacing / std::pow( 2.0, static_cast< double >( ImageDimension + 1 ) ) );
    }

  if ( m_GradientMagnitudeIsFixed == false )
    {
    if ( ( this->GetElapsedIterations() % m_ConductanceScalingUpdateInterval ) == 0 )
      {
      f->CalculateAverageGradientMagnitudeSquared( this->GetOutput() );
      }
    }
  else
    {
    f->SetAverageGradientMagnitudeSquared(m_FixedAverageGradientMagnitude
                                          *
                                          m_FixedAverageGradientMagnitude);
    }
  f->InitializeIteration();

  if ( this->GetNumberOfIterations() != 0 )
    {
    this->UpdateProgress( ( (float)( this->GetElapsedIterations() ) )
                          / ( (float)( this->GetNumberOfIterations() ) ) );
    }
  else
    {
    this->UpdateProgress(0);
    }
}

template< typename TInputImage, typename TOutputImage >
void
AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf( os, indent.GetNextIndent() );
  os << indent << "TimeStep: " << m_TimeStep << std::endl;
  os << indent << "ConductanceParameter: "
     << m_ConductanceParameter << std::endl;
  os << indent << "ConductanceScalingParameter: "
     << m_ConductanceScalingParameter << std::endl;
  os << indent << "ConductanceScalingUpdateInterval: "
     << m_ConductanceScalingUpdateInterval << std::endl;
  os << indent << "FixedAverageGradientMagnitude: "
     << m_FixedAverageGradientMagnitude << std::endl;
}
} // end namespace itk

#endif
