/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnisotropicDiffusionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAnisotropicDiffusionImageFilter_txx_
#define __itkAnisotropicDiffusionImageFilter_txx_

#include "itkAnisotropicDiffusionImageFilter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
::AnisotropicDiffusionImageFilter()
{
  m_NumberOfIterations = 0;
  m_ConductanceParameter = 1.0;
  m_ConductanceScalingParameter = 1.0;
  m_ConductanceScalingUpdateInterval = 0;
  m_TimeStep = 0.5 / pow(2.0, static_cast<double>(ImageDimension));
  m_FixedAverageGradientMagnitude = 0.0;
  m_ConductanceScalingUpdateInterval = 1;
  m_GradientMagnitudeIsFixed = false;
}

/** Supplies the halting criteria for this class of filters.  The
  * algorithm will stop after a user-specified number of iterations. */
template <class TInputImage, class TOutputImage>
bool
AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
::Halt()
{
  if (this->GetElapsedIterations() == m_NumberOfIterations) return true;
  else return false;
}

/** Prepare for the iteration process. */
template <class TInputImage, class TOutputImage>
void
AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
::InitializeIteration()
{
  AnisotropicDiffusionFunction<UpdateBufferType> *f = 
    dynamic_cast<AnisotropicDiffusionFunction<UpdateBufferType> *>
    (this->GetDifferenceFunction().GetPointer());
  if (! f)
    {  throw ExceptionObject(__FILE__, __LINE__);    }
  
  f->SetConductanceParameter(m_ConductanceParameter);
  f->SetTimeStep(m_TimeStep);
  
  if ( m_TimeStep >  1.0 / pow(2.0, static_cast<double>(ImageDimension))  )
    {
    //    f->SetTimeStep(1.0 / pow(2.0, static_cast<double>(ImageDimension))); 
    itkWarningMacro(<< "Anisotropic diffusion has attempted to use a time step which may introduce instability into the solution." );
    }
  
  if (m_GradientMagnitudeIsFixed == false && (this->GetElapsedIterations() % m_ConductanceScalingUpdateInterval)==0 )
    {
    f->CalculateAverageGradientMagnitudeSquared(this->GetOutput());
    }
  else
    {
    f->SetAverageGradientMagnitudeSquared(m_FixedAverageGradientMagnitude 
                                          *
                                          m_FixedAverageGradientMagnitude);
    }
  f->InitializeIteration();

  if (m_NumberOfIterations != 0)
    this->UpdateProgress(((float)(this->GetElapsedIterations()))
                         /((float)(m_NumberOfIterations)));
  else this->UpdateProgress(0);
}

template <class TInputImage, class TOutputImage>
void
AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent.GetNextIndent());
  os << indent << "TimeStep: " << m_TimeStep << std::endl;
  os << indent << "ConductanceParameter: "
     << m_ConductanceParameter << std::endl;
  os << indent << "ConductanceScalingParameter: "
     << m_ConductanceScalingParameter << std::endl;
  os << indent << "NumberOfIterations: " << m_NumberOfIterations
     << std::endl;
  os << indent << "ConductanceScalingUpdateInterval: "
     << m_ConductanceScalingUpdateInterval << std::endl;
  os << indent << "FixedAverageGradientMagnitude: "
     << m_FixedAverageGradientMagnitude << std::endl;
}

} // end namespace itk

#endif
