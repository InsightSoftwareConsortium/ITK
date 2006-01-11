/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnisotropicDiffusionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  this->SetNumberOfIterations(1);
  m_ConductanceParameter = 1.0;
  m_ConductanceScalingParameter = 1.0;
  m_ConductanceScalingUpdateInterval = 1;
  m_TimeStep = 0.5 / pow(2.0, static_cast<double>(ImageDimension));
  m_FixedAverageGradientMagnitude = 1.0;
  m_GradientMagnitudeIsFixed = false;
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
    {
    throw ExceptionObject(__FILE__, __LINE__, "Anisotropic diffusion function is not set.", ITK_LOCATION);
    }
  
  f->SetConductanceParameter(m_ConductanceParameter);
  f->SetTimeStep(m_TimeStep);
  
  // Check the timestep for stability
  double minSpacing;
  if (this->GetUseImageSpacing())
    {
    minSpacing = this->GetInput()->GetSpacing()[0];
    for (unsigned int i = 1; i < ImageDimension; i++)
      {
      if (this->GetInput()->GetSpacing()[i] < minSpacing)
        {
        minSpacing = this->GetInput()->GetSpacing()[i];
        }
      }
    }
  else
    {
    minSpacing = 1.0;
    }
  if ( m_TimeStep >  (minSpacing / pow(2.0, static_cast<double>(ImageDimension) + 1))  )
    {
    //    f->SetTimeStep(1.0 / pow(2.0, static_cast<double>(ImageDimension))); 
    itkWarningMacro(<< std::endl << "Anisotropic diffusion unstable time step: " << m_TimeStep << std::endl << "Minimum stable time step for this image is " << minSpacing / pow(2.0, static_cast<double>(ImageDimension+1)));
    }
  
  if (m_GradientMagnitudeIsFixed == false)
    {
    if ((this->GetElapsedIterations() % m_ConductanceScalingUpdateInterval)==0 )
      {
      f->CalculateAverageGradientMagnitudeSquared(this->GetOutput());
      }
    }
  else
    {
    f->SetAverageGradientMagnitudeSquared(m_FixedAverageGradientMagnitude 
                                          *
                                          m_FixedAverageGradientMagnitude);
    }
  f->InitializeIteration();
  
  if (this->GetNumberOfIterations() != 0)
    {
    this->UpdateProgress(((float)(this->GetElapsedIterations()))
                         /((float)(this->GetNumberOfIterations())));
    }
  else
    {
    this->UpdateProgress(0);
    }
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
  os << indent << "ConductanceScalingUpdateInterval: "
     << m_ConductanceScalingUpdateInterval << std::endl;
  os << indent << "FixedAverageGradientMagnitude: "
     << m_FixedAverageGradientMagnitude << std::endl;
}

} // end namespace itk

#endif
