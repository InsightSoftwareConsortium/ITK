/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodSampler.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNeighborhoodSampler_txx
#define __itkNeighborhoodSampler_txx

#include "itkNeighborhoodSampler.h"

namespace itk
{
namespace Statistics
{
template< class TSample >
NeighborhoodSampler< TSample >
::NeighborhoodSampler()
{}

template< class TSample >
NeighborhoodSampler< TSample >
::~NeighborhoodSampler()
{}

template< class TSample >
void
NeighborhoodSampler< TSample >
::GenerateData()
{
  const SampleType *inputSample = this->GetInput();

  SubsampleType *outputSubSample =
    static_cast< SubsampleType * >( this->ProcessObject::GetOutput(0) );

  outputSubSample->SetSample(inputSample);
  outputSubSample->Clear();

  const InputRadiusObjectType *radiusObject = this->GetRadiusInput();

  if ( radiusObject == NULL )
    {
    itkExceptionMacro("Radius input is missing");
    }

  // FIXME : What should follow here ?
  //
  // const RadiusType radius = radiusObject->Get();
  //
}

template< class TSample >
void
NeighborhoodSampler< TSample >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  // m_Radius
  os << indent << "Radius: " << this->GetRadiusInput() << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
