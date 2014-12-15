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
#ifndef itkNeighborhoodSampler_hxx
#define itkNeighborhoodSampler_hxx

#include "itkNeighborhoodSampler.h"

namespace itk
{
namespace Statistics
{
template< typename TSample >
NeighborhoodSampler< TSample >
::NeighborhoodSampler()
{}

template< typename TSample >
NeighborhoodSampler< TSample >
::~NeighborhoodSampler()
{}

template< typename TSample >
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

  if ( radiusObject == ITK_NULLPTR )
    {
    itkExceptionMacro("Radius input is missing");
    }

  // FIXME : What should follow here ?
  //
  // const RadiusType radius = radiusObject->Get();
  //
}

template< typename TSample >
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
