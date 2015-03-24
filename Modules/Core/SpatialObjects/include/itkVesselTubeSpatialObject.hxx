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
#ifndef itkVesselTubeSpatialObject_hxx
#define itkVesselTubeSpatialObject_hxx


#include "itkVesselTubeSpatialObject.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
VesselTubeSpatialObject< TDimension >
::VesselTubeSpatialObject():Superclass()
{
  this->m_ParentPoint = -1;
  this->SetDimension(TDimension);
  this->SetTypeName("VesselTubeSpatialObject");
}

/** Destructor */
template< unsigned int TDimension >
VesselTubeSpatialObject< TDimension >
::~VesselTubeSpatialObject()
{}

/** Print the object */
template< unsigned int TDimension >
void
VesselTubeSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "VesselTubeSpatialObject(" << this << ")" << std::endl;
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif // end itkVesselTubeSpatialObject_hxx
