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
#ifndef itkPointSetToSpatialObjectDemonsRegistration_hxx
#define itkPointSetToSpatialObjectDemonsRegistration_hxx

#include "itkPointSetToSpatialObjectDemonsRegistration.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TFixedPointSet, typename TMovingSpatialObject >
PointSetToSpatialObjectDemonsRegistration< TFixedPointSet, TMovingSpatialObject >
::PointSetToSpatialObjectDemonsRegistration()
{
  m_FixedPointSet           = ITK_NULLPTR; // has to be provided by the user.
  m_MovingSpatialObject     = ITK_NULLPTR; // has to be provided by the user.
}

/**
 * PrintSelf
 */
template< typename TFixedPointSet, typename TMovingSpatialObject >
void
PointSetToSpatialObjectDemonsRegistration< TFixedPointSet, TMovingSpatialObject >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Fixed PointSet: " << m_FixedPointSet.GetPointer() << std::endl;
  os << indent << "Moving SpatialObject: " << m_MovingSpatialObject.GetPointer() << std::endl;
}

} // end namespace itk
#endif
